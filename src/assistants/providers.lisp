(in-package #:xeon.assistants)

(defparameter *agent-instructions*
  "You are a coding agent inside the Xeon Lisp editor. Use Xeon's tools to work with the editor itself: editor_state and editor_read show open documents, including unsaved changes; editor_open, editor_select, editor_replace, editor_write, editor_new and editor_save work with documents in the editor, where the user sees and can undo your edits. Use runtime_state, repl_start, repl_evaluate, the debugger_* and inspector_* tools for the connected Lisp REPL and debugger. Prefer editing open documents through the editor tools. Request approval for actions when required; do not bypass the host."
  "What Agent mode tells the provider about Xeon's host tools.")

(defun provider-arguments (session)
  (ecase (session-provider session)
    (:codex '("app-server"))
    (:claude
     (append '("-p" "--input-format" "stream-json" "--output-format" "stream-json"
               "--verbose" "--include-partial-messages" "--permission-mode" "manual"
               ;; Without a prompt tool, -p denies anything that needs permission
               ;; outright; stdio sends can_use_tool control requests to Xeon instead.
               "--permission-prompt-tool" "stdio")
       (if (eq (session-mode session) :chat) '("--tools" "") (list "--append-system-prompt" *agent-instructions*))
       (when (session-model session) (list "--model" (session-model session)))
       (when (session-effort session) (list "--effort" (session-effort session)))
       (when (session-conversation session) (list "--resume" (session-conversation session)))
       (list "--mcp-config" (json-string (jobj "mcpServers" (jobj "xeon" (jobj "type" "sdk" "name" "xeon")))))
       '("--allowedTools" "mcp__xeon__*")))))

(defun normalize-models (session models)
  (let ((codex (eq (session-provider session) :codex)))
    (setf (session-models session)
      (loop for model in (jlist models) unless (jtrue-p (jget model "hidden")) collect
        (list :id (jget model (if codex "id" "value"))
              :default (if codex (jtrue-p (jget model "isDefault")) (equal (jget model "value") "default"))
              :label (or (jget model "displayName") (jget model "value") (jget model "id"))
              :efforts (if codex
                           (mapcar (lambda (level) (jget level "reasoningEffort")) (jlist (jget model "supportedReasoningEfforts")))
                           (when (jtrue-p (jget model "supportsEffort")) (jlist (jget model "supportedEffortLevels")))))))
    (session-update session)))

(defun codex-thread-params (session)
  (jobj "cwd" (uiop:native-namestring (session-directory session))
        "model" (or (session-model session) :null)
        "approvalPolicy" "untrusted"
        "sandbox" (if (eq (session-mode session) :chat) "read-only" "workspace-write")
        "config" (if (eq (session-mode session) :chat)
                     (jobj "features" (jobj "shell_tool" :false "apply_patch_freeform" :false)) (jobj))
        "baseInstructions" (if (eq (session-mode session) :chat)
          "You are in Chat mode. Explain and discuss. Do not edit files or run commands. Runtime read tools may be used."
          *agent-instructions*)
        "dynamicTools" (if (session-conversation session) :omit (coerce (runtime-tools) 'vector))
        "threadId" (or (session-conversation session) :omit)))
(defun initialize-provider (session)
  (ecase (session-provider session)
    (:codex
     (request session "initialize" (jobj "clientInfo" (jobj "name" "xeon" "version" "0.1.0")
                                          "capabilities" (jobj "experimentalApi" t))
       (lambda (result) (declare (ignore result))
         (send-json session (jobj "method" "initialized"))
         (request session "account/rateLimits/read" (jobj) (lambda (result) (update-codex-limits session result)) #'identity)
         (request session "model/list" (jobj "limit" 100) (lambda (models) (normalize-models session (jget models "data"))))
         (request session (if (session-conversation session) "thread/resume" "thread/start") (codex-thread-params session)
           (lambda (thread)
             (setf (session-conversation session) (jget thread "thread" "id"))
             (unless (session-conversation session) (error "Codex returned no conversation id."))
             (session-update session :status :ready))))))
    (:claude
     (request session "initialize" (jobj "sdkMcpServers" #("xeon"))
       (lambda (result) (normalize-models session (jget result "models")) (session-update session :status :ready))))))

(defun provider-image-input (provider text images)
  (coerce (append (list (jobj "type" "text" "text" text))
            (mapcar (lambda (url)
                      (if (eq provider :codex) (jobj "type" "image" "url" url)
                          (let ((comma (position #\, url)) (semi (position #\; url)))
                            (jobj "type" "image" "source"
                              (jobj "type" "base64" "media_type" (subseq url 5 semi)
                                    "data" (subseq url (1+ comma))))))) images)) 'vector))

(defun provider-send (session text &optional images)
  (ecase (session-provider session)
    (:codex
     (request session "turn/start"
       (jobj "threadId" (session-conversation session) "input" (provider-image-input :codex text images)
             "model" (or (session-model session) :null) "effort" (or (session-effort session) :null))
       (lambda (result) (when (jget result "turn" "id")
                         (setf (session-turn session) (jget result "turn" "id"))
                         (flush-codex-steering session)))
       (lambda (error)
         (let ((pending (session-steering session)))
           (setf (session-turn session) nil (session-steering session) nil)
           (session-update session :text
             (format nil "~%[Send failed: ~A~A]~%" error
                     (if pending "; pending steering was not sent. Please resend your guidance" ""))
             :status :ready)))))
    (:claude (send-json session (jobj "type" "user" "message" (jobj "role" "user" "content" (if images (provider-image-input :claude text images) text)))))))

(defun flush-codex-steering (session)
  ;; The user can submit before turn/start has returned its turn id.
  (when (and (session-turn session) (session-steering session))
    (let ((text (format nil "~{~A~^~%~%~}" (session-steering session))))
      (setf (session-steering session) nil)
      (request session "turn/steer"
        (jobj "threadId" (session-conversation session)
              "expectedTurnId" (session-turn session)
              "input" (vector (jobj "type" "text" "text" text)))
        #'identity
        (lambda (error)
          ;; A completion may race with steering. Never silently retry against
          ;; another turn or mark a still-running agent ready.
          (session-update session :text
            (format nil "~%[Steering not delivered: ~A. Please resend: ~A]~%" error text)))))))

(defun provider-steer (session text)
  (let ((already-pending (session-steering session)))
    (setf (session-steering session) (append already-pending (list text)))
    (ecase (session-provider session)
      (:codex (flush-codex-steering session))
      (:claude
       ;; stream-json queues user messages rather than steering the active
       ;; response. Interrupt once, then send after its terminal result.
       (unless already-pending
         (request session "interrupt" (jobj) #'identity
           (lambda (error)
             (setf (session-steering session) nil)
             (session-update session :text
               (format nil "~%[Steering interrupt failed: ~A. Please resend your guidance.]~%" error)))))))))
(defun provider-interrupt (session)
  (when (member (session-status session) '(:busy :waiting))
    (ecase (session-provider session)
      (:codex
       (when (session-turn session)
         (request session "turn/interrupt" (jobj "threadId" (session-conversation session) "turnId" (session-turn session)) #'identity)))
      (:claude (request session "interrupt" (jobj) #'identity)))))

(defun add-prompt (session id method params &key tool-callback)
  (let ((prompt (normalize-prompt session id method params)))
    (when tool-callback (setf (getf prompt :callback) tool-callback))
    (sb-thread:with-mutex ((session-lock session))
      (setf (session-requests session) (append (session-requests session) (list prompt))))
    (session-update session :status :waiting)
    (maybe-auto-approve session prompt)))
(defun remove-prompt (session id)
  (let ((found nil))
    (sb-thread:with-mutex ((session-lock session))
      (setf found (find id (session-requests session) :key (lambda (p) (getf p :id)) :test #'equal))
      (when found
        (setf (session-requests session) (remove id (session-requests session) :key (lambda (p) (getf p :id)) :test #'equal))))
    (when found (session-update session :status (if (session-requests session) :waiting :busy)))))
(defun turn-finished (session text)
  (sb-thread:with-mutex ((session-lock session)) (setf (session-requests session) nil))
  (setf (session-turn session) nil)
  (session-update session :text (or text (format nil "~%")) :status :ready)
  (when (session-steering session)
    (let ((guidance (format nil "~{~A~^~%~%~}" (session-steering session))))
      (setf (session-steering session) nil)
      (begin-message-turn session guidance))))
(defun record-item-text (session id text &key delta)
  (let ((previous (gethash id (session-streamed session))))
    (cond (delta (setf (gethash id (session-streamed session)) (concatenate 'string (or previous "") text))
                 (session-update session :text text :role :assistant :message-key (list (session-generation session) (session-message-epoch session) id)))
          ((null previous) (setf (gethash id (session-streamed session)) text)
           (session-update session :text text :role :assistant :message-key (list (session-generation session) (session-message-epoch session) id)))
          ((and (<= (length previous) (length text)) (string= previous text :end2 (length previous)))
           (setf (gethash id (session-streamed session)) text)
           (session-update session :text (subseq text (length previous)) :role :assistant
                           :message-key (list (session-generation session) (session-message-epoch session) id))))))

(defun codex-message (session object)
  (let ((method (jget object "method")) (params (jget object "params")) (id (jget object "id")))
    (cond
      ((and method id)
       (if (equal method "item/tool/call")
           (dispatch-runtime-tool session (jget params "tool") (jget params "arguments")
             (lambda (text error-p)
               (send-json session (jobj "id" id "result" (jobj "success" (if error-p :false t)
                 "contentItems" (vector (jobj "type" "inputText" "text" text)))))))
           (add-prompt session id method params)))
      (id (finish-request session id (jget object "result") (jget object "error" "message")))
      ((equal method "account/rateLimits/updated") (update-codex-limits session params))
      ((equal method "turn/started")
       (setf (session-turn session) (jget params "turn" "id"))
       (flush-codex-steering session))
      ((equal method "item/agentMessage/delta")
       (record-item-text session (jget params "itemId") (or (jget params "delta") "") :delta t))
      ((equal method "item/started")
       (let ((item (jget params "item")))
         (when (> (hash-table-count (session-items session)) 256) (clrhash (session-items session)))
         (when (jget item "id") (setf (gethash (jget item "id") (session-items session)) item))))
      ((equal method "item/completed")
       (let ((item (jget params "item")))
         (cond ((equal (jget item "type") "agentMessage") (record-item-text session (jget item "id") (or (jget item "text") "")))
               ((member (jget item "type") '("commandExecution" "fileChange" "mcpToolCall") :test #'equal)
                (session-update session :text (format nil "~%[~A · ~A]~%~A~%" (jget item "type") (jget item "status")
                  (or (jget item "aggregatedOutput") (jget item "changes") "")))))))
      ((equal method "thread/tokenUsage/updated")
       (let ((usage (jget params "tokenUsage")))
         (setf (session-usage session) (list :input (jget usage "total" "inputTokens") :output (jget usage "total" "outputTokens")
                                           :total (jget usage "total" "totalTokens"))
               (session-context session) (list :used (jget usage "last" "totalTokens") :window (jget usage "modelContextWindow"))))
       (session-update session))
      ((equal method "turn/completed")
       (turn-finished session (when (jget params "turn" "error" "message") (format nil "~%[~A]~%" (jget params "turn" "error" "message")))))
      ((equal method "serverRequest/resolved") (remove-prompt session (jget params "requestId")))
      ((member method '("error" "warning" "configWarning") :test #'equal)
       (session-update session :text (format nil "~%[~A]~%" (or (jget params "error" "message") (jget params "message") (jget params "summary"))))))))

(defun claude-message (session object)
  (let ((type (jget object "type")))
    (cond
      ((equal type "rate_limit_event") (update-claude-limits session (jget object "rate_limit_info")))
      ((equal type "control_response")
       (let* ((response (jget object "response")) (id (parse-integer (or (jget response "request_id") "") :junk-allowed t)))
         (finish-request session id (jget response "response") (when (equal (jget response "subtype") "error") (jget response "error")))))
      ((equal type "control_request")
       (let ((params (jget object "request")) (id (jget object "request_id")))
         (if (equal (jget params "subtype") "mcp_message") (claude-mcp-request session id params)
             (add-prompt session id (jget params "subtype") params))))
      ((equal type "control_cancel_request") (remove-prompt session (jget object "request_id")))
      ((equal type "stream_event")
       (let* ((event (jget object "event")) (kind (jget event "type")))
         (cond ((equal kind "message_start")
                (setf (gethash "claude-message-id" (session-streamed session)) (jget event "message" "id")))
               ((equal kind "content_block_start")
                (setf (gethash "claude-block-index" (session-streamed session)) (jget event "index")))
               ((equal kind "content_block_delta")
                (when (equal (jget event "delta" "type") "text_delta")
                  (record-item-text session (list (gethash "claude-message-id" (session-streamed session)) (jget event "index"))
                    (or (jget event "delta" "text") "") :delta t))))))
      ((and (equal type "system") (equal (jget object "subtype") "init"))
       (setf (session-conversation session) (jget object "session_id")))
      ((equal type "assistant")
       ;; Claude Code sends each finished content block as its own assistant event,
       ;; so a block's position there is not its stream index (a thinking block or
       ;; tool call can come first). The block just streamed has the same key, so
       ;; its text is not shown twice.
       (let* ((id (jget object "message" "id"))
              (blocks (jlist (jget object "message" "content")))
              (streamed (session-streamed session))
              (stream-index (and (= 1 (length blocks))
                                 (equal id (gethash "claude-message-id" streamed))
                                 (gethash "claude-block-index" streamed))))
         (loop for block in blocks for index from 0 do
           (cond ((equal (jget block "type") "text")
                  (record-item-text session (list id (or stream-index index)) (or (jget block "text") "")))
                 ((equal (jget block "type") "tool_use")
                  (session-update session :text (format nil "~%[~A]~%" (jget block "name"))))))))
      ((equal type "result")
       (setf (session-conversation session) (or (jget object "session_id") (session-conversation session)))
       (let* ((usage (jget object "usage")) (model-usage (let ((models (jget object "modelUsage")))
                        (when (and (jobject-p models) (= (length (cdr models)) 1)) (cdadr models)))))
         (setf (session-usage session) (list :input (when (numberp (jget usage "input_tokens"))
                                                     (+ (jget usage "input_tokens") (or (jget usage "cache_read_input_tokens") 0)
                                                        (or (jget usage "cache_creation_input_tokens") 0))) :output (jget usage "output_tokens")
                                           :cost (jget object "total_cost_usd"))
               (session-context session)
               (list :used nil
                     :window (jget model-usage "contextWindow"))))
       (turn-finished session (when (jtrue-p (jget object "is_error"))
                               (format nil "~%[~A]~%" (or (jget object "result") (jget object "errors")))))
       (request session "get_context_usage" (jobj)
         (lambda (result)
           ;; Current SDK uses totalTokens/maxTokens; accept older providers too.
           (when (or (jget result "maxTokens") (jget result "contextWindow"))
             (setf (session-context session)
                   (list :used (or (jget result "totalTokens") (jget result "usedTokens"))
                         :window (or (jget result "maxTokens") (jget result "contextWindow"))))
             (session-update session))) #'identity)))))
(defun provider-message (session object)
  (ecase (session-provider session) (:codex (codex-message session object)) (:claude (claude-message session object))))
