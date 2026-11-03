(in-package #:xeon.ui)

;;; AI chat: a tab holding one Codex or Claude conversation. Native controls above
;;; choose the provider, model, effort and mode, delegate approvals, and show the
;;; connection, context window and usage limits. A web view below shows the
;;; transcript (rendered by the backend as escaped HTML), request cards for
;;; approvals, questions and forms, and the composer.
;;;
;;; The backend session reports changes on its own threads; the tab coalesces them
;;; into one UI update and sends only changed messages to the page.

(defclass chat-page (vbox)
  ((workbench :initarg :workbench :reader chat-workbench)
   (session :initform nil :accessor chat-session)
   (epoch :initform 0 :accessor chat-epoch)
   (view :accessor chat-view)
   (ready :initform nil :accessor chat-ready-p)
   (snapshot :initform nil :accessor chat-snapshot)
   (versions :initform (make-hash-table) :reader chat-versions)
   (codes :initform (make-hash-table :test 'equal) :reader chat-codes)
   (images :initform nil :accessor chat-images)
   (pending-user-id :initform nil :accessor chat-pending-user-id)
   (post-pending :initform nil :accessor chat-post-pending)
   (lock :initform (sb-thread:make-mutex :name "Chat UI delivery") :reader chat-lock)
   (queued-scripts :initform '() :accessor chat-queued-scripts
                   :documentation "Calls made before the page finished loading, in order.")
   (controls :initform (make-hash-table) :reader chat-controls)
   (program :initarg :program :initform nil :reader chat-program
            :documentation "A provider command to run instead of the installed CLI, for tests."))
  (:default-initargs :spacing 0))

(defun control (page name) (gethash name (chat-controls page)))
(defun (setf control) (widget page name) (setf (gethash name (chat-controls page)) widget))

(defun chat-json (value) (xeon.assistants::json-string value))
(defun chat-object (&rest fields) (apply #'xeon.assistants::jobj fields))
(defun chat-get (value &rest path) (apply #'xeon.assistants::jget value path))
(defun chat-array (value) (xeon.assistants::jlist value))

(defun chat-call (page method &rest arguments)
  "Calls the page's JavaScript METHOD with ARGUMENTS as JSON. Calls made after the page
said it was ready but before the web view finished loading wait for it."
  (let ((view (chat-view page))
        (script (format nil "window.~A(~{~A~^,~});" method (mapcar #'chat-json arguments))))
    (when (and (chat-ready-p page) (not (destroyed-p view)))
      (if (web-view-ready-p view)
          (run-chat-script page script)
          (setf (chat-queued-scripts page) (append (chat-queued-scripts page) (list script)))))))

(defun run-chat-script (page script)
  (run-script (chat-view page) script :callback (lambda (result error) (declare (ignore result error)))))

(defun flush-chat-scripts (page)
  (let ((scripts (chat-queued-scripts page)))
    (setf (chat-queued-scripts page) '())
    (dolist (script scripts) (run-chat-script page script))))

(defvar *chat-store-override* nil
  "A chat history folder to use instead of Xeon's configuration folder, for tests.")

(defun chat-history-store ()
  (or *chat-store-override*
      (merge-pathnames "chats/" (xeon.themes:default-configuration-directory))))

(defun chat-folder (workbench)
  (or (explorer-folder workbench) (uiop:getcwd)))

(defun chat-page-html ()
  "The backend's trusted chat page, with Xeon's request cards and composer."
  (let* ((base (xeon.chat-render:chat-page))
         (script (search "<script nonce=" base)))
    (concatenate 'string (subseq base 0 script)
                 "<style>" (uiop:read-file-string (asdf:system-relative-pathname "xeon" "assets/chat/chat.css")) "</style>"
                 "<script nonce='xeon-chat'>" (uiop:read-file-string (asdf:system-relative-pathname "xeon" "assets/chat/chat.js")) "</script>"
                 (subseq base script))))

(defun default-provider ()
  (or (xeon.assistants:preferred-provider (chat-history-store))
      (cond ((xeon.assistants:assistant-available-p :codex) :codex)
            ((xeon.assistants:assistant-available-p :claude) :claude)
            (t :codex))))

(defun provider-label (provider) (if (eq provider :codex) "Codex" "Claude"))

;;; Notifications from the session threads

(defun notify-chat (page session)
  (when (and (eq session (chat-session page)) (not (destroyed-p page)))
    (when (sb-thread:with-mutex ((chat-lock page))
            (unless (chat-post-pending page) (setf (chat-post-pending page) t)))
      (call-in-ui-thread (lambda () (refresh-chat page)) :owner page))))

;;; Session lifetime

(defun replace-chat-session (page provider &key (connect t))
  (when (chat-session page) (xeon.assistants:stop-session (chat-session page)))
  (incf (chat-epoch page))
  (setf (chat-images page) nil (chat-pending-user-id page) nil (chat-snapshot page) nil)
  (clrhash (chat-versions page)) (clrhash (chat-codes page))
  (chat-call page "xeonWorkspace.attachments" #())
  (chat-call page "xeonInput.ack" :false)
  (chat-call page "xeonChat.reset")
  (let ((backend *backend*) (workbench (chat-workbench page)))
    (setf (chat-session page)
          (xeon.assistants:make-session
           provider (chat-folder workbench)
           :program (chat-program page)
           :store (chat-history-store)
           :tool-context (lambda () (let ((*backend* backend)) (chat-tool-context workbench)))
           :tool-handler (lambda (name arguments complete current-p context)
                           (let ((*backend* backend))
                             (call-in-ui-thread
                              (lambda ()
                                (handler-case (chat-tool workbench name arguments complete current-p context)
                                  (error (condition) (funcall complete (princ-to-string condition) t))))
                              :owner page)))
           :on-change (lambda (session) (let ((*backend* backend)) (notify-chat page session))))))
  (xeon.assistants:remember-provider (chat-history-store) provider)
  (when connect (xeon.assistants:start-session (chat-session page)))
  (refresh-chat page))

(defun stop-chat (page)
  (when (chat-session page)
    (xeon.assistants:stop-session (chat-session page))
    (setf (chat-session page) nil)))

(defun chat-pages (workbench)
  (remove-if-not (lambda (page) (typep page 'chat-page)) (all-tab-pages workbench)))

(defun follow-chat-folders (workbench folder)
  "Moves every chat to the newly opened folder: each starts a new conversation there."
  (dolist (page (chat-pages workbench))
    (when (chat-session page)
      (setf (chat-images page) nil (chat-pending-user-id page) nil)
      (chat-call page "xeonInput.ack" :false)
      (chat-call page "xeonWorkspace.attachments" #())
      (xeon.assistants:change-session-directory (chat-session page) folder))))

;;; Snapshots to the page and the native controls

(defparameter *tool-request-titles*
  '(("repl_start" . "Start a Lisp runtime?") ("repl_evaluate" . "Evaluate in the REPL?")
    ("repl_interrupt" . "Interrupt the REPL?") ("debugger_restart" . "Invoke a debugger restart?")
    ("debugger_abort" . "Abort the debugger level?") ("inspector_evaluate" . "Evaluate in the inspector?")
    ("editor_replace" . "Edit a document?") ("editor_write" . "Replace a document's text?")
    ("editor_save" . "Save a document?"))
  "Plain titles for the editor and runtime actions the assistant asks to take.")

(defun request-title (prompt)
  (if (equal (getf prompt :method) "runtime-tool")
      (let ((name (chat-get (getf prompt :params) "title")))
        (or (cdr (assoc name *tool-request-titles* :test #'equal)) (getf prompt :title)))
      (getf prompt :title)))

(defun request-summary (prompt)
  "The part of a request worth reading before the details: the command, or a tool's
arguments."
  (let ((params (getf prompt :params)))
    (cond ((equal (getf prompt :method) "runtime-tool")
           (let ((arguments (chat-get params "arguments")))
             (if (and arguments (cdr arguments))
                 (string-right-trim '(#\Newline) (xeon.assistants::prompt-value-text arguments))
                 (format nil "Tool: ~A" (chat-get params "title")))))
          ((chat-get params "command")
           (let ((command (chat-get params "command")))
             (if (stringp command) command (xeon.assistants::prompt-value-text command))))
          ((chat-get params "input" "command") (chat-get params "input" "command"))
          (t nil))))

(defun chat-request-object (prompt epoch)
  (chat-object "id" (getf prompt :id) "generation" (getf prompt :generation) "epoch" epoch
               "kind" (getf prompt :kind) "title" (request-title prompt)
               "summary" (or (request-summary prompt) :null)
               "details" (getf prompt :details)
               "schema" (or (getf prompt :schema) :null)
               "confirmation" (if (xeon.assistants::confirmation-elicitation-p prompt) t :false)
               "url" (or (chat-get (getf prompt :params) "url") :null)
               "decisions" (coerce (getf prompt :decisions) 'vector)
               "questions" (coerce (loop for q in (getf prompt :questions) collect
                                     (chat-object "id" (getf q :id) "label" (getf q :label)
                                                  "multiple" (if (getf q :multiple) t :false)
                                                  "secret" (if (getf q :secret) t :false)
                                                  "options" (coerce (loop for o in (getf q :options) collect
                                                                          (chat-object "label" (getf o :label)
                                                                                       "description" (or (getf o :description) :null)))
                                                                    'vector)))
                                   'vector)))

(defun chat-state-object (page state)
  (chat-object "status" (getf state :status) "mode" (getf state :mode)
               "error" (or (getf state :error) :null) "errorRevision" (getf state :error-revision)
               "requests" (coerce (mapcar (lambda (p) (chat-request-object p (chat-epoch page))) (getf state :requests))
                                  'vector)))

(defun context-text (state)
  (let* ((context (getf state :context)) (used (getf context :used)) (window (getf context :window)))
    (cond ((and (numberp used) (numberp window) (plusp window))
           (format nil "Context ~A / ~A" (token-count used) (token-count window)))
          ((numberp used) (format nil "Context ~A" (token-count used)))
          ((numberp window) (format nil "Context window ~A" (token-count window)))
          (t "Context not reported"))))

(defun token-count (n)
  "A compact token count: 950, 12.4k, 1.2M."
  (cond ((< n 1000) (format nil "~D" n))
        ((< n 1000000) (format nil "~,1Fk" (/ n 1000)))
        (t (format nil "~,1FM" (/ n 1000000)))))

(defun context-fraction (state)
  (let* ((context (getf state :context)) (used (getf context :used)) (window (getf context :window)))
    (and (numberp used) (numberp window) (plusp window) (min 1d0 (float (/ used window) 1d0)))))

(defun tokens-text (state)
  (let ((usage (getf state :usage)))
    (if usage
        (format nil "Tokens ~A in · ~A out"
                (if (numberp (getf usage :input)) (token-count (getf usage :input)) "—")
                (if (numberp (getf usage :output)) (token-count (getf usage :output)) "—"))
        "")))

(defun status-caption (status)
  (case status
    (:ready "Idle") (:starting "Connecting…") (:busy "Working…") (:waiting "Needs your input")
    (:error "Not connected") (t (string-capitalize status))))

(defun status-color (status)
  (case status
    (:ready :system-green) (:busy :accent) (:waiting :system-orange) (:error :system-red) (t :secondary-text)))

(defun set-combo (combo items selected labels)
  "Shows ITEMS (ids) in COMBO with LABELS, selecting SELECTED, without emitting :changed."
  (with-handlers-blocked (combo :changed)
    (unless (equal items (coerce (items combo) 'list))
      (setf (label-function combo) (lambda (id) (or (cdr (assoc id labels :test #'equal)) (princ-to-string id)))
            (items combo) items))
    (let ((index (or (position selected items :test #'equal) 0)))
      (unless (eql index (selected-index combo)) (setf (selected-index combo) index)))))

(defun refresh-chat-controls (page state)
  (let* ((status (getf state :status))
         (settled (member status '(:ready :error)))
         (live (member status '(:ready :busy :waiting)))
         (models (getf state :models))
         (model (if (getf state :model)
                    (find (getf state :model) models :key (lambda (m) (getf m :id)) :test #'equal)
                    (find-if (lambda (m) (getf m :default)) models)))
         (efforts (getf model :efforts)))
    (set-combo (control page :provider) '(:codex :claude) (getf state :provider)
               '((:codex . "Codex") (:claude . "Claude")))
    (set-combo (control page :model) (cons nil (mapcar (lambda (m) (getf m :id)) models)) (getf state :model)
               (cons '(nil . "Default model") (mapcar (lambda (m) (cons (getf m :id) (getf m :label))) models)))
    (set-combo (control page :effort) (cons nil efforts) (getf state :effort)
               (cons '(nil . "Default effort") (mapcar (lambda (e) (cons e (string-capitalize e))) efforts)))
    (set-combo (control page :mode) '(:agent :chat) (getf state :mode)
               '((:agent . "Agent") (:chat . "Chat only")))
    (dolist (name '(:provider :model :mode :new :clear))
      (set-enabled (control page name) settled))
    (set-enabled (control page :effort) (and settled efforts))
    (let ((approve (control page :approve)))
      (with-handlers-blocked (approve :toggled)
        (setf (checked approve) (and (getf state :auto-approve) t)))
      (set-enabled approve (and live (eq (getf state :mode) :agent))))
    (setf (visible (control page :reconnect)) (eq status :error))
    (let ((label (control page :status)))
      (setf (text label) (format nil "● ~A" (status-caption status))
            (text-color label) (status-color status)))
    (let ((limits (control page :limits)))
      (setf (text limits) (xeon.assistants::limits-summary (getf state :limits))
            (tooltip limits) (xeon.assistants::limits-detail (getf state :limits))))
    (setf (text (control page :context)) (context-text state))
    (let ((fraction (context-fraction state)) (bar (control page :context-bar)))
      (setf (visible bar) (and fraction t))
      (when fraction (setf (value bar) fraction))
      (setf (tooltip (control page :context))
            (if fraction
                (format nil "~,1F% of the model's context window is in use." (* 100 fraction))
                "The provider has not reported context usage yet.")))
    (setf (text (control page :tokens)) (tokens-text state))
    (let ((directory (getf state :directory)))
      (setf (text (control page :folder))
            (if directory
                (or (car (last (pathname-directory (uiop:ensure-directory-pathname directory)))) directory)
                "")
            (tooltip (control page :folder))
            (and directory (format nil "The assistant works in ~A" directory))))))

(defun chat-message-payload (page state)
  (chat-object "version" (getf state :revision) "provider" (getf state :provider)
               "busy" (if (member (getf state :status) '(:busy :waiting)) t :false)
               "waiting" (if (eq (getf state :status) :waiting) t :false)
               "messages"
               (coerce
                (loop for message in (getf state :messages)
                      for id = (getf message :id) for version = (getf message :version)
                      collect (chat-object "id" id "version" version "html"
                                           (if (eql version (gethash id (chat-versions page)))
                                               :omit
                                               (let ((codes '()))
                                                 (prog1 (xeon.chat-render:render-message
                                                         message (getf state :provider)
                                                         (lambda (index code)
                                                           (let ((key (format nil "~D-~D-~D" id version index)))
                                                             (push (cons key code) codes)
                                                             key)))
                                                   (setf (gethash id (chat-codes page)) codes
                                                         (gethash id (chat-versions page)) version))))))
                'vector)))

(defun refresh-chat (page)
  (sb-thread:with-mutex ((chat-lock page)) (setf (chat-post-pending page) nil))
  (let ((session (chat-session page)))
    (when (and session (not (destroyed-p page)))
      (let* ((state (xeon.assistants:session-snapshot session))
             (previous (chat-snapshot page)))
        (setf (chat-snapshot page) state)
        (refresh-chat-controls page state)
        (update-chat-tab page state)
        ;; A sent message is settled once the backend records it (or fails).
        (when (chat-pending-user-id page)
          (cond ((find-if (lambda (m) (and (eq (getf m :role) :user) (> (getf m :id) (chat-pending-user-id page))))
                          (getf state :messages))
                 (setf (chat-pending-user-id page) nil (chat-images page) nil)
                 (chat-call page "xeonWorkspace.ack" t)
                 (chat-call page "xeonWorkspace.attachments" #()))
                ((eq (getf state :status) :error)
                 (setf (chat-pending-user-id page) nil)
                 (chat-call page "xeonWorkspace.ack" :false))))
        (when (chat-ready-p page)
          (chat-call page "xeonWorkspace.update" (chat-state-object page state))
          (unless (and previous (= (getf previous :revision) (getf state :revision))
                       (= (getf previous :generation) (getf state :generation)))
            (when (or (null previous) (/= (getf previous :generation) (getf state :generation)))
              (clrhash (chat-versions page))
              (chat-call page "xeonChat.reset"))
            (let ((live (mapcar (lambda (m) (getf m :id)) (getf state :messages))))
              (maphash (lambda (id version) (declare (ignore version))
                         (unless (member id live) (remhash id (chat-versions page)) (remhash id (chat-codes page))))
                       (chat-versions page)))
            (let ((json (chat-json (chat-message-payload page state)))
                  (version (getf state :revision)))
              (chat-call page "xeonChat.begin" version)
              (loop for start from 0 below (length json) by 24000
                    do (chat-call page "xeonChat.chunk" version (subseq json start (min (length json) (+ start 24000)))))
              (chat-call page "xeonChat.end" version))))))))

(defun update-chat-tab (page state)
  (let ((tabs (parent page)))
    (when (and tabs (member page (children tabs)))
      (let ((label (format nil "~A chat" (provider-label (getf state :provider)))))
        (unless (equal label (tab-label tabs page)) (setf (tab-label tabs page) label)))
      ;; A request waiting for the user is marked in the tab, as unsaved files are.
      (let ((waiting (and (getf state :requests) t)) (accessory (tab-accessory tabs page)))
        (unless (eq waiting (and accessory t))
          (setf (tab-accessory tabs page)
                (when waiting (make-instance 'label :text "●" :text-color :system-orange
                                                    :font (make-font :size 9) :valign :center
                                                    :tooltip "The assistant is waiting for you"))))))))

;;; Theme

(defun chat-theme-object (palette)
  (if (native-palette-p palette)
      (chat-object "scheme" "light dark"
                   "bg" (native-css :content-background) "fg" (native-css :text)
                   "muted" (native-css :secondary-text)
                   "surface" "light-dark(#f5f5f7,#262628)"
                   "user" (format nil "color-mix(in srgb,~A 22%,transparent)" (native-css :accent))
                   "border" (native-css :separator) "accent" (native-css :accent)
                   "code" "light-dark(rgba(0,0,0,.05),rgba(255,255,255,.07))" "warn" (native-css :system-orange)
                   "kw" (native-css :system-pink) "str" (native-css :system-red) "comment" (native-css :secondary-text)
                   "number" (native-css :system-orange) "symbol" (native-css :system-teal)
                   "function" (native-css :system-purple) "parameter" (native-css :system-indigo))
      (flet ((c (role) (palette-color palette role)))
        (chat-object "scheme" (if (palette-dark-p palette) "dark" "light")
                     "bg" (c :editor-background) "fg" (c :editor-foreground) "muted" (c :sidebar-foreground)
                     "surface" (c :input-background)
                     "user" (format nil "color-mix(in srgb,~A 30%,~A)" (c :accent) (c :input-background))
                     "border" (c :border) "accent" (c :accent) "code" (c :sidebar-background) "warn" "#e5aa85"
                     "kw" (c :syntax-keyword) "str" (c :syntax-string) "comment" (c :syntax-comment)
                     "number" (c :syntax-number) "symbol" (c :syntax-symbol) "function" (c :syntax-function)
                     "parameter" (c :syntax-parameter)))))

(defun theme-chat (page)
  (chat-call page "xeonChat.theme" (chat-theme-object (workbench-palette (chat-workbench page)))))

(defun restyle-chats (workbench)
  (dolist (page (chat-pages workbench)) (theme-chat page)))

;;; Messages from the page

(defun chat-settled-p (page)
  (member (getf (xeon.assistants:session-snapshot (chat-session page)) :status) '(:ready :error)))

(defun chat-ready (page)
  (setf (chat-ready-p page) t (chat-snapshot page) nil)
  (clrhash (chat-versions page))
  (theme-chat page)
  (refresh-chat page)
  (chat-call page "xeonWorkspace.attachments" (coerce (chat-images page) 'vector))
  (chat-call page "xeonWorkspace.focus"))

(defun handle-chat-message (page json)
  (handler-case
      (let* ((event (xeon.assistants::parse-json json))
             (type (chat-get event "type"))
             (session (chat-session page)))
        (cond
          ((equal type "ready") (chat-ready page))
          ((null session) nil)
          ((equal type "submit-input")
           (when (chat-pending-user-id page) (error "Your previous message is still being sent."))
           (let ((text (chat-get event "text")) (state (xeon.assistants:session-snapshot session)))
             (unless (and (stringp text) (<= (length text) 131072)) (error "Messages must be text under 128 KiB."))
             (unless (member (getf state :status) '(:ready :busy :waiting)) (error "The assistant is not connected yet."))
             (when (and (zerop (length (string-trim '(#\Space #\Newline #\Return #\Tab) text))) (null (chat-images page)))
               (error "Write a message or attach a screenshot first."))
             (setf (chat-pending-user-id page)
                   (reduce #'max (getf state :messages) :key (lambda (m) (getf m :id)) :initial-value 0))
             (handler-case (xeon.assistants:send-message session text :images (chat-images page))
               (error (c) (setf (chat-pending-user-id page) nil) (error c)))))
          ((equal type "interrupt") (xeon.assistants:interrupt-session session))
          ((equal type "answer")
           (let* ((state (xeon.assistants:session-snapshot session))
                  (prompt (find (chat-get event "id") (getf state :requests) :key (lambda (p) (getf p :id)) :test #'equal))
                  (generation (chat-get event "generation"))
                  (decision (chat-get event "decision"))
                  (answers (chat-get event "answers")))
             (unless (eql (chat-get event "epoch") (chat-epoch page)) (error "That request has expired."))
             (unless (and prompt (integerp generation) (= generation (getf prompt :generation)))
               (error "That request has expired."))
             (when (eq (getf prompt :kind) :questions)
               (setf answers (loop for (id . values) in (cdr answers) collect (cons id (chat-array values)))))
             (xeon.assistants:answer-request session (getf prompt :id) generation
                                             (cond ((equal decision "allow") :allow) ((equal decision "deny") :deny)
                                                   ((equal decision "cancel") :cancel) (t decision))
                                             answers)))
          ((equal type "attach-image")
           (unless (eq (getf (xeon.assistants:session-snapshot session) :status) :ready)
             (error "Screenshots can be attached while the assistant is idle."))
           (let ((images (append (chat-images page) (list (chat-get event "url")))))
             (xeon.assistants::validate-message-images images)
             (setf (chat-images page) images)
             (chat-call page "xeonWorkspace.attachments" (coerce images 'vector))))
          ((equal type "remove-image")
           (when (chat-pending-user-id page) (error "Wait for your message to finish sending."))
           (let ((index (chat-get event "index")))
             (unless (and (integerp index) (< -1 index (length (chat-images page))))
               (error "That attachment is no longer available."))
             (setf (chat-images page) (loop for image in (chat-images page) for i from 0 unless (= i index) collect image))
             (chat-call page "xeonWorkspace.attachments" (coerce (chat-images page) 'vector))))
          ((equal type "attachment-error") (error "~A" (chat-get event "text")))
          ((equal type "copy-code")
           (let ((code (loop for codes being the hash-values of (chat-codes page)
                             thereis (cdr (assoc (chat-get event "id") codes :test #'equal)))))
             (when code
               (setf (clipboard-text) code)
               (chat-call page "xeonChat.copied" (chat-get event "id")))))
          ((equal type "copy-message")
           (let ((message (find (chat-get event "id") (getf (chat-snapshot page) :messages) :key (lambda (m) (getf m :id)))))
             (when message (setf (clipboard-text) (getf message :text)))))
          ((equal type "open-link")
           (let ((url (chat-get event "url")))
             (when (xeon.chat-render::safe-link-p url) (open-url url))))
          ((equal type "sidebar-shortcut")
           (if (equal (chat-get event "id") "left") (toggle-sidebar (chat-workbench page)) (toggle-tools (chat-workbench page))))
          ((member type '("selection" "rendered") :test #'equal) nil)
          (t nil)))
    (error (condition)
      (chat-call page "xeonWorkspace.error" (princ-to-string condition))
      (chat-call page "xeonWorkspace.ack" :false))))

;;; Native header actions

(defun configure-chat (page &rest settings)
  (with-reported-errors ((chat-workbench page))
    (unless (chat-settled-p page) (error "Wait for the current turn before changing AI settings."))
    (apply #'xeon.assistants:configure-session (chat-session page) settings)))

(defun confirm-chat-action (page message detail)
  (confirm message :detail detail :yes "Continue" :parent (workbench-window (chat-workbench page))))

(defun make-chat-header (page)
  (let* ((provider (make-instance 'combo-box :control-size :small :tooltip "AI provider"))
         (model (make-instance 'combo-box :control-size :small :tooltip "Model"))
         (effort (make-instance 'combo-box :control-size :small :tooltip "Reasoning effort"))
         (mode (make-instance 'combo-box :control-size :small
                                         :tooltip "Agent can run commands, edit files and use the editor, REPL and debugger. Chat only discusses."))
         (approve (make-instance 'check-box :label "Approve for me" :control-size :small
                                            :tooltip "For this chat: allow commands, file edits and Xeon editor, REPL and debugger actions without asking. Questions and permission requests still ask."))
         (new (small-button "New Chat" :tooltip "Start a new conversation in this tab"))
         (clear (small-button "Clear Context" :tooltip "Keep the transcript, but start a fresh provider conversation"))
         (reconnect (small-button "Reconnect" :visible nil :tooltip "Reconnect and resume this conversation"))
         (status (status-label))
         (limits (status-label :tooltip "Usage limits"))
         (context (status-label))
         (bar (make-instance 'progress-bar :min 0 :max 1 :value 0 :width 70 :valign :center :visible nil
                                           :control-size :small))
         (tokens (status-label))
         (folder (status-label :halign :end)))
    (setf (control page :provider) provider (control page :model) model (control page :effort) effort
          (control page :mode) mode (control page :approve) approve (control page :new) new
          (control page :clear) clear (control page :reconnect) reconnect (control page :status) status
          (control page :limits) limits (control page :context) context (control page :context-bar) bar
          (control page :tokens) tokens (control page :folder) folder)
    (on provider :changed
        (lambda (e)
          (let ((value (event-value e)))
            (when (and value (not (eq value (xeon.assistants:session-provider (chat-session page)))))
              (with-reported-errors ((chat-workbench page))
                (unless (chat-settled-p page) (error "Wait for the current turn before switching providers."))
                (replace-chat-session page value))))))
    (on model :changed (lambda (e) (configure-chat page :model (event-value e))))
    (on effort :changed (lambda (e) (configure-chat page :effort (event-value e))))
    (on mode :changed (lambda (e) (configure-chat page :mode (event-value e))))
    (on approve :toggled
        (lambda (e) (with-reported-errors ((chat-workbench page))
                      (xeon.assistants:set-auto-approve (chat-session page) (event-value e)))))
    (on new :clicked
        (lambda (e) (declare (ignore e))
          (when (confirm-chat-action page "Start a new chat?" "The current conversation is saved in Xeon's chat history.")
            (replace-chat-session page (xeon.assistants:session-provider (chat-session page))))))
    (on clear :clicked
        (lambda (e) (declare (ignore e))
          (when (confirm-chat-action page "Clear the conversation context?"
                                     "The transcript stays visible, but the assistant starts a fresh conversation without it.")
            (xeon.assistants:clear-context (chat-session page)))))
    (on reconnect :clicked
        (lambda (e) (declare (ignore e))
          (xeon.assistants:start-session (chat-session page) :resume t)))
    (ui (vbox :spacing 0 :surface :toolbar
          (hbox :padding '(8 12 4 12) :spacing 8
            (:widget provider) (:widget model) (:widget effort) (:widget mode)
            (spacer)
            (:widget approve) (:widget reconnect) (:widget new) (:widget clear))
          (hbox :padding '(2 14 8 14) :spacing 16
            (:widget status) (:widget limits)
            (hbox :spacing 8 (:widget context) (:widget bar))
            (:widget tokens) (spacer) (:widget folder))
          (separator)))))

;;; Opening a chat

(defun open-ai-chat (workbench &key (provider (default-provider)) program (connect t))
  "Opens a new AI chat tab for the project folder."
  (let* ((page (make-instance 'chat-page :workbench workbench :program program))
         (view (make-instance 'web-view :html (chat-page-html) :local-messages-p t)))
    (setf (chat-view page) view)
    (add page (make-chat-header page))
    (add page view :expand t)
    (on view :message (lambda (e) (handle-chat-message page (event-value e))))
    (on view :loaded (lambda (e) (declare (ignore e)) (flush-chat-scripts page)))
    (on view :navigation-request
        (lambda (e)
          (when (or (event-new-window-p e) (member (event-navigation-kind e) '(:link :form)))
            (veto e))))
    (on view :load-error
        (lambda (e) (report workbench "Chat page failed to load: ~A" (event-value e))))
    (let ((tabs (workbench-tabs workbench)))
      (add tabs page :tab-label (format nil "~A chat" (provider-label provider))
                     :tab-icon (icon :chat :size 14))
      (replace-chat-session page provider :connect connect)
      (select-page tabs page)
      (focus view))
    page))

(defun active-chat (workbench)
  (let ((page (selected-page (workbench-tabs workbench))))
    (and page (typep page 'chat-page) page)))
