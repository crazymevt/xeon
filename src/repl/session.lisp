(in-package #:xeon.repl)

(defstruct (repl-session (:constructor %session))
  (help-cache (make-hash-table :test #'equal))
  (lock (sb-thread:make-mutex :name "REPL state"))
  (wake (sb-thread:make-semaphore :count 0)) queue thread stopped
  on-change
  connection process output-thread startup-directory cancel-startup dialect (status :disconnected)
  (transcript "") (revision 0) (history nil) input-request debug-stack inspection capabilities
  (prompt "") port-file startup-file ownership-token
  spans results pending-result evaluating (evaluation-id 0) (generation 0)
  problems problem-source)

(defun record-repl (session text &key status kind result-id)
  (sb-thread:with-mutex ((repl-session-lock session))
    (let* ((old-size (length (repl-session-transcript session)))
           (all (concatenate 'string (repl-session-transcript session) text))
           (start (max 0 (- (length all) 131072))))
      (when (and kind (plusp (length text)))
        (push (list old-size (+ old-size (length text)) kind result-id) (repl-session-spans session)))
      (setf (repl-session-transcript session) (subseq all start)
            (repl-session-spans session)
            (loop for (from to style id) in (repl-session-spans session)
                  when (> to start) collect (list (max 0 (- from start)) (- to start) style id))))
    (when status (setf (repl-session-status session) status))
    (incf (repl-session-revision session)))
  (notify-repl-change session))

(defun notify-repl-change (session)
  "Notify outside the state mutex; the UI owns dispatch and coalescing."
  (let ((callback (repl-session-on-change session)))
    (when callback (funcall callback))))

(defun session-stopped-p (session)
  (sb-thread:with-mutex ((repl-session-lock session)) (repl-session-stopped session)))
(defun enqueue-repl (session function)
  (sb-thread:with-mutex ((repl-session-lock session))
    (unless (repl-session-stopped session)
      (when (>= (length (repl-session-queue session)) 256) (error "Too many queued REPL operations."))
      (setf (repl-session-queue session) (nconc (repl-session-queue session) (list function)))
      (sb-thread:signal-semaphore (repl-session-wake session)))))
(defun make-repl-session (&key on-change)
  (let ((session (%session :on-change on-change)))
    (setf (repl-session-thread session)
      (sb-thread:make-thread
        (lambda ()
          (let ((*repl-session* session))
            (loop
              (sb-thread:wait-on-semaphore (repl-session-wake session))
              (when (session-stopped-p session) (return))
              (let ((function (sb-thread:with-mutex ((repl-session-lock session)) (pop (repl-session-queue session)))))
                (when function
                  (handler-case (funcall function)
                    (error (c) (record-repl session (format nil "~%[~A]~%" c)
                      :status (unless (connection-open-p (repl-session-connection session)) :error)))))))))
        :name "Xeon REPL dispatcher"))
    session))
(defun repl-snapshot (session)
  (sb-thread:with-mutex ((repl-session-lock session))
    (list :status (repl-session-status session) :revision (repl-session-revision session)
          :text (copy-seq (repl-session-transcript session)) :prompt (repl-session-prompt session)
          :dialect (repl-session-dialect session) :debug (copy-tree (first (repl-session-debug-stack session)))
          :inspection (copy-tree (repl-session-inspection session)) :capabilities (copy-list (repl-session-capabilities session))
          :spans (copy-tree (repl-session-spans session)) :results (copy-tree (repl-session-results session))
          :problems (copy-tree (repl-session-problems session))
          :generation (repl-session-generation session) :evaluation-id (repl-session-evaluation-id session))))
(defun repl-history (session)
  (sb-thread:with-mutex ((repl-session-lock session)) (mapcar #'copy-seq (repl-session-history session))))
(defun repl-clear (session)
  (sb-thread:with-mutex ((repl-session-lock session))
    (setf (repl-session-transcript session) "" (repl-session-spans session) nil
          (repl-session-results session) nil (repl-session-pending-result session) nil) (incf (repl-session-revision session)))
  (notify-repl-change session))
(defun disconnect-now (session)
  (let ((connection (repl-session-connection session)))
    (setf (repl-session-connection session) nil)
    (when connection
      (ignore-errors (sb-bsd-sockets:socket-shutdown
        (if (typep connection 'nrepl-connection) (nrepl-socket connection) (connection-socket connection)) :direction :io))
      (ignore-errors (backend-disconnect connection))))
  (when (repl-session-process session)
    (ignore-errors (uiop:terminate-process (repl-session-process session)))
    (setf (repl-session-process session) nil))
  (dolist (path (list (repl-session-port-file session) (repl-session-startup-file session) (repl-session-ownership-token session)))
    (when path (ignore-errors (delete-file path))))
  (sb-thread:with-mutex ((repl-session-lock session))
    (dolist (span (repl-session-spans session)) (setf (fourth span) nil))
    (incf (repl-session-generation session))
    (clrhash (repl-session-help-cache session))
    (setf (repl-session-results session) nil (repl-session-pending-result session) nil
          (repl-session-evaluating session) nil
          (repl-session-debug-stack session) nil (repl-session-inspection session) nil
          (repl-session-input-request session) nil (repl-session-capabilities session) nil))
  (record-repl session (format nil "[Disconnected]~%") :status :disconnected))
(defun disconnect-repl (session)
  (sb-thread:with-mutex ((repl-session-lock session)) (setf (repl-session-cancel-startup session) t))
  (enqueue-repl session (lambda () (disconnect-now session))))
(defun stop-repl-session (session)
  ;; Socket shutdown unblocks the reader; no GUI callbacks enter this backend.
  (sb-thread:with-mutex ((repl-session-lock session))
    (setf (repl-session-stopped session) t (repl-session-queue session) nil
          (repl-session-on-change session) nil)
    (clrhash (repl-session-help-cache session)))
  (let ((connection (repl-session-connection session)))
    (when connection
      (ignore-errors (sb-bsd-sockets:socket-shutdown
        (if (typep connection 'nrepl-connection) (nrepl-socket connection) (connection-socket connection)) :direction :io))
      (ignore-errors (sb-bsd-sockets:socket-close
        (if (typep connection 'nrepl-connection) (nrepl-socket connection) (connection-socket connection))))))
  (when (repl-session-process session) (ignore-errors (uiop:terminate-process (repl-session-process session))))
  (dolist (path (list (repl-session-port-file session) (repl-session-startup-file session) (repl-session-ownership-token session)))
    (when path (ignore-errors (delete-file path))))
  (sb-thread:signal-semaphore (repl-session-wake session))
  session)

(defun repl-active-status (session)
  (sb-thread:with-mutex ((repl-session-lock session))
    (cond ((find-if-not (lambda (level) (getf level :post-mortem)) (repl-session-debug-stack session)) :debugging)
          ((repl-session-input-request session) :input)
          ((repl-session-evaluating session) :busy)
          (t :ready))))
(defun finish-repl-evaluation (session connection text)
  (when (eq connection (repl-session-connection session))
    (sb-thread:with-mutex ((repl-session-lock session)) (setf (repl-session-evaluating session) nil))
    (record-repl session text :status (repl-active-status session))))

(defun repl-event (session connection event)
  (when (eq connection (repl-session-connection session))
    (case (first event)
      (:krypton-problem (record-problem session (second event)))
      (:background-message
       (when (eq (second event) :krypton-problem) (record-problem session (third event))))
      (:presentation-start
       (setf (repl-session-pending-result session) (list :id (second event) :text "")))
      (:presentation-end
       (let ((result (repl-session-pending-result session)))
         (when (and result (equal (getf result :id) (second event)))
           (sb-thread:with-mutex ((repl-session-lock session))
             (push (list :id (getf result :id) :text (getf result :text)
                         :generation (repl-session-generation session)) (repl-session-results session))
             (setf (repl-session-results session) (subseq (repl-session-results session) 0
               (min 128 (length (repl-session-results session)))))))
         (setf (repl-session-pending-result session) nil)))
      (:write-string
       (let* ((text (or (second event) "")) (result-p (eq (third event) :repl-result))
              (result (when result-p (repl-session-pending-result session))))
         (when result
           (setf (getf result :text) (concatenate 'string (getf result :text)
             (subseq text 0 (max 0 (min (length text) (- 131072 (length (getf result :text)))))))))
         (record-repl session text :kind (when result-p :result) :result-id (getf result :id))))
      (:new-package
       (setf (connection-package connection) (second event)
             (connection-prompt connection) (or (third event) (second event)))
       (sb-thread:with-mutex ((repl-session-lock session))
         (setf (repl-session-prompt session) (or (third event) (second event) ""))
         (incf (repl-session-revision session))))
      (:read-string
       (setf (repl-session-input-request session) (rest event))
       (record-repl session (format nil "~%[Program is waiting for input]~%") :status :input))
      (:read-aborted
       (setf (repl-session-input-request session) nil)
       (record-repl session "" :status :busy))
      (:debug
       (when (repl-session-evaluating session)
         (record-problem session (list :severity :error :message (format nil "~A" (fourth event)))))
       (destructuring-bind (thread level condition restarts frames &rest more) (rest event)
         (declare (ignore more))
         (sb-thread:with-mutex ((repl-session-lock session))
           (push (list :thread thread :level level :condition (format nil "~A" condition)
                       :restarts restarts :frames frames) (repl-session-debug-stack session)))
         (record-repl session (format nil "~%[Debugger] ~A~%" condition) :status :debugging)))
      (:debug-return
       (sb-thread:with-mutex ((repl-session-lock session))
         (setf (repl-session-debug-stack session)
           (remove-if (lambda (level) (and (equal (getf level :thread) (second event))
                                           (>= (getf level :level) (third event))))
                      (repl-session-debug-stack session))))
       (record-repl session "" :status (repl-active-status session)))
      (:post-mortem
       (let ((frame (find-if (lambda (frame) (and (second frame) (third frame))) (fourth event))))
         (record-problem session
           (append (list :severity :error :message (format nil "~A: ~A" (third event) (second event)))
                   (when frame (list :file (second frame) :line (third frame))))))
       (sb-thread:with-mutex ((repl-session-lock session))
         (setf (repl-session-debug-stack session)
           (list (list :post-mortem t :condition (format nil "~A: ~A" (third event) (second event))
                       :frames (fourth event)))))
       (record-repl session ""))
      (:disconnected (record-repl session (format nil "~%[~A]~%" (second event)) :status :disconnected)))))

(defun prepare-nrepl-values (session connection on-ready)
  (nrepl-send connection
    (list (cons "op" "eval")
          (cons "code" (uiop:read-file-string (asdf:system-relative-pathname "xeon/repl" "vendor/krypton-nrepl/values.clj"))))
    :session :tooling
    :on-done (lambda (responses)
      (unless (some (lambda (reply) (nrepl-status-p reply "eval-error")) responses)
        (setf (nrepl-eval-function connection) "xeon.repl.values/evaluate"))
      (session-ready session connection)
      (when on-ready (funcall on-ready)))
    :on-abort (lambda (reason) (declare (ignore reason))
                (session-ready session connection)
                (when on-ready (funcall on-ready)))))

(defun session-ready (session connection)
  (when (eq connection (repl-session-connection session))
    (let ((capabilities
            (typecase connection
              (swank-connection '(:debugger :inspector :interrupt :compile))
              (nrepl-connection (append '(:interrupt)
                                  (when (nrepl-op-p connection "inspect-push") '(:inspector))
                                  (when (or (nrepl-op-p connection "stacktrace") (nrepl-op-p connection "analyze-last-stacktrace")) '(:post-mortem))))
              (racket-connection (append '(:interrupt :post-mortem) (when (racket-inspector-p connection) '(:inspector)))))))
      (sb-thread:with-mutex ((repl-session-lock session))
        (setf (repl-session-capabilities session) capabilities
              (repl-session-prompt session) (or (connection-prompt connection) ""))))
    (record-repl session (format nil "~%[Connected: ~A]~%" (connection-implementation connection)) :status :ready)))

(defun startup-command (session dialect executable directory)
  (declare (ignore directory))
  (let* ((token (uiop:with-temporary-file (:pathname path :prefix "krypton-repl-" :type "token" :keep t) path))
         (port-file (make-pathname :type "port" :defaults token))
         (startup (make-pathname :type "lisp" :defaults port-file))
         (root (asdf:system-source-directory "xeon/repl")))
    (setf (repl-session-ownership-token session) token
          (repl-session-port-file session) port-file (repl-session-startup-file session) startup)
    (ecase dialect
      (:common-lisp
       (let ((cache (merge-pathnames "xeon-swank/" (uiop:temporary-directory))))
         (ensure-directories-exist cache)
         (with-open-file (out startup :direction :output :if-exists :error)
           (format out "(require :asdf)~%(load ~S)~%(setf swank-loader:*fasl-directory* ~S)~%(swank-loader:init)~%(swank:start-server ~S :dont-close t)~%"
             (namestring (merge-pathnames "vendor/swank/swank-loader.lisp" root)) (namestring cache) (namestring port-file)))
         (list executable "--noinform" "--load" (namestring startup))))
      (:racket (list executable (namestring (merge-pathnames "vendor/cadre-racket/main.rkt" root)) "--port-file" (namestring port-file)))
      (:clojure
       (list executable "-Sdeps" "{:deps {nrepl/nrepl {:mvn/version \"1.7.0\"} cider/cider-nrepl {:mvn/version \"0.62.2\"}}}"
             "-M" "-m" "nrepl.cmdline" "--bind" "127.0.0.1" "--port" "0" "--middleware" "[cider.nrepl/cider-middleware]")))))

(defun launch-runtime (session dialect executable directory)
  (let* ((command (startup-command session dialect executable directory))
         (process (uiop:launch-program command :directory directory :input :stream :output :stream :error-output :output)))
    (setf (repl-session-process session) process)
    (let ((port nil) (port-lock (sb-thread:make-mutex)))
      (setf (repl-session-output-thread session)
        (sb-thread:make-thread
          (lambda ()
            (handler-case
                (let ((stream (uiop:process-info-output process)) (line ""))
                  (loop for c = (read-char stream nil nil) while c do
                    (setf line (concatenate 'string line (string c)))
                    (when (or (char= c #\Newline) (>= (length line) 1024))
                      (let ((start (search "nREPL server started on port " line)))
                        (when start
                          (sb-thread:with-mutex (port-lock)
                            (setf port (parse-integer line :start (+ start 29) :junk-allowed t)))))
                      (record-repl session line) (setf line ""))))
              (error (c) (unless (session-stopped-p session) (record-repl session (format nil "[Runtime output: ~A]" c))))))
          :name "Xeon runtime output"))
      (loop with deadline = (+ (get-internal-real-time) (* 120 internal-time-units-per-second)) do
        (when (or (session-stopped-p session)
                  (sb-thread:with-mutex ((repl-session-lock session)) (repl-session-cancel-startup session)))
          (ignore-errors (uiop:terminate-process process)) (error "Runtime startup cancelled."))
        (let ((number (or (sb-thread:with-mutex (port-lock) port)
                          (when (probe-file (repl-session-port-file session))
                            (ignore-errors (parse-integer (uiop:read-file-string (repl-session-port-file session)) :junk-allowed t))))))
          (when (and number (<= 1 number 65535)) (return number)))
        (unless (uiop:process-alive-p process) (error "Runtime exited before opening its REPL port."))
        (when (> (get-internal-real-time) deadline) (uiop:terminate-process process) (error "Runtime startup timed out."))
        (sleep .05)))))

(defun connect-repl (session dialect host port &key launch executable directory on-ready)
  (check-type host string) (check-type port (integer 1 65535))
  (unless (member dialect '(:common-lisp :clojure :racket)) (error "Unknown REPL language."))
  (sb-thread:with-mutex ((repl-session-lock session))
    (setf (repl-session-cancel-startup session) nil))
  (enqueue-repl session
    (lambda ()
      (disconnect-now session)
      (setf (repl-session-dialect session) dialect)
      (record-repl session "" :status (if launch :starting :connecting))
      (let* ((actual-port (if launch (launch-runtime session dialect executable directory) port))
             (active nil)
             (deliver (lambda (function)
                        (enqueue-repl session
                          (lambda () (when (eq active (repl-session-connection session)) (funcall function))))))
             (handler (lambda (connection event) (repl-event session connection event)))
             (ready (lambda (connection)
                      (if (typep connection 'nrepl-connection)
                          (prepare-nrepl-values session connection on-ready)
                          (progn (session-ready session connection)
                                 (when on-ready (funcall on-ready))))))
             (failure (lambda (reason) (record-repl session (format nil "~%[Connection failed: ~A]~%" reason) :status :error)))
             (connection
               (ecase dialect
                 (:common-lisp (swank-connect (if launch "127.0.0.1" host) actual-port :deliver deliver :handler handler))
                 (:clojure (nrepl-connect (if launch "127.0.0.1" host) actual-port :deliver deliver :handler handler :on-ready ready :on-failure failure))
                 (:racket (racket-connect (if launch "127.0.0.1" host) actual-port :deliver deliver :handler handler :on-ready ready :on-failure failure)))))
        (setf active connection (repl-session-connection session) connection)
        (when (eq dialect :common-lisp) (swank-start-session connection :on-ready ready :on-failure failure))))))

(defun require-session-connection (session)
  (let ((connection (repl-session-connection session)))
    (unless (connection-open-p connection) (error "Connect a REPL first.")) connection))
(defun retain-repl-history (source history)
  "Retain the latest submission plus older entries within an 8 Mi-character budget."
  (loop with size = (length source) with result = (list source)
        for previous in history for count from 1 below 100
        while (<= (+ size (length previous)) (* 8 1024 1024)) do
          (incf size (length previous)) (push previous result)
        finally (return (nreverse result))))

(defun repl-submit (session source &key source-context)
  (check-type source string)
  (let ((source (copy-seq source)))
    (enqueue-repl session
      (lambda ()
        (let ((connection (require-session-connection session)))
          (cond ((repl-session-input-request session)
                 (destructuring-bind (thread tag &rest more) (repl-session-input-request session)
                   (declare (ignore more))
                   (backend-send-input connection thread tag (concatenate 'string source (string #\Newline))))
                 (setf (repl-session-input-request session) nil) (record-repl session "" :status :busy))
                (t
                 (unless (eq (getf (repl-snapshot session) :status) :ready) (error "REPL is not ready to evaluate."))
                 (begin-problem-operation session source-context)
                 (sb-thread:with-mutex ((repl-session-lock session))
                   (setf (repl-session-history session) (retain-repl-history source (repl-session-history session))))
                 (incf (repl-session-evaluation-id session))
                 (setf (repl-session-evaluating session) t)
                 (record-repl session (format nil "~A> " (connection-prompt connection)) :status :busy :kind :prompt)
                 (record-repl session (format nil "~A~%" (subseq source (max 0 (- (length source) 131071)))))
                 (handler-case
                  (backend-repl-eval connection source
                   :on-ok (lambda (result) (declare (ignore result)) (finish-repl-evaluation session connection ""))
                   :on-abort (lambda (reason)
                               (record-problem session (list :severity :error :message (format nil "~A" reason)))
                               (finish-repl-evaluation session connection (format nil "~%[Aborted: ~A]~%" reason))))
                  (error (condition)
                    (record-problem session (list :severity :error :message (format nil "~A" condition)))
                    (finish-repl-evaluation session connection (format nil "~%[Aborted: ~A]~%" condition)))))))))))
(defun repl-interrupt (session)
  (enqueue-repl session (lambda ()
    (when (member (repl-active-status session) '(:busy :input :debugging))
      (backend-interrupt (require-session-connection session))))))

(defun store-inspection (session inspection)
  (sb-thread:with-mutex ((repl-session-lock session)) (setf (repl-session-inspection session) inspection))
  (record-repl session ""))
(defun swank-inspection-request (session connection form &key (thread t))
  ;; Tooling errors belong to the inspector, not an interactive debugger level.
  (let ((condition (remote-symbol "krypton-inspection-condition")))
    (swank-rex connection
      (list (remote-symbol "cl:handler-case") form
        (list (remote-symbol "cl:error") (list condition)
          (list (remote-symbol "cl:list") :krypton-inspection-error
            (list (remote-symbol "cl:princ-to-string") condition))))
      :thread thread
      :on-ok (lambda (value)
        (if (and (consp value) (eq (first value) :krypton-inspection-error))
            (record-repl session (format nil "~%[Inspector: ~A]~%" (second value)))
            (store-inspection session value)))
      :on-abort (lambda (reason) (record-repl session (format nil "~%[Inspector aborted: ~A]~%" reason))))))

(defun repl-inspect (session expression)
  (enqueue-repl session
    (lambda ()
      (when (every #'whitespace-char-p expression) (error "Enter an expression to inspect."))
      (let* ((connection (require-session-connection session)) (debug (first (repl-session-debug-stack session)))
             (success (lambda (value) (store-inspection session value))))
        (typecase connection
          (swank-connection
           (swank-inspection-request session connection (swank-call "swank:init-inspector" expression)
             :thread (or (getf debug :thread) t)))
          (nrepl-connection
           (unless (nrepl-op-p connection "inspect-push") (error "This nREPL server does not provide cider-nrepl inspection."))
           (nrepl-inspect connection expression (connection-package connection) :on-ok success))
          (racket-connection
           (unless (racket-inspector-p connection) (error "This Racket bridge does not provide an inspector."))
           (racket-request connection :inspect-expression (list expression) nil success nil))
          (t (error "This backend does not provide an inspector.")))))))
(defun repl-inspector-action (session kind &optional index expected)
  (enqueue-repl session
    (lambda ()
      (block action
      (when (and expected (not (equal expected (repl-session-inspection session))))
        (return-from action nil))
      (let ((connection (require-session-connection session)) (success (lambda (value) (store-inspection session value))))
        (typecase connection
          (swank-connection
           (let ((debug (first (repl-session-debug-stack session))))
             (swank-inspection-request session connection
               (ecase kind (:part (swank-call "swank:inspect-nth-part" index))
                           (:back (swank-call "swank:inspector-pop"))
                           (:refresh (swank-call "swank:inspector-reinspect")))
               :thread (or (getf debug :thread) t))))
          (nrepl-connection
           (nrepl-inspect-op connection (ecase kind (:part "inspect-push") (:back "inspect-pop") (:refresh "inspect-refresh"))
                            :index index :on-ok success))
          (racket-connection
           (racket-request connection (ecase kind (:part :inspect-part) (:back :inspect-back) (:refresh :inspect-refresh))
             (if (eq kind :part) (list index) nil) nil success nil))))))))
(defun debug-request (session form on-ok)
  (let* ((connection (require-session-connection session)) (debug (first (repl-session-debug-stack session))))
    (unless (and (typep connection 'swank-connection) debug (not (getf debug :post-mortem)))
      (error "No paused Swank debugger."))
    (swank-rex connection form :thread (getf debug :thread) :on-ok on-ok)))
(defun repl-debug-frame (session frame)
  (enqueue-repl session (lambda ()
    (let ((level (first (repl-session-debug-stack session))))
      (debug-request session (swank-call "swank:frame-locals-and-catch-tags" frame)
        (lambda (locals)
          (sb-thread:with-mutex ((repl-session-lock session))
            (when (eq level (first (repl-session-debug-stack session)))
              (setf (getf (first (repl-session-debug-stack session)) :locals) (first locals)
                    (getf (first (repl-session-debug-stack session)) :selected-frame) frame)))
          (record-repl session "")))))))
(defun repl-debug-restart (session index)
  (enqueue-repl session (lambda ()
    (let ((debug (first (repl-session-debug-stack session))))
      (debug-request session (swank-call "swank:invoke-nth-restart-for-emacs" (getf debug :level) index) #'identity)))))
(defun repl-debug-abort (session)
  (enqueue-repl session (lambda ()
    (let ((debug (first (repl-session-debug-stack session))))
      (if (getf debug :post-mortem)
          (progn (sb-thread:with-mutex ((repl-session-lock session)) (setf (repl-session-debug-stack session) nil)) (record-repl session ""))
          (debug-request session (swank-call "swank:throw-to-toplevel") #'identity))))))
(defun repl-debug-inspect-local (session frame index)
  (enqueue-repl session (lambda ()
    (let ((debug (first (repl-session-debug-stack session))))
      (unless (and debug (not (getf debug :post-mortem))) (error "No paused Swank debugger."))
      (swank-inspection-request session (require-session-connection session)
        (swank-call "swank:inspect-frame-var" frame index) :thread (getf debug :thread))))))

(defun repl-inspect-result (session id generation)
  "Inspect a retained runtime object, never its printed expression."
  (enqueue-repl session
    (lambda ()
      (let ((valid (sb-thread:with-mutex ((repl-session-lock session))
                     (and (= generation (repl-session-generation session))
                          (find id (repl-session-results session) :key (lambda (result) (getf result :id)) :test #'equal)))))
        (unless valid (error "This returned value is no longer available.")))
      (let ((connection (require-session-connection session)) (success (lambda (value) (store-inspection session value))))
        (typecase connection
          (swank-connection (swank-inspection-request session connection (swank-call "swank:inspect-presentation" id t)))
          (nrepl-connection
           (unless (and (nrepl-eval-function connection) (nrepl-op-p connection "inspect-push"))
             (error "This nREPL server cannot inspect returned values."))
           (nrepl-inspect connection (format nil "(xeon.repl.values/value ~D)" (parse-integer id))
             (connection-package connection) :on-ok success))
          (racket-connection (racket-request connection :inspect-result (list id) nil success nil)))))))
