;;;; connection.lisp — a connection to an nREPL server (Clojure)
;;;;
;;;; Messages both ways are bencoded dictionaries. A request carries an "op"
;;;; and an "id"; the server answers with any number of messages with that id
;;;; (output, values, the namespace), the last one with "done" in its
;;;; "status". A reader thread reads them and hands each to the
;;;; connection's DELIVER function, as Swank's does (swank/connection.lisp).
;;;;
;;;; Two sessions are cloned at the start: one for evaluation and the REPL,
;;;; whose *1, *e and namespace persist, and one for tooling (completion,
;;;; documentation), so those keep working while code runs. Output, values
;;;; for the REPL and namespace changes go to the HANDLER as Swank-shaped
;;;; events, (:write-string text target) and (:new-package name prompt), so
;;;; the editor shows them as it shows Swank's. cider-nrepl also sends
;;;; messages nobody asked for (its "state"), some with the id of a request
;;;; already done; those are ignored.

(in-package #:xeon.repl)

(defstruct (nrepl-request (:constructor make-nrepl-request (on-message on-done on-abort)))
  on-message on-done on-abort (responses '()))

(defclass nrepl-connection (lisp-connection)
  ((eval-function :initform nil :accessor nrepl-eval-function)
   (socket :initarg :socket :reader nrepl-socket)
   (stream :initarg :stream :reader nrepl-stream)
   (write-lock :initform (sb-thread:make-mutex :name "nrepl write") :reader nrepl-write-lock)
   (next-id :initform 1 :accessor nrepl-next-id)
   (requests :initform (make-hash-table :test 'equal :synchronized t) :reader nrepl-requests)
   (reader-thread :initform nil :accessor nrepl-reader-thread)
   (deliver :initarg :deliver :initform #'funcall :reader nrepl-deliver)
   (handler :initarg :handler :initform nil :accessor nrepl-handler
            :documentation "Called with the connection and each Swank-shaped event.")
   (session :initform nil :accessor nrepl-session :documentation "The session code is evaluated in.")
   (tooling-session :initform nil :accessor nrepl-tooling-session)
   (ops :initform '() :accessor nrepl-ops :documentation "The ops the server has, from describe.")
   (versions :initform '() :accessor nrepl-versions)
   (eval-ids :initform '() :accessor nrepl-eval-ids :documentation "Ids of evaluations still running."))
  (:default-initargs :package "user" :prompt "user")
  (:documentation "A connection to a Clojure nREPL server."))

(defmethod connection-language ((connection nrepl-connection)) :clojure)

(defmethod connection-implementation ((connection nrepl-connection))
  (let ((clojure (dict-get (nrepl-versions connection) "clojure")))
    (format nil "Clojure~@[ ~a~]" (and clojure (dict-get clojure "version-string")))))

(defun nrepl-op-p (connection op)
  "True if the server has OP (cider-nrepl adds info, complete, fn-refs…)."
  (member op (nrepl-ops connection) :test #'string=))

(defun nrepl-write (connection message)
  (sb-thread:with-mutex ((nrepl-write-lock connection))
    (let ((stream (nrepl-stream connection)))
      (write-sequence (bencode message) stream)
      (finish-output stream))))

(defun nrepl-send (connection message &key on-message on-done on-abort (session :eval))
  "Send MESSAGE (an alist with \"op\" and the op's arguments). SESSION is
:eval, :tooling, nil (none) or a session id. ON-MESSAGE is called with each
answer as it arrives, ON-DONE with all of them (in order) after the last,
ON-ABORT with a reason if the connection closes first. Returns the id."
  (let* ((id (princ-to-string (nrepl-next-id connection)))
         (session (case session
                    (:eval (nrepl-session connection))
                    (:tooling (or (nrepl-tooling-session connection) (nrepl-session connection)))
                    (t session)))
         (message (append (list (cons "id" id))
                          (and session (list (cons "session" session)))
                          message)))
    (incf (nrepl-next-id connection))
    (setf (gethash id (nrepl-requests connection)) (make-nrepl-request on-message on-done on-abort))
    (handler-case (nrepl-write connection message)
      (error (e)
        (remhash id (nrepl-requests connection))
        (editor-error "Could not send to nREPL: ~a" e)))
    id))

(defun nrepl-status-p (message status)
  (member status (dict-get message "status") :test #'string=))

(defun nrepl-dispatch (connection message)
  ;; cider-nrepl's state messages say what each namespace defines: learn
  ;; the indentation of the macros with :style/indent.
  (let* ((id (dict-get message "id"))
         (request (and id (gethash id (nrepl-requests connection)))))
    (cond (request
           ;; Long-running evaluations may print forever. Keep a bounded
           ;; reply tail; output is delivered separately to the transcript.
           (when (>= (length (nrepl-request-responses request)) 32)
             (setf (nrepl-request-responses request) (subseq (nrepl-request-responses request) 0 31)))
           (push message (nrepl-request-responses request))
           (when (nrepl-request-on-message request)
             (funcall (nrepl-request-on-message request) message))
           (when (nrepl-status-p message "done")
             (remhash id (nrepl-requests connection))
             (setf (nrepl-eval-ids connection) (remove id (nrepl-eval-ids connection) :test #'string=))
             (when (nrepl-request-on-done request)
               (funcall (nrepl-request-on-done request) (reverse (nrepl-request-responses request))))))
          ;; cider-nrepl's state messages, and stray output: output still shows.
          ((or (dict-get message "out") (dict-get message "err"))
           (nrepl-event connection (list :write-string (or (dict-get message "out") (dict-get message "err")) nil))))))

(defun nrepl-event (connection event)
  (when (nrepl-handler connection)
    (funcall (nrepl-handler connection) connection event)))

(defun nrepl-reader-loop (connection)
  (let ((stream (nrepl-stream connection)))
    (loop
      (multiple-value-bind (message reason)
          (handler-case (values (bdecode-stream stream) nil)
            (end-of-file () (values nil "nREPL closed the connection."))
            (error (e) (values nil (princ-to-string e))))
        (cond (reason
               (funcall (nrepl-deliver connection) (lambda () (nrepl-closed connection reason)))
               (return))
              ((consp message)
               (funcall (nrepl-deliver connection)
                        (lambda ()
                          (handler-case (nrepl-dispatch connection message)
                            (error (e) (message "Error handling a message from nREPL: ~a" e)))))))))))

(defun nrepl-closed (connection reason)
  (unless (eq (connection-state connection) :closed)
    (setf (connection-state connection) :closed)
    (ignore-errors (sb-bsd-sockets:socket-close (nrepl-socket connection)))
    (let ((pending (loop for r being the hash-values of (nrepl-requests connection) collect r)))
      (clrhash (nrepl-requests connection))
      (dolist (r pending)
        (when (nrepl-request-on-abort r) (funcall (nrepl-request-on-abort r) reason))))
    (nrepl-event connection (list :disconnected reason))))

(defun nrepl-connect (host port &key (deliver #'funcall) handler on-ready (on-failure #'default-abort))
  "Connect to the nREPL server at HOST:PORT, clone the sessions and ask what
it can do; then call ON-READY with the connection (through DELIVER)."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case (sb-bsd-sockets:socket-connect socket (resolve-host host) port)
      (error (e)
        (sb-bsd-sockets:socket-close socket)
        (editor-error "Could not connect to nREPL at ~a:~d: ~a" host port e)))
    (let ((connection (make-instance 'nrepl-connection
                                     :socket socket :host host :port port
                                     :stream (sb-bsd-sockets:socket-make-stream
                                              socket :input t :output t
                                                     :element-type '(unsigned-byte 8) :buffering :full)
                                     :deliver deliver :handler handler)))
      (setf (nrepl-reader-thread connection)
            (sb-thread:make-thread (lambda () (nrepl-reader-loop connection))
                                   :name (format nil "nrepl reader ~a:~d" host port)))
      (flet ((clone (then)
               (nrepl-send connection '(("op" . "clone")) :session nil :on-abort on-failure
                           :on-done (lambda (responses)
                                      (let ((session (some (lambda (m) (dict-get m "new-session")) responses)))
                                        (if session
                                            (funcall then session)
                                            (funcall on-failure "nREPL didn't make a session")))))))
        (clone (lambda (session)
                 (setf (nrepl-session connection) session)
                 (clone (lambda (tooling)
                          (setf (nrepl-tooling-session connection) tooling)
                          (nrepl-send connection '(("op" . "describe")) :session :tooling :on-abort on-failure
                                      :on-done (lambda (responses)
                                                 (let ((reply (first responses)))
                                                   (setf (nrepl-ops connection)
                                                         (mapcar #'car (dict-get reply "ops"))
                                                         (nrepl-versions connection) (dict-get reply "versions")))
                                                 (when on-ready (funcall on-ready connection)))))))))
      connection)))

(defun nrepl-disconnect (connection)
  (when (connection-open-p connection)
    (ignore-errors (close (nrepl-stream connection)))
    (nrepl-closed connection "Disconnected.")))
