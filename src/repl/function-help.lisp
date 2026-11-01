(in-package #:xeon.repl)

(defgeneric backend-function-help (connection name package success failure))

(defmethod backend-function-help ((connection swank-connection) name package success failure)
  (swank-request connection (swank-call "swank:operator-arglist" name package) package
    (lambda (signature)
      (if (stringp signature)
          (swank-request connection (swank-call "swank:documentation-symbol" name) package
            (lambda (doc) (funcall success signature doc))
            (lambda (reason) (declare (ignore reason)) (funcall success signature nil)))
          (funcall success nil nil))) failure))

(defmethod backend-function-help ((connection racket-connection) name package success failure)
  (racket-request connection :arglist (list name) package
    (lambda (signature)
      (if (stringp signature)
          (racket-request connection :describe (list name) package
            (lambda (doc) (funcall success signature doc))
            (lambda (reason) (declare (ignore reason)) (funcall success signature nil)))
          (funcall success nil nil))) failure))

(defmethod backend-function-help ((connection nrepl-connection) name package success failure)
  (let ((op (cond ((nrepl-op-p connection "info") "info")
                  ((nrepl-op-p connection "lookup") "lookup"))))
    (if (null op) (funcall success nil nil)
        (nrepl-send connection
          (list (cons "op" op) (cons "sym" name) (cons "ns" (or package "user")))
          :session :tooling :on-abort failure
          :on-done
          (lambda (responses)
            (let ((signature nil) (doc nil))
              (dolist (response responses)
                (let* ((info (or (dict-get response "info") response))
                       (args (or (dict-get info "arglists-str") (dict-get info "arglists"))))
                  (when (stringp args) (setf signature (format nil "~A ~A" name args)))
                  (when (stringp (dict-get info "doc")) (setf doc (dict-get info "doc")))))
              (funcall success signature doc)))))))

(defun help-text (value)
  (when (and (stringp value) (plusp (length value)))
    (substitute #\Space #\Null (subseq value 0 (min 4096 (length value))))))

(defun repl-function-help (session name dialect)
  "Nonblocking metadata lookup in the connected REPL's current package/namespace.
Returns cached signature and documentation. No editor source is evaluated.
Cache keys include connection, namespace and evaluation epoch; expires in 5s.
At most eight requests are pending and 64 entries retained per session."
  (unless (and (stringp name) (<= 1 (length name) 256))
    (return-from repl-function-help nil))
  (let (key entry connection package generation evaluation)
    (sb-thread:with-mutex ((repl-session-lock session))
      (setf connection (repl-session-connection session))
      (unless (and (not (repl-session-stopped session)) connection
                   (connection-open-p connection)
                   (eq dialect (repl-session-dialect session))
                   (eq :ready (repl-session-status session)))
        (return-from repl-function-help nil))
      (setf package (connection-package connection)
            generation (repl-session-generation session)
            evaluation (repl-session-evaluation-id session)
            key (list connection generation evaluation package name)
            entry (gethash key (repl-session-help-cache session)))
      (when (and entry (or (eq (second entry) :pending)
                          (< (- (get-internal-real-time) (first entry))
                             (* 5 internal-time-units-per-second))))
        (return-from repl-function-help (values (third entry) (fourth entry))))
      (when (>= (loop for value being the hash-values of (repl-session-help-cache session)
                      count (eq (second value) :pending)) 8)
        (return-from repl-function-help nil))
      (when (>= (hash-table-count (repl-session-help-cache session)) 64)
        (let ((expired (loop for k being the hash-keys of (repl-session-help-cache session)
                             using (hash-value value)
                             unless (eq (second value) :pending) collect k)))
          (dolist (k expired) (remhash k (repl-session-help-cache session)))))
      (setf entry (list (get-internal-real-time) :pending nil nil)
            (gethash key (repl-session-help-cache session)) entry))
    (labels ((finish (signature doc)
               (sb-thread:with-mutex ((repl-session-lock session))
                 (when (eq entry (gethash key (repl-session-help-cache session)))
                   (if (and (not (repl-session-stopped session))
                            (eq connection (repl-session-connection session))
                            (= generation (repl-session-generation session))
                            (= evaluation (repl-session-evaluation-id session))
                            (equal package (connection-package connection)))
                       (setf (gethash key (repl-session-help-cache session))
                             (list (get-internal-real-time) :done (help-text signature) (help-text doc)))
                       (remhash key (repl-session-help-cache session)))))))
      (enqueue-repl session
        (lambda ()
          (if (and (eq connection (repl-session-connection session))
                     (= generation (repl-session-generation session))
                     (= evaluation (repl-session-evaluation-id session))
                     (equal package (connection-package connection)))
            (handler-case
                (backend-function-help connection name package #'finish
                  (lambda (reason) (declare (ignore reason)) (finish nil nil)))
              (error () (finish nil nil)))
              (finish nil nil)))))
    (values nil nil)))
