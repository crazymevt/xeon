(in-package #:xeon.repl)

(defun begin-problem-operation (session context)
  "Replace diagnostics for this source; retain issues in other documents."
  (let ((context (copy-list context)))
    (sb-thread:with-mutex ((repl-session-lock session))
      (setf (repl-session-problem-source session) context
            (repl-session-problems session)
            (remove-if (lambda (issue)
                         (or (equal (getf context :source-id) (getf issue :source-id))
                             (and (getf context :file) (equal (getf context :file) (getf issue :file)))))
                       (repl-session-problems session)))
      (incf (repl-session-revision session))))
  (notify-repl-change session))

(defun normalize-problem (note context)
  "Normalize Swank locations to zero-based character positions, without reading code."
  (let* ((location (getf note :location))
         (source (when (eq (first location) :location) (second location)))
         ;; A generated :SOURCE-FORM is not positioned in the original file.
         (where (when (member (first source) '(:file :buffer :buffer-and-file)) (third location)))
         (file (case (first source) (:file (second source)) (:buffer-and-file (third source))))
         (position (case (first where)
                     (:position (and (integerp (second where)) (max 0 (1- (second where)))))
                     (:offset (and (integerp (second where)) (integerp (third where))
                                   (max 0 (+ (1- (second where)) (third where))))))))
    (list :source-id (getf context :source-id) :revision (getf context :revision)
          :buffer-id (getf context :buffer-id)
          :buffer-name (or (when (member (first source) '(:buffer :buffer-and-file)) (second source))
                           (getf context :buffer-name))
          :file (or (getf note :file) file (getf context :file))
          :position (or position (unless (or file (getf note :line)) (getf context :position)))
          :line (getf note :line) :column (getf note :column)
          :severity (or (getf note :severity) :error)
          :message (let ((message (format nil "~A" (getf note :message))))
                     (subseq message 0 (min 8192 (length message)))))))

(defun record-problem (session note)
  ;; Reinstalling a definition is routine editor compilation, not an issue.
  (when (eq :redefinition (getf note :severity)) (return-from record-problem nil))
  (let ((issue (normalize-problem note (repl-session-problem-source session))))
    (sb-thread:with-mutex ((repl-session-lock session))
      (unless (member issue (repl-session-problems session) :test #'equal)
        (setf (repl-session-problems session)
              (subseq (cons issue (repl-session-problems session)) 0
                      (min 500 (1+ (length (repl-session-problems session)))))))
      (incf (repl-session-revision session))))
  (notify-repl-change session))

(defun repl-compile (session source &key source-context (line 1) (column 1))
  "Compile and install the current editor source, without saving it to disk."
  (check-type source string)
  (let ((source (copy-seq source)) (context (copy-list source-context)))
    (enqueue-repl session
      (lambda ()
        (let ((connection (require-session-connection session)))
          (unless (backend-supports-p connection :compile) (error "This runtime does not support compilation."))
          (unless (eq :ready (getf (repl-snapshot session) :status)) (error "REPL is not ready to compile."))
          (begin-problem-operation session context)
          (incf (repl-session-evaluation-id session))
          (setf (repl-session-evaluating session) t)
          (record-repl session (format nil "~%[Compiling ~A]~%" (or (getf context :buffer-name) "source")) :status :busy)
          (labels ((failed (reason)
                     (when (eq connection (repl-session-connection session))
                       (record-problem session (list :severity :error :message (format nil "~A" reason)))
                       (finish-repl-evaluation session connection (format nil "[Compilation aborted: ~A]~%" reason)))))
            (handler-case
                (backend-compile-form connection source
                  :buffer-name (getf context :buffer-name) :file (getf context :file)
                  :position (1+ (or (getf context :position) 0)) :line line :column column
                  :on-ok (lambda (result)
                           (when (eq connection (repl-session-connection session))
                             (dolist (note (second result)) (record-problem session note))
                             (unless (or (third result) (second result))
                               (record-problem session (list :severity :error :message "Compilation failed.")))
                             (finish-repl-evaluation session connection
                               (format nil "[Compilation ~A: ~D diagnostic~:P]~%"
                                 (if (third result) "finished" "failed") (length (second result))))))
                  :on-abort #'failed)
              (error (condition) (failed condition)))))))))
