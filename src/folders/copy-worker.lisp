(in-package #:xeon.folders)

(defstruct (file-copy-job (:constructor %copy-job))
  thread cancel done (copied 0) total current result error
  #+sb-thread (lock (sb-thread:make-mutex :name "file copy progress")))
(defmacro with-copy-lock ((job) &body body)
  #+sb-thread `(sb-thread:with-mutex ((file-copy-job-lock ,job)) ,@body)
  #-sb-thread `(progn ,@body))

(defun start-file-copy (project paths directory)
  "Copy without GUI/manager objects; progress is coalesced into one bounded snapshot."
  (let ((job (%copy-job)) (paths (copy-list paths)))
    (flet ((run ()
             (handler-case
                 (let ((result (copy-folder-entries project paths directory
                         :cancelled-p (lambda () (with-copy-lock (job) (file-copy-job-cancel job)))
                         :progress (lambda (copied total current)
                                     (with-copy-lock (job)
                                       (setf (file-copy-job-copied job) copied (file-copy-job-total job) total
                                             (file-copy-job-current job) current))))))
                   (with-copy-lock (job) (setf (file-copy-job-result job) result)))
               (error (condition) (with-copy-lock (job) (setf (file-copy-job-error job) (princ-to-string condition)))))
             (with-copy-lock (job) (setf (file-copy-job-done job) t))))
      #+sb-thread (setf (file-copy-job-thread job) (sb-thread:make-thread #'run :name "Xeon file copy"))
      #-sb-thread (run))
    job))

(defun file-copy-status (job)
  (with-copy-lock (job)
    (values (file-copy-job-done job) (file-copy-job-copied job) (file-copy-job-total job)
            (file-copy-job-current job) (copy-list (file-copy-job-result job)) (file-copy-job-error job))))

(defun cancel-file-copy (job &key wait-p)
  #-sb-thread (declare (ignore wait-p))
  (with-copy-lock (job) (setf (file-copy-job-cancel job) t))
  #+sb-thread
  (when (and wait-p (file-copy-job-thread job))
    (when (eq :timed-out (sb-thread:join-thread (file-copy-job-thread job) :timeout 5 :default :timed-out))
      (error "File copy did not stop within five seconds.")))
  job)
