(in-package #:xeon.folders)
(defstruct folder-result id value error)
(defstruct (folder-worker (:constructor %worker))
  (pending nil) (results nil) (next-id 0) stopped thread
  #+sb-thread (lock (sb-thread:make-mutex :name "folder worker"))
  #+sb-thread (wake (sb-thread:make-semaphore :count 0)))
(defmacro with-worker-lock ((worker) &body body)
  #+sb-thread `(sb-thread:with-mutex ((folder-worker-lock ,worker)) ,@body)
  #-sb-thread `(progn ,@body))
(defun perform-folder-work (request)
  (destructuring-bind (id kind subject directory show-hidden) request
    (handler-case
        (make-folder-result :id id :value
                            (ecase kind
                              (:open (let ((project (open-folder-project subject)))
                                       (multiple-value-bind (systems warning) (discover-folder-systems project)
                                         (list project (list-folder-children project (project-root project) :show-hidden show-hidden) systems warning))))
                              (:git-status (read-git-status subject))
                              (:git-diff (read-git-diff subject (second directory) :staged-p (eq (first directory) :staged)))
                              (:git-action (destructuring-bind (action paths message) directory
                                             (perform-git-action subject action paths :message message)
                                             (read-git-status subject)))
                              (:children (list-folder-children subject directory :show-hidden show-hidden))))
      (error (condition) (make-folder-result :id id :error (princ-to-string condition))))))
(defun make-folder-worker ()
  (let ((worker (%worker)))
    #+sb-thread
    (setf (folder-worker-thread worker)
          (sb-thread:make-thread
           (lambda ()
             (loop
               (sb-thread:wait-on-semaphore (folder-worker-wake worker))
               (let ((request (with-worker-lock (worker)
                                (when (folder-worker-stopped worker) (return))
                                (pop (folder-worker-pending worker)))))
                 (when request
                   (let ((result (perform-folder-work request)))
                     (with-worker-lock (worker)
                       (unless (folder-worker-stopped worker) (push result (folder-worker-results worker)))))))))
           :name "Xeon folder reader"))
    worker))
(defun request-folder-work (worker kind subject &key directory show-hidden)
  (ecase kind (:open (check-type subject (or string pathname))) ((:children :git-status :git-action :git-diff) (check-type subject folder-project)))
  (with-worker-lock (worker)
    (when (folder-worker-stopped worker) (error "Folder worker is stopped."))
    (when (eq kind :open) (setf (folder-worker-pending worker) nil))
    (when (>= (length (folder-worker-pending worker)) 64) (error "Too many pending folder requests."))
    (let ((request (list (incf (folder-worker-next-id worker)) kind subject directory show-hidden)))
      #+sb-thread
      (progn (setf (folder-worker-pending worker) (nconc (folder-worker-pending worker) (list request)))
             (sb-thread:signal-semaphore (folder-worker-wake worker)))
      #-sb-thread (push (perform-folder-work request) (folder-worker-results worker))
      (first request))))
(defun drain-folder-results (worker)
  (with-worker-lock (worker) (prog1 (nreverse (folder-worker-results worker)) (setf (folder-worker-results worker) nil))))
(defun stop-folder-worker (worker &key wait-p)
  #-sb-thread (declare (ignore wait-p))
  (with-worker-lock (worker) (setf (folder-worker-stopped worker) t (folder-worker-pending worker) nil (folder-worker-results worker) nil))
  #+sb-thread (sb-thread:signal-semaphore (folder-worker-wake worker))
  #+sb-thread
  (when (and wait-p (folder-worker-thread worker))
    (when (eq :timed-out (sb-thread:join-thread (folder-worker-thread worker) :timeout 5 :default :timed-out))
      (error "Folder reader did not finish shutdown within five seconds.")))
  worker)
