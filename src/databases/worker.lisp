(in-package #:xeon.databases)

;;; A database worker owns one SQLite connection on its own thread and runs jobs on
;;; it one at a time, so a slow query never blocks the caller. The connection opens
;;; with the first job and closes when the worker stops. Callbacks run on the worker
;;; thread; a UI hands them to its own thread.

(defclass database-worker ()
  ((path :initarg :path :reader database-worker-path)
   (read-only :initarg :read-only :initform nil :reader database-worker-read-only-p)
   (connection :initform nil :accessor worker-connection)
   (lock :initform (sb-thread:make-mutex :name "Database worker") :reader worker-lock)
   (wakeup :initform (sb-thread:make-waitqueue :name "Database worker") :reader worker-wakeup)
   (jobs :initform '() :accessor worker-jobs)
   (busy :initform nil :accessor worker-busy)
   (stopped :initform nil :accessor worker-stopped)
   (thread :initform nil :accessor worker-thread)))

(defun make-database-worker (path &key read-only (name "Xeon database"))
  "Starts a worker for the SQLite database at PATH."
  (let ((worker (make-instance 'database-worker :path (pathname path) :read-only read-only)))
    (setf (worker-thread worker)
          (sb-thread:make-thread (lambda () (run-worker worker)) :name name))
    worker))

(defun database-worker-busy-p (worker)
  "True while a job runs or waits."
  (sb-thread:with-mutex ((worker-lock worker))
    (and (or (worker-busy worker) (worker-jobs worker)) t)))

(defun database-worker-stopped-p (worker)
  (sb-thread:with-mutex ((worker-lock worker)) (worker-stopped worker)))

(defun submit-database-job (worker function &optional on-done)
  "Queues FUNCTION, called with the open connection on the worker thread. ON-DONE is then
called there with FUNCTION's value and NIL, or NIL and the error message."
  (sb-thread:with-mutex ((worker-lock worker))
    (when (worker-stopped worker) (error "The database connection is closed."))
    (setf (worker-jobs worker) (append (worker-jobs worker) (list (cons function on-done))))
    (sb-thread:condition-notify (worker-wakeup worker)))
  worker)

(defun interrupt-database-worker (worker)
  "Stops the running statement, if any; its job ends with an \"interrupted\" error."
  (sb-thread:with-mutex ((worker-lock worker))
    (when (and (worker-busy worker) (worker-connection worker))
      (interrupt (worker-connection worker)))))

(defun stop-database-worker (worker)
  "Drops queued jobs, interrupts the running one and closes the connection. Returns at
once; the thread finishes on its own."
  (sb-thread:with-mutex ((worker-lock worker))
    (unless (worker-stopped worker)
      (setf (worker-stopped worker) t (worker-jobs worker) '())
      (when (and (worker-busy worker) (worker-connection worker))
        (interrupt (worker-connection worker)))
      (sb-thread:condition-notify (worker-wakeup worker))))
  worker)

(defun next-job (worker)
  "Waits for a job; NIL once the worker is stopped."
  (sb-thread:with-mutex ((worker-lock worker))
    (loop
      (when (worker-stopped worker) (return nil))
      (let ((job (pop (worker-jobs worker))))
        (when job (setf (worker-busy worker) t) (return job)))
      (sb-thread:condition-wait (worker-wakeup worker) (worker-lock worker)))))

(defun worker-open-connection (worker)
  (or (worker-connection worker)
      (let ((connection (connect (database-worker-path worker)
                                 :read-only (database-worker-read-only-p worker))))
        (sb-thread:with-mutex ((worker-lock worker))
          (setf (worker-connection worker) connection)))))

(defun run-worker (worker)
  (unwind-protect
       (loop for job = (next-job worker)
             while job
             do (destructuring-bind (function . on-done) job
                  (let ((value nil) (message nil))
                    (handler-case (setf value (funcall function (worker-open-connection worker)))
                      (error (condition) (setf message (princ-to-string condition))))
                    (sb-thread:with-mutex ((worker-lock worker)) (setf (worker-busy worker) nil))
                    (when on-done (ignore-errors (funcall on-done value message))))))
    (let ((connection (sb-thread:with-mutex ((worker-lock worker))
                        (setf (worker-stopped worker) t (worker-busy worker) nil)
                        (shiftf (worker-connection worker) nil))))
      (when connection (ignore-errors (disconnect connection))))))
