(in-package #:xeon.http)

;; SBCL worker lifecycle adapter. Never calls UI APIs from the worker.
(defstruct (request-job (:constructor %make-job))
  (lock (sb-thread:make-mutex :name "HTTP job")) thread cancelled
  (state :running) response error)

(defun job-cancelled-p (job)
  (sb-thread:with-mutex ((request-job-lock job)) (request-job-cancelled job)))

(defun start-request (request)
  "Snapshot a request and start one worker. Poll JOB-SNAPSHOT on the UI thread."
  (validate-request request)
  (initialize-transport)
  (let ((snapshot (copy-request request)) (job (%make-job)))
    (setf (request-url snapshot) (copy-seq (request-url request))
          (request-method snapshot) (copy-seq (request-method request))
          (request-ca-file snapshot) (when (request-ca-file request) (copy-seq (request-ca-file request)))
          (request-headers snapshot) (mapcar (lambda (h) (cons (copy-seq (car h)) (copy-seq (cdr h))))
                                            (request-headers request))
          (request-body snapshot) (when (request-body request) (copy-seq (request-body request))))
    (setf (request-job-thread job)
          (sb-thread:make-thread
           (lambda ()
             (let ((response nil) (failure nil))
               (handler-case (setf response (send-request snapshot :cancelled-p (lambda () (job-cancelled-p job))))
                 (error (condition) (setf failure condition)))
               (sb-thread:with-mutex ((request-job-lock job))
                 (setf (request-job-state job) (cond ((request-job-cancelled job) :cancelled)
                                                           (failure :failed) (t :complete))
                       (request-job-response job) (unless (request-job-cancelled job) response)
                       (request-job-error job) (unless (request-job-cancelled job) failure)))))
           :name "Xeon HTTP request"))
    job))

(defun cancel-request (job)
  "Cooperatively cancel; never terminate a thread inside foreign code."
  (sb-thread:with-mutex ((request-job-lock job))
    (when (eq :running (request-job-state job)) (setf (request-job-cancelled job) t)))
  job)

(defun job-snapshot (job)
  (sb-thread:with-mutex ((request-job-lock job))
    (values (request-job-state job) (request-job-response job) (request-job-error job))))

(defun wait-request (job &key (timeout 1))
  "Bounded wait for tests/shutdown. NIL means the native operation has not returned."
  (not (eq :pending (sb-thread:join-thread (request-job-thread job) :timeout timeout :default :pending))))
