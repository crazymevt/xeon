(in-package #:xeon.folders)

(defstruct workspace-hit path buffer revision start end line column preview signature)
(defstruct workspace-report hits (skipped 0) truncated-p)

(defun search-workspace (project snapshots query &key (limit 2000) cancelled-p)
  "Search PROJECT recursively and (original snapshot revision) buffers. Snapshots win
 over disk files, including ignored files; skip .git, links and binary/large files.
Return bounded location records, never GUI objects or retained file buffers."
  (check-type limit (integer 1 100000))
  (let ((hits nil) (skipped 0) (seen (make-hash-table :test #+win32 'equalp #-win32 'equal))
        (count 0) (truncated nil))
    (labels ((cancel () (when (and cancelled-p (funcall cancelled-p))
                         (error 'xeon.core:search-cancelled)))
             (key (path) (namestring (or (ignore-errors (truename path)) path)))
             (scan (buffer path original revision)
               (cancel)
               (xeon.core:map-search-matches
                (xeon.core:make-search-session buffer query)
                (lambda (match)
                  (when (>= count limit) (setf truncated t) (return-from search-workspace
                    (make-workspace-report :hits (nreverse hits) :skipped skipped :truncated-p t)))
                  (multiple-value-bind (line column)
                      (xeon.core:position-line-column buffer (xeon.core:search-match-start match))
                    (let* ((start (max (xeon.core:line-column-position buffer line 0)
                                       (- (xeon.core:search-match-start match) 48)))
                           (preview (xeon.core:buffer-substring buffer start
                                      (min (xeon.core:buffer-length buffer) (+ start 160))))
                           (newline (position #\Newline preview)))
                      (push (make-workspace-hit :path path :buffer original
                              :revision revision
                              :start (xeon.core:search-match-start match) :end (xeon.core:search-match-end match)
                              :signature (sxhash (xeon.core:search-match-text match))
                              :line (1+ line) :column (1+ column)
                              :preview (string-trim '(#\Return #\Tab #\Space) (subseq preview 0 newline))) hits)
                      (incf count)))) :cancelled-p cancelled-p))
             (walk (directory)
               (cancel)
               (handler-case
                   (dolist (entry (list-folder-children project directory :show-hidden t))
                     (cancel)
                     (let ((path (entry-path entry)))
                       (cond ((or (entry-link-p entry) (equal ".git" (entry-name entry))) nil)
                             ((entry-directory-p entry) (walk path))
                             ((not (gethash (key path) seen))
                              (setf (gethash (key path) seen) t)
                              (handler-case
                                  (progn
                                    (with-open-file (stream path :element-type '(unsigned-byte 8))
                                      (when (> (file-length stream) (* 64 1024 1024))
                                        (error "File exceeds search limit.")))
                                    (let ((buffer (xeon.core:open-buffer-file path)))
                                      (if (xeon.core:find-search-match
                                           (xeon.core:make-search-session buffer
                                             (xeon.core:make-search-query (string #\Null))))
                                          (incf skipped) (scan buffer path nil 0))))
                                (xeon.core:search-cancelled (c) (error c))
                                (error () (incf skipped)))))))
                 (xeon.core:search-cancelled (c) (error c))
                 (error () (incf skipped)))))
      (dolist (pair snapshots)
        (let ((path (xeon.core:buffer-pathname (second pair))))
          (when path (setf (gethash (key path) seen) t))
          (scan (second pair) path (first pair) (third pair))))
      (when project (walk (project-root project))))
    (make-workspace-report :hits (nreverse hits) :skipped skipped :truncated-p truncated)))

(defstruct (workspace-search-job (:constructor %workspace-job))
  thread cancel done report error
  #+sb-thread (lock (sb-thread:make-mutex :name "workspace search")))
(defmacro with-search-job-lock ((job) &body body)
  #+sb-thread `(sb-thread:with-mutex ((workspace-search-job-lock ,job)) ,@body)
  #-sb-thread `(progn ,@body))
(defun cancel-workspace-search (job)
  (with-search-job-lock (job) (setf (workspace-search-job-cancel job) t)))
(defun workspace-search-status (job)
  (with-search-job-lock (job)
    (values (workspace-search-job-done job) (workspace-search-job-report job) (workspace-search-job-error job))))
(defun start-workspace-search (project buffers query)
  "Capture immutable text snapshots on the caller's thread, then search off-GUI."
  (let ((job (%workspace-job))
        (snapshots (mapcar (lambda (buffer) (list buffer (xeon.core:copy-buffer buffer) (xeon.core:buffer-revision buffer))) buffers)))
    (labels ((run ()
               (let ((report nil) (failure nil))
                 (handler-case
                     (setf report (search-workspace project snapshots query
                                    :cancelled-p (lambda () (with-search-job-lock (job) (workspace-search-job-cancel job)))))
                   (xeon.core:search-cancelled () nil)
                   (error (c) (setf failure (princ-to-string c))))
                 (with-search-job-lock (job)
                   (setf (workspace-search-job-report job) report (workspace-search-job-error job) failure
                         (workspace-search-job-done job) t)))))
      #+sb-thread (setf (workspace-search-job-thread job) (sb-thread:make-thread #'run :name "Xeon workspace search"))
      #-sb-thread (run))
    job))
