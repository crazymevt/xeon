(in-package #:xeon.folders)

;; OS/process/implementation adapters. GUI objects never enter this package.
(defmacro with-folder-trace ((stage detail) &body body)
  `(if (equal "1" (uiop:getenv "XEON_TRACE_EXPLORER"))
       (let ((start (get-internal-real-time)))
         (unwind-protect (progn ,@body)
           (format *error-output* "~&[explorer] ~A ~A ~,2F ms~%" ,stage ,detail
                   (* 1000.0 (/ (- (get-internal-real-time) start)
                                internal-time-units-per-second)))
           (finish-output *error-output*)))
       (progn ,@body)))

#+sbcl
(defun git-environment ()
  "The process environment for Git. Xeon has no terminal to answer a credential prompt,
so Git fails instead of waiting for one; and reading status takes no optional index
lock, so it never makes the user's own Git commands fail."
  (append '("GIT_TERMINAL_PROMPT=0" "GIT_OPTIONAL_LOCKS=0")
          (remove-if (lambda (entry)
                       (or (uiop:string-prefix-p "GIT_TERMINAL_PROMPT=" entry)
                           (uiop:string-prefix-p "GIT_OPTIONAL_LOCKS=" entry)))
                     (sb-ext:posix-environ))))

(defun run-folder-git (directory arguments input)
  ;; File-backed I/O avoids SBCL's descriptor-copy/event wait path. In the
  ;; native GUI that path can sleep for its full ten-second timeout after Git
  ;; exits. Poll only while this worker owns a running child, never at idle.
  (uiop:with-temporary-file (:pathname stdin :stream in :external-format :utf-8)
    (when input (write-string input in))
    (close in)
    (uiop:with-temporary-file (:pathname stdout)
      (uiop:with-temporary-file (:pathname stderr)
        (let ((process (uiop:launch-program
                        (append (list "git" "-C" (uiop:native-namestring directory)) arguments)
                        :input stdin :output stdout :error-output stderr
                        :if-output-exists :supersede :if-error-output-exists :supersede
                        #+sbcl :environment #+sbcl (git-environment))))
          (unwind-protect
               (progn
                 (loop while (uiop:process-alive-p process) do (sleep .005))
                 (let ((code (uiop:wait-process process)))
                   (values (uiop:read-file-string stdout :external-format :utf-8)
                           (uiop:read-file-string stderr :external-format :utf-8)
                           code)))
            (when (uiop:process-alive-p process)
              (uiop:terminate-process process :urgent t))
            (uiop:wait-process process)))))))

(defun git-command (directory arguments &optional input)
 (with-folder-trace ("git" arguments)
  (handler-case
      (run-folder-git directory arguments input)
    (error (condition) (values "" (princ-to-string condition) -1)))))

(defun symbolic-entry-p (path)
  #+(and sbcl (not win32))
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat (string-right-trim "/" (uiop:native-namestring path)))))
  #+(and sbcl win32)
  (let ((units (xeon.core::windows-path-units path)))
    (sb-sys:with-pinned-objects (units)
      (let ((attributes (xeon.core::%file-attributes-wide
                         (sb-alien:sap-alien (sb-sys:vector-sap units) (* sb-alien:unsigned-short)))))
        (when (= attributes #xffffffff) (error "Cannot inspect directory entry ~A." path))
        (logbitp 10 attributes))))
  #-sbcl (not (equalp path (truename path))))

(defun directory-entries (directory)
  (with-folder-trace ("directory" directory)
    (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* directory))))

(defun folder-files (directory &key recursive)
  "List files without following symbolic links or descending into hidden folders."
  (let ((files nil))
    (labels ((visit (directory)
               (dolist (path (directory-entries directory))
                 (unless (symbolic-entry-p path)
                   (if (uiop:directory-pathname-p path)
                       (let ((name (car (last (pathname-directory path)))))
                         (when (and recursive (stringp name)
                                    (not (and (plusp (length name)) (char= #\. (char name 0)))))
                           (visit path)))
                       (push path files))))))
      (visit directory))
    (sort files #'string-lessp :key #'namestring)))

(defun git-entry-name (path)
  #+win32 (substitute #\/ #\\ (uiop:native-namestring path))
  #-win32 (uiop:native-namestring path))

(defun folder-path-equal-p (first second)
  #+win32 (equalp first second)
  #-win32 (equal first second))

(defun repository-marker-p (directory)
  (loop for current = directory then (make-pathname :directory (butlast (pathname-directory current)) :defaults current)
        when (ignore-errors (probe-file (merge-pathnames ".git" current))) return t
        when (<= (length (pathname-directory current)) 1) return nil))
