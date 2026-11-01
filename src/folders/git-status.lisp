(in-package #:xeon.folders)

(defstruct git-change path original index worktree)
(defstruct git-status branch changes error (badges (make-hash-table :test #'equal)))

(defun parse-git-status (output &optional (prefix ""))
  "Parse porcelain v1 -z. Rename source follows destination; never split on whitespace."
  (let ((records (uiop:split-string output :separator '(#\Null))) (changes nil) branch)
    (loop while records for record = (pop records) do
      (cond ((uiop:string-prefix-p "## " record) (setf branch (subseq record 3)))
            ((>= (length record) 4)
             (let* ((index (char record 0)) (worktree (char record 1))
                    (original (when (or (find index "RC") (find worktree "RC"))
                                (or (pop records) (error "Missing Git rename source.")))))
               (let ((path (subseq record 3)))
                 (unless (uiop:string-prefix-p prefix path) (error "Git returned a path outside the opened folder."))
                 (setf path (subseq path (length prefix)))
                 (when original
                   (setf original (if (uiop:string-prefix-p prefix original) (subseq original (length prefix))
                                      ;; Do not silently unstage half a cross-folder rename.
                                      (concatenate 'string "../" original))))
               (push (make-git-change :path path :original original
                                      :index index :worktree worktree) changes))))))
    (let ((status (make-git-status :branch branch :changes (nreverse changes))))
      (dolist (change (git-status-changes status))
        (let ((path (git-change-path change)))
          (setf (gethash path (git-status-badges status)) (git-change-badge change))
          (loop for index from 0 below (length path) when (char= #\/ (char path index)) do
            (setf (gethash (subseq path 0 (1+ index)) (git-status-badges status)) "•"))))
      status)))

(defun read-git-status (project)
  "Read one repository snapshot off the GUI thread. Paths are relative to PROJECT."
  (multiple-value-bind (output errors code)
      (git-command (project-root project)
                   '("-c" "status.relativePaths=true" "status" "--porcelain=v1" "-z" "--branch" "--untracked-files=all" "."))
    (if (zerop code)
        (multiple-value-bind (prefix errors code) (git-command (project-root project) '("rev-parse" "--show-prefix"))
          (unless (zerop code) (error "Cannot determine Git folder prefix: ~A" errors))
          (parse-git-status output (string-right-trim '(#\Newline #\Return) prefix)))
        (make-git-status :error (if (project-git-p project) errors
                                   "No Git repository in this folder (or Git is unavailable).")))))

(defun git-change-conflict-p (change)
  (or (find (git-change-index change) "U") (find (git-change-worktree change) "U")
      (member (coerce (list (git-change-index change) (git-change-worktree change)) 'string)
              '("AA" "DD") :test #'equal)))

(defun git-change-badge (change)
  (cond ((git-change-conflict-p change) "!")
        ((char= #\? (git-change-index change)) "U")
        ((char/= #\Space (git-change-worktree change)) (string (git-change-worktree change)))
        (t (string (git-change-index change)))))

(defun git-path-badge (status path &key directory-p)
  "Return a file badge, or a changed-descendant dot for a directory."
  (when (and status (not (git-status-error status)))
    (gethash (if (and directory-p (not (uiop:string-suffix-p path "/")))
                 (concatenate 'string path "/") path)
             (git-status-badges status))))

(defun perform-git-action (project action paths &key message branch)
  "Explicit Git operations; never discard local changes or force a push."
  (unless (project-git-p project) (error "Open a Git working tree first."))
  (dolist (path paths)
    (unless (and (stringp path) (plusp (length path))
                 (not (uiop:absolute-pathname-p (pathname path)))
                 (not (member ".." (uiop:split-string path :separator '(#\/ #\\)) :test #'equal)))
      (error "Git path must be relative to the opened folder: ~S" path)))
  (let ((arguments
          (ecase action
            (:stage (unless paths (error "Choose a file to stage."))
                    (append '("--literal-pathspecs" "add" "--") paths))
            (:unstage (unless paths (error "Choose a staged file."))
                      ;; reset HEAD is unavailable before the first commit.
                      (multiple-value-bind (out err code) (git-command (project-root project) '("rev-parse" "--verify" "HEAD"))
                        (declare (ignore out err))
                        (append (if (zerop code) '("--literal-pathspecs" "reset" "-q" "HEAD" "--")
                                    '("--literal-pathspecs" "rm" "--cached" "-q" "--")) paths)))
            (:fetch '("fetch"))
            (:pull '("pull" "--ff-only"))
            (:push '("push"))
            (:switch (unless (and branch (plusp (length branch)) (char/= (char branch 0) #\-))
                       (error "Enter a local branch name."))
                     (list "switch" "--" branch))
            (:commit (unless (and message (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) message))))
                       (error "Enter a commit message."))
                     (list "commit" "-m" message)))))
    (multiple-value-bind (output errors code) (git-command (project-root project) arguments)
      (unless (zerop code) (error "Git ~A failed: ~A~A" action errors output))
      output)))

(defun git-all-paths (project action)
  "Changed paths in the opened folder only, including both sides of staged renames."
  (let ((status (read-git-status project)))
    (when (git-status-error status) (error "~A" (git-status-error status)))
    (remove-duplicates
     (loop for change in (git-status-changes status)
           when (if (eq action :unstage-all)
                    (not (find (git-change-index change) " ?!"))
                    (or (char= (git-change-index change) #\?)
                        (not (find (git-change-worktree change) " ?!"))))
             append (remove nil (list (git-change-path change)
                                      (when (eq action :unstage-all) (git-change-original change)))))
     :test #'equal)))

(defun read-git-diff (project change &key staged-p)
  "Read a patch without external diff drivers or text converters on the folder worker."
  (let* ((path (git-change-path change))
         (untracked (char= (git-change-index change) #\?)))
    (multiple-value-bind (output errors code)
        (git-command (project-root project)
                     (if untracked
                         ;; Git's null device spelling is portable, including Git for Windows.
                         (list "diff" "--no-index" "--no-color" "--no-ext-diff" "--no-textconv" "--" "/dev/null" path)
                         (append '("--literal-pathspecs" "diff" "--no-color" "--no-ext-diff" "--no-textconv")
                                 (when staged-p '("--cached"))
                                 (list "--" path)
                                 (when (git-change-original change) (list (git-change-original change))))))
      (unless (or (zerop code) (and untracked (= code 1)))
        (error "Git diff failed: ~A~A" errors output))
      (if (plusp (length output)) output "No textual differences (the file may have changed since the last refresh)."))))
