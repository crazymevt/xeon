(in-package #:xeon.folders)

(defstruct git-change path original index worktree)
(defstruct git-status branch changes error (badges (make-hash-table :test #'equal))
  ;; From the branch line: the local branch (NIL when detached), its upstream, how far
  ;; it is ahead of and behind it, whether the upstream was deleted, and whether the
  ;; branch has no commits yet.
  head upstream (ahead 0) (behind 0) gone-p detached-p unborn-p)

(defun parse-git-branch-line (line status)
  "Fills STATUS from a porcelain v1 branch line (after \"## \"): \"main\",
\"main...origin/main [ahead 1, behind 2]\", \"No commits yet on main\" or
\"HEAD (no branch)\". Ref names cannot hold spaces or \"..\", so splitting is safe."
  (let ((bracket (search " [" line)))
    (when bracket
      (dolist (part (uiop:split-string (subseq line (+ 2 bracket) (or (position #\] line :from-end t) (length line)))
                                       :separator ","))
        (let ((part (string-trim " " part)))
          (flet ((count-after (prefix)
                   (and (uiop:string-prefix-p prefix part)
                        (or (parse-integer part :start (length prefix) :junk-allowed t) 0))))
            (cond ((count-after "ahead ") (setf (git-status-ahead status) (count-after "ahead ")))
                  ((count-after "behind ") (setf (git-status-behind status) (count-after "behind ")))
                  ((string= part "gone") (setf (git-status-gone-p status) t))))))
      (setf line (subseq line 0 bracket))))
  (if (or (string= line "HEAD (no branch)") (uiop:string-prefix-p "HEAD (" line))
      (setf (git-status-detached-p status) t)
      (progn
        (dolist (prefix '("No commits yet on " "Initial commit on "))
          (when (uiop:string-prefix-p prefix line)
            (setf (git-status-unborn-p status) t line (subseq line (length prefix)))))
        (let ((dots (search "..." line)))
          (setf (git-status-head status) (subseq line 0 dots)
                (git-status-upstream status) (and dots (subseq line (+ 3 dots)))))))
  status)

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
      (when branch (parse-git-branch-line branch status))
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

(define-condition git-failed (error)
  ((action :initarg :action :reader git-failed-action)
   (output :initarg :output :reader git-failed-output
           :documentation "What Git wrote, its errors first, trimmed."))
  (:report (lambda (condition stream)
             (format stream "Git could not ~A~@[: ~A~]" (git-action-phrase (git-failed-action condition))
                     (git-failure-summary (git-failed-output condition))))))

(defun git-action-phrase (action)
  (case action
    (:stage "stage the file") (:unstage "unstage the file") (:fetch "fetch")
    (:pull "pull") (:push "push") (:publish "publish the branch") (:switch "switch branches")
    (:create-branch "create the branch") (:commit "commit") (:init "create a repository")
    (t (string-downcase action))))

(defun git-failure-summary (output)
  "The line of Git's output that says what went wrong: its first error, fatal or
rejection, else its first line."
  (let ((lines (remove "" (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                                  (uiop:split-string output :separator '(#\Newline)))
                       :test #'string=)))
    (flet ((strip (line)
             (dolist (prefix '("fatal: " "error: ") line)
               (when (uiop:string-prefix-p prefix line)
                 (return (subseq line (length prefix)))))))
      (let ((line (or (find-if (lambda (line) (or (uiop:string-prefix-p "fatal: " line)
                                                  (uiop:string-prefix-p "error: " line)))
                               lines)
                      (find-if (lambda (line) (search "[rejected]" line)) lines)
                      (first lines))))
        (and line (strip line))))))

(defun git-name-argument-p (name)
  (and (stringp name) (plusp (length name)) (char/= (char name 0) #\-)))

(defun valid-git-branch-name-p (project name)
  "True when NAME can name a new local branch, as git check-ref-format --branch says."
  (and (git-name-argument-p name)
       (zerop (nth-value 2 (git-command (project-root project) (list "check-ref-format" "--branch" name))))))

(defun perform-git-action (project action paths &key message branch remote)
  "Explicit Git operations; never discard local changes or force a push."
  (when (eq action :init)
    (multiple-value-bind (output errors code) (git-command (project-root project) '("init" "--quiet"))
      (unless (zerop code)
        (error 'git-failed :action action :output (string-trim '(#\Newline #\Space) (concatenate 'string errors output))))
      (return-from perform-git-action output)))
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
            ;; Pushes the current branch to REMOTE under the same name and tracks it.
            (:publish (unless (git-name-argument-p remote) (error "Choose a remote to publish to."))
                      (unless (git-name-argument-p branch) (error "Switch to a branch to publish it."))
                      (list "push" "--set-upstream" remote (concatenate 'string "HEAD:refs/heads/" branch)))
            (:switch (unless (git-name-argument-p branch)
                       (error "Enter a local branch name."))
                     (list "switch" "--" branch))
            (:create-branch (unless (valid-git-branch-name-p project branch)
                              (error "~S is not a valid branch name." (or branch "")))
                            (list "switch" "--create" branch))
            (:commit (unless (and message (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) message))))
                       (error "Enter a commit message."))
                     (list "commit" "-m" message)))))
    (multiple-value-bind (output errors code) (git-command (project-root project) arguments)
      (unless (zerop code)
        (error 'git-failed :action action
                           :output (string-trim '(#\Newline #\Space) (format nil "~A~%~A" errors output))))
      output)))

(defun read-git-head-commit (project)
  "The commit HEAD is on, as a plist :HASH (abbreviated) :SUBJECT :DATE (relative, as
\"2 hours ago\"), or NIL before the first commit."
  (multiple-value-bind (output errors code)
      (git-command (project-root project) '("log" "-1" "--no-color" "--format=%h%x00%s%x00%cr" "HEAD" "--"))
    (declare (ignore errors))
    (when (zerop code)
      (let ((fields (uiop:split-string (string-right-trim '(#\Newline #\Return) output) :separator '(#\Null))))
        (when (= 3 (length fields))
          (list :hash (first fields) :subject (second fields) :date (third fields)))))))

(defun read-git-branches (project)
  "The local branch names, most recently committed to first."
  (multiple-value-bind (output errors code)
      (git-command (project-root project)
                   '("for-each-ref" "--sort=-committerdate" "--format=%(refname:short)" "refs/heads/"))
    (unless (zerop code) (error "Cannot list Git branches: ~A" errors))
    (remove "" (uiop:split-string output :separator '(#\Newline #\Return)) :test #'string=)))

(defun read-git-remotes (project)
  "The repository's remote names."
  (multiple-value-bind (output errors code) (git-command (project-root project) '("remote"))
    (unless (zerop code) (error "Cannot list Git remotes: ~A" errors))
    (remove "" (uiop:split-string output :separator '(#\Newline #\Return)) :test #'string=)))

(defun git-all-paths (project action)
  "Changed paths in the opened folder only, including both sides of staged renames.
Conflicted files are left out: staging one marks it resolved, which should be done for
each file once its conflict is."
  (let ((status (read-git-status project)))
    (when (git-status-error status) (error "~A" (git-status-error status)))
    (remove-duplicates
     (loop for change in (git-status-changes status)
           when (and (not (git-change-conflict-p change))
                     (if (eq action :unstage-all)
                         (not (find (git-change-index change) " ?!"))
                         (or (char= (git-change-index change) #\?)
                             (not (find (git-change-worktree change) " ?!")))))
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
