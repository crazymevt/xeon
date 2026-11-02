(in-package #:xeon.ui)

;;; The Source Control view: the open folder's Git changes, grouped into merge
;;; conflicts, staged changes and changes; a commit message; and the branch, with
;;; how far it is ahead of and behind its upstream.
;;;
;;; Status is read on a worker thread when the view is shown, the window becomes
;;; active, a document is saved, the folder changes or a Git operation ends; never on
;;; a timer. Requests made while a read runs fold into one more read after it. Git
;;; operations run one at a time on a worker thread too, so a slow push or a commit
;;; hook never holds up the window. Diffs open as read-only tabs that stay current.
;;; Nothing here discards local changes or forces a push.

(defparameter *change-groups*
  '((:conflicts "Merge Conflicts") (:staged "Staged Changes") (:changes "Changes")))

(defparameter *branch-menu-limit* 15
  "How many recent branches the branch menu lists; the rest are reached by name.")

(defclass source-control ()
  ((workbench :initarg :workbench :reader scm-workbench)
   (root :initform nil :accessor scm-root)
   ;; The last status read, or NIL when the folder is not a Git working tree.
   (status :initform nil :accessor scm-status)
   (error :initform nil :accessor scm-error)
   (head :initform nil :accessor scm-head)
   (branches :initform '() :accessor scm-branches)
   (remotes :initform '() :accessor scm-remotes)
   (groups :initform '() :accessor scm-groups)
   ;; :stale until the first read ends, then :loaded.
   (state :initform :stale :accessor scm-state)
   (reading :initform nil :accessor scm-reading)
   (again :initform nil :accessor scm-again)
   (generation :initform 0 :accessor scm-generation)
   ;; What the running Git operation is doing, as "Pushing to origin/main…", or NIL.
   (job :initform nil :accessor scm-job)
   ;; What the tree shows, so an unchanged status leaves it alone.
   (signature :initform :none :accessor scm-signature)
   (opened-groups :initform '() :accessor scm-opened-groups)
   ;; Open diff tabs, as (key . document) with key (path staged-p); the preview tab
   ;; is the one a single click reuses.
   (diffs :initform '() :accessor scm-diffs)
   (preview :initform nil :accessor scm-preview)
   (diff-generation :initform 0 :accessor scm-diff-generation)
   (diff-reading :initform nil :accessor scm-diff-reading)
   (widgets :initform (make-hash-table) :reader scm-widgets)))

(defstruct (scm-row (:constructor %make-scm-row))
  kind group change children)

;;; Rows

(defun path-leaf (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun path-folder (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 slash) "")))

(defun change-staged-p (change)
  (and (not (xeon.folders:git-change-conflict-p change))
       (not (find (xeon.folders:git-change-index change) " ?"))))

(defun change-unstaged-p (change)
  (and (not (xeon.folders:git-change-conflict-p change))
       (or (char= #\? (xeon.folders:git-change-index change))
           (char/= #\Space (xeon.folders:git-change-worktree change)))))

(defun change-rows (status)
  "The groups of STATUS's changes that have any, in *CHANGE-GROUPS* order. A file
staged and then changed again is in both Staged Changes and Changes."
  (let ((members (list (list :conflicts) (list :staged) (list :changes))))
    (dolist (change (xeon.folders:git-status-changes status))
      (cond ((xeon.folders:git-change-conflict-p change) (push change (cdr (assoc :conflicts members))))
            (t (when (change-staged-p change) (push change (cdr (assoc :staged members))))
               (when (change-unstaged-p change) (push change (cdr (assoc :changes members)))))))
    (loop for (group . changes) in members
          when changes
            collect (%make-scm-row
                     :kind :group :group group
                     :children (mapcar (lambda (change) (%make-scm-row :kind :change :group group :change change))
                                       (sort changes #'string-lessp :key #'xeon.folders:git-change-path))))))

(defun group-row-p (row) (eq :group (scm-row-kind row)))

(defun row-path (row) (xeon.folders:git-change-path (scm-row-change row)))

(defun row-key (row)
  (if (group-row-p row)
      (list :group (scm-row-group row))
      (list (scm-row-group row) (row-path row))))

(defun row-name (row)
  (if (group-row-p row)
      (second (assoc (scm-row-group row) *change-groups*))
      (path-leaf (row-path row))))

(defun row-folder (row)
  "The folder of a changed file, and for a rename where it came from."
  (if (group-row-p row)
      ""
      (let* ((change (scm-row-change row))
             (folder (path-folder (row-path row)))
             (original (xeon.folders:git-change-original change)))
        (if original
            (format nil "~@[~A ~]← ~A" (and (plusp (length folder)) folder)
                    (if (equal (path-folder original) folder) (path-leaf original) original))
            folder))))

(defun row-count (row)
  (if (group-row-p row) (princ-to-string (length (scm-row-children row))) ""))

(defun change-letter (change group)
  "The one-letter status shown for CHANGE in GROUP: the index side for staged changes,
the working tree side for the others."
  (case group
    (:conflicts "!")
    (:staged (string (xeon.folders:git-change-index change)))
    (t (if (char= #\? (xeon.folders:git-change-index change))
           "U"
           (string (xeon.folders:git-change-worktree change))))))

(defun letter-role (letter)
  (cond ((find letter '("A" "U") :test #'string=) :inserted)
        ((find letter '("D" "!") :test #'string=) :deleted)
        ((find letter '("R" "C") :test #'string=) :renamed)
        (t :modified)))

(defun letter-description (letter)
  (or (second (assoc letter '(("M" "Modified") ("A" "Added") ("D" "Deleted") ("R" "Renamed")
                              ("C" "Copied") ("U" "Untracked") ("T" "Type changed") ("!" "Conflicted"))
                     :test #'string=))
      "Changed"))

(defun change-row-icon (scm row)
  (unless (group-row-p row)
    (file-icon (workbench-palette (scm-workbench scm))
               (xeon.folders:file-icon-kind (path-leaf (row-path row)) nil))))

(defun row-status-icon (scm row)
  (unless (group-row-p row)
    (let ((letter (change-letter (scm-row-change row) (scm-row-group row))))
      (glyph-image letter :font (make-font :size 11 :weight :bold) :size 16
                          :color (palette-color (workbench-palette (scm-workbench scm)) (letter-role letter))))))

(defun group-changes (scm group)
  (let ((row (find group (scm-groups scm) :key #'scm-row-group)))
    (and row (scm-row-children row))))

(defun changed-file-count (scm)
  (let ((status (scm-status scm)))
    (if status (length (xeon.folders:git-status-changes status)) 0)))

;;; Small widget helpers: set a property only when it changes, so a refresh that
;;; finds nothing new touches nothing native.

(defun set-text (widget text)
  (unless (equal text (text widget)) (setf (text widget) text)))

(defun set-visible (widget value)
  (let ((value (and value t)))
    (unless (eq value (and (visible widget) t)) (setf (visible widget) value))))

(defun set-label (widget value)
  (unless (equal value (label widget))
    (setf (label widget) value)))

(defun set-tooltip (widget value)
  (unless (equal value (tooltip widget)) (setf (tooltip widget) value)))

;;; Reading the repository

(defun refresh-source-control (workbench)
  "Reads the repository's status again on a worker thread. A request made while a read
or a Git operation runs is folded into one read after it."
  (let ((scm (workbench-source-control workbench))
        (root (explorer-folder workbench)))
    (when (and scm root)
      (if (or (scm-reading scm) (scm-job scm))
          (setf (scm-again scm) t)
          (read-repository scm root)))))

(defun diff-text (project status key)
  "The patch for KEY, (path staged-p), against STATUS, read on a worker thread."
  (destructuring-bind (path staged-p) key
    (let ((change (find-if (lambda (change)
                             (and (equal path (xeon.folders:git-change-path change))
                                  (if staged-p
                                      (change-staged-p change)
                                      (or (change-unstaged-p change) (xeon.folders:git-change-conflict-p change)))))
                           (and status (xeon.folders:git-status-changes status)))))
      (if change
          (handler-case (xeon.folders:read-git-diff project change :staged-p staged-p)
            (error (condition) (princ-to-string condition)))
          (format nil "No ~:[unstaged~;staged~] changes to ~A.~%" staged-p path)))))

(defun read-repository (scm root)
  (let ((generation (incf (scm-generation scm)))
        (keys (mapcar #'car (live-diffs scm)))
        (owner (part scm :tree))
        (backend *backend*))
    (setf (scm-reading scm) t (scm-again scm) nil (scm-root scm) root)
    (sb-thread:make-thread
     (lambda ()
       (let ((*backend* backend) status failure head branches remotes diffs)
         (handler-case
             (let ((project (xeon.folders:open-folder-project root)))
               (setf status (xeon.folders:read-git-status project)
                     failure (xeon.folders:git-status-error status))
               (unless failure
                 (setf head (ignore-errors (xeon.folders:read-git-head-commit project))
                       branches (ignore-errors (xeon.folders:read-git-branches project))
                       remotes (ignore-errors (xeon.folders:read-git-remotes project))
                       diffs (loop for key in keys collect (cons key (diff-text project status key))))))
           (error (condition) (setf failure (princ-to-string condition) status nil)))
         (call-in-ui-thread
          (lambda ()
            (when (= generation (scm-generation scm))
              (setf (scm-reading scm) nil (scm-state scm) :loaded)
              (apply-repository scm (and (not failure) status) failure head branches remotes)
              (loop for (key . text) in diffs
                    for d = (cdr (assoc key (live-diffs scm) :test #'equal))
                    when d do (set-diff-text d text))
              (when (scm-again scm) (refresh-source-control (scm-workbench scm)))))
          :owner owner)))
     :name "Xeon Git status")))

(defun apply-repository (scm status failure head branches remotes)
  (setf (scm-status scm) status (scm-error scm) failure (scm-head scm) head
        (scm-branches scm) branches (scm-remotes scm) remotes
        (scm-groups scm) (and status (change-rows status)))
  (update-explorer-git-status (scm-workbench scm) status)
  (refresh-change-tree scm)
  (refresh-scm-view scm)
  (refresh-branch-status (scm-workbench scm)))

(defun source-control-folder-changed (workbench)
  "The explorer opened another folder: forget this one's state and read the new one."
  (let ((scm (workbench-source-control workbench)))
    (when scm
      (incf (scm-generation scm))
      (setf (scm-reading scm) nil (scm-again scm) nil (scm-state scm) :stale
            (scm-root scm) (explorer-folder workbench))
      (apply-repository scm nil nil nil '() '())
      (refresh-source-control workbench))))

;;; Showing the state

(defun tree-signature (groups)
  (loop for group in groups
        collect (cons (scm-row-group group)
                      (loop for row in (scm-row-children group)
                            for change = (scm-row-change row)
                            collect (list (xeon.folders:git-change-path change) (xeon.folders:git-change-original change)
                                          (xeon.folders:git-change-index change) (xeon.folders:git-change-worktree change))))))

(defun refresh-change-tree (scm)
  "Gives the tree the groups when they changed. The tree keeps open groups and the
selection by key; a group is opened the first time it appears."
  (let ((tree (part scm :tree))
        (groups (scm-groups scm))
        (signature (tree-signature (scm-groups scm))))
    (unless (equal signature (scm-signature scm))
      (setf (scm-signature scm) signature
            (roots tree) groups)
      (dolist (group groups)
        (unless (member (scm-row-group group) (scm-opened-groups scm))
          (push (scm-row-group group) (scm-opened-groups scm))
          (expand-item tree group))))))

(defun show-scm-body (scm page)
  "Shows the :LOADING, :NO-REPOSITORY or :REPOSITORY page; the spinner turns only while shown."
  (let ((spinner (part scm :spinner)))
    (unless (eq (and (active spinner) t) (eq page :loading))
      (setf (active spinner) (eq page :loading))))
  (let ((body (part scm :body)) (widget (part scm page)))
    (unless (eq widget (selected-page body)) (select-page body widget))))

(defun refresh-scm-view (scm)
  "Brings pages, counts, the commit controls and the branch bar up to date."
  (let ((status (scm-status scm)))
    (cond ((and (eq (scm-state scm) :stale) (null status))
           (show-scm-body scm :loading))
          ((null status)
           (let ((no-repository (or (null (scm-error scm)) (search "No Git repository" (scm-error scm)))))
             (set-text (part scm :empty-title) (if no-repository "No Git repository" "Git is unavailable"))
             (set-text (part scm :empty-text)
                       (if no-repository
                           "This folder is not in a Git working tree. Create a repository to track its changes, or open another folder."
                           (scm-error scm)))
             (set-visible (part scm :initialize) no-repository)
             (set-enabled (part scm :initialize) (and no-repository (not (scm-job scm)))))
           (show-scm-body scm :no-repository))
          (t (show-scm-body scm :repository)
             (let ((changes (part scm :changes))
                   (page (part scm (if (scm-groups scm) :tree-page :clean))))
               (unless (eq page (selected-page changes)) (select-page changes page)))))
    (let ((count (changed-file-count scm)))
      (set-text (part scm :count) (if (plusp count) (princ-to-string count) "")))
    (set-enabled (part scm :refresh) (scm-root scm))
    (set-enabled (part scm :more) status)
    (refresh-commit-controls scm)
    (refresh-branch-bar scm)
    (refresh-rail-tooltip (scm-workbench scm))))

(defun commit-message (scm) (buffer-text (buffer (part scm :message))))

(defun blank-p (string) (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) string))))

(defun head-name (scm)
  (let ((status (scm-status scm)))
    (cond ((null status) nil)
          ((xeon.folders:git-status-detached-p status)
           (format nil "detached at ~A" (or (getf (scm-head scm) :hash) "HEAD")))
          (t (xeon.folders:git-status-head status)))))

(defun refresh-commit-controls (scm)
  (let* ((staged (length (group-changes scm :staged)))
         (conflicts (length (group-changes scm :conflicts)))
         (unstaged (length (group-changes scm :changes)))
         (empty (blank-p (commit-message scm)))
         (shortcut (shortcut-label "Primary+Return")))
    (set-enabled (part scm :commit) (and (plusp staged) (zerop conflicts) (not empty) (not (scm-job scm))))
    (set-tooltip (part scm :commit)
                 (format nil "Commit the staged changes~@[ to ~A~] · ~A" (head-name scm) shortcut))
    (set-text (part scm :commit-hint)
              (cond ((plusp conflicts)
                     (format nil "Resolve ~:[the conflict~;each conflict~], then mark ~:*~:[it~;them~] as resolved to commit."
                             (> conflicts 1)))
                    ((plusp staged)
                     (format nil "~D file~:P staged~:[ · ~A to commit~;~*~]" staged empty shortcut))
                    ((plusp unstaged) "Stage the changes to include in the commit.")
                    (t "Nothing to commit.")))))

(defun publish-remote (scm)
  (let ((remotes (scm-remotes scm)))
    (or (find "origin" remotes :test #'string=) (first remotes))))

(defun sync-plan (scm)
  "What the sync button does, as values: :PUBLISH, :SYNC, :DIVERGED, :FETCH or NIL; its
label; and its tooltip."
  (let* ((status (scm-status scm))
         (branch (and status (xeon.folders:git-status-head status)))
         (upstream (and status (xeon.folders:git-status-upstream status)))
         (ahead (if status (xeon.folders:git-status-ahead status) 0))
         (behind (if status (xeon.folders:git-status-behind status) 0)))
    (cond ((or (null status) (null (scm-remotes scm)) (null branch)
               (xeon.folders:git-status-unborn-p status))
           nil)
          ((or (null upstream) (xeon.folders:git-status-gone-p status))
           (values :publish "Publish"
                   (format nil "~:[~*~;~A was deleted on the remote. ~]Publish ~A to ~A"
                           (and upstream (xeon.folders:git-status-gone-p status)) upstream
                           branch (publish-remote scm))))
          ((and (zerop ahead) (zerop behind))
           (values :fetch "" (format nil "Up to date with ~A · Click to fetch new commits" upstream)))
          (t (values (if (and (plusp ahead) (plusp behind)) :diverged :sync)
                     (format nil "~D↓ ~D↑" behind ahead)
                     (cond ((and (plusp ahead) (plusp behind))
                            (format nil "~A and ~A have diverged: ~D commit~:P to pull, ~D to push"
                                    branch upstream behind ahead))
                           ((plusp behind) (format nil "Pull ~D commit~:P from ~A" behind upstream))
                           (t (format nil "Push ~D commit~:P to ~A" ahead upstream))))))))

(defun refresh-branch-bar (scm)
  (let ((status (scm-status scm))
        (job (scm-job scm))
        (head (scm-head scm)))
    (set-label (part scm :branch) (or (head-name scm) ""))
    (set-tooltip (part scm :branch)
                 (if (and status (xeon.folders:git-status-unborn-p status))
                     (format nil "~A has no commits yet · Click for branches" (head-name scm))
                     "Switch or create a branch"))
    (set-enabled (part scm :branch) (and status (not job)))
    (multiple-value-bind (plan label tooltip) (sync-plan scm)
      (let ((sync (part scm :sync)))
        (set-visible sync plan)
        (when plan
          (set-label sync label)
          (set-tooltip sync tooltip)
          (let ((glyph (if (eq plan :publish) :publish :sync)))
            (unless (eq glyph (user-data sync))
              (setf (user-data sync) glyph (image sync) (icon glyph :size 14)))))
        (set-enabled sync (and plan (not job)))))
    (let ((spinner (part scm :busy)))
      (set-visible spinner job)
      (unless (eq (and (active spinner) t) (and job t)) (setf (active spinner) (and job t))))
    (set-text (part scm :activity)
              (cond (job job)
                    (head (format nil "~A · ~A" (getf head :hash) (shorten (getf head :subject) 34)))
                    ((and status (xeon.folders:git-status-unborn-p status)) "No commits yet")
                    (t "")))
    (set-tooltip (part scm :activity)
                 (and head (not job)
                      (format nil "Last commit ~A, ~A~%~A" (getf head :hash) (getf head :date) (getf head :subject))))))

(defun shorten (text limit)
  (if (> (length text) limit) (concatenate 'string (string-right-trim " " (subseq text 0 (1- limit))) "…") text))

(defun refresh-rail-tooltip (workbench)
  (let ((button (and (slot-boundp workbench 'rail)
                     (find :source-control (rail-buttons workbench) :key #'user-data)))
        (scm (workbench-source-control workbench)))
    (when (and button scm)
      (let ((count (changed-file-count scm)))
        (set-tooltip button (format nil "Source Control~[~:; · ~:*~D change~:P~] · ~A"
                                    count (shortcut-label "Primary+Shift+G")))))))

(defun refresh-branch-status (workbench)
  "The branch in the status bar, shown in a Git working tree."
  (let ((scm (workbench-source-control workbench))
        (status (workbench-status workbench)))
    (when (and scm (slot-boundp status 'branch))
      (let ((box (status-branch status))
            (button (status-branch status))
            (name (head-name scm)))
        (unless name (set-visible box nil))
        (when name
          (let* ((git (scm-status scm))
                 (ahead (xeon.folders:git-status-ahead git))
                 (behind (xeon.folders:git-status-behind git))
                 (count (changed-file-count scm)))
            (set-label button (format nil "~A~:[~; ~D↓ ~D↑~]" name (or (plusp ahead) (plusp behind)) behind ahead))
            (set-tooltip button (format nil "On branch ~A~[ · No changes~:; · ~:*~D changed file~:P~]" name count))
            (set-visible box t)))))))

(defun restyle-source-control (workbench)
  "The theme changed: the message takes the editor font, and status letters and file
icons take their colors from the theme."
  (let ((scm (workbench-source-control workbench)))
    (when scm
      (setf (font (part scm :message)) (editor-font (workbench-themes workbench)))
      (refresh-tree (part scm :tree))
      (let ((status (workbench-status workbench)))
        (when (slot-boundp status 'branch)
          (setf (image (status-branch status))
                (icon :branch :size 13 :color :secondary-text)))))))

;;; Diffs

(defun live-diffs (scm)
  "The open diff tabs, forgetting any closed since."
  (let ((open (workbench-documents (scm-workbench scm))))
    (setf (scm-diffs scm) (remove-if-not (lambda (entry) (member (cdr entry) open)) (scm-diffs scm)))
    (unless (member (scm-preview scm) open) (setf (scm-preview scm) nil))
    (scm-diffs scm)))

(defun row-diff-key (row) (list (row-path row) (eq :staged (scm-row-group row))))

(defun diff-title (key)
  (format nil "~A (~:[Working Tree~;Staged~])" (path-leaf (first key)) (second key)))

(defun diff-description (key)
  (format nil "~A · ~:[Changes in the working tree~;Staged changes~]" (first key) (second key)))

(defun set-diff-text (d text &key reset)
  "Shows TEXT in the read-only diff document D, leaving it unmodified."
  (let ((buffer (document-buffer d)))
    (unless (string= text (xeon.core:buffer-text buffer))
      (setf (xeon.core:buffer-read-only-p buffer) nil)
      (unwind-protect (xeon.core:replace-range buffer 0 (xeon.core:buffer-length buffer) text)
        (setf (xeon.core:buffer-read-only-p buffer) t))
      (xeon.core:mark-buffer-saved buffer)
      (when reset
        (dolist (view (document-views d))
          (set-selection view 0 0)
          (scroll-to-position view 0)))
      (document-changed (document-workbench d) d))))

(defun make-diff-document (workbench key text)
  (let ((scratch (disposable-document workbench))
        (buffer (xeon.core:make-buffer :name (diff-title key) :text text :read-only-p t :history-limit 0)))
    (xeon.syntax:set-buffer-language buffer :diff)
    (xeon.core:add-buffer (workbench-manager workbench) buffer)
    (let ((d (add-document workbench buffer)))
      (setf (document-description d) (diff-description key))
      (update-document-tab workbench d)
      (when scratch (close-document workbench scratch t))
      d)))

(defun show-diff (scm key text pin)
  "Shows TEXT, KEY's diff: in its own tab when one is open; else, unless PIN, in the
preview tab; else in a new tab."
  (let* ((workbench (scm-workbench scm))
         (diffs (live-diffs scm))
         (existing (cdr (assoc key diffs :test #'equal)))
         (preview (scm-preview scm))
         (d (cond (existing (set-diff-text existing text) existing)
                  ((and preview (not pin))
                   (setf (scm-diffs scm) (acons key preview (remove preview diffs :key #'cdr))
                         (xeon.core:buffer-name (document-buffer preview)) (diff-title key)
                         (document-description preview) (diff-description key))
                   (set-diff-text preview text :reset t)
                   (update-document-tab workbench preview)
                   preview)
                  (t (let ((d (make-diff-document workbench key text)))
                       (push (cons key d) (scm-diffs scm))
                       (unless pin (setf (scm-preview scm) d))
                       d)))))
    (when (and pin (eq d (scm-preview scm))) (setf (scm-preview scm) nil))
    (if pin
        (show-document workbench d)
        ;; A preview leaves the keyboard in the list, to go on to the next file.
        (let ((page (find-document-page workbench d)))
          (when page (select-page (workbench-tabs workbench) page))
          (focus (part scm :tree))))
    d))

(defun open-change (scm row &key (pin t))
  "Shows ROW's diff, read on a worker thread. A single click shows it in the preview
tab, which the next single click reuses; opening it outright keeps its tab."
  (let ((key (row-diff-key row))
        (generation (incf (scm-diff-generation scm)))
        (root (scm-root scm))
        (status (scm-status scm))
        (owner (part scm :tree))
        (backend *backend*))
    (when root
      (setf (scm-diff-reading scm) t)
      (sb-thread:make-thread
       (lambda ()
         (let* ((*backend* backend)
                (text (handler-case (diff-text (xeon.folders:open-folder-project root) status key)
                        (error (condition) (princ-to-string condition)))))
           (call-in-ui-thread
            (lambda ()
              (when (= generation (scm-diff-generation scm))
                (setf (scm-diff-reading scm) nil)
                (with-reported-errors ((scm-workbench scm)) (show-diff scm key text pin))))
            :owner owner)))
       :name "Xeon Git diff"))))

(defun row-file (scm row) (merge-pathnames (row-path row) (scm-root scm)))

(defun open-change-file (scm row)
  (let ((file (row-file scm row)))
    (unless (probe-file file) (error "~A was deleted." (row-path row)))
    (open-document (scm-workbench scm) file)))

;;; Git operations

(defun show-git-failure (scm condition)
  "A Git refusal gets an alert with Git's own words; anything else, a status message."
  (let ((workbench (scm-workbench scm)))
    (if (typep condition 'xeon.folders:git-failed)
        (let ((output (xeon.folders:git-failed-output condition)))
          (message-box (princ-to-string condition)
                       :detail (if (> (length output) 3000) (concatenate 'string (subseq output 0 3000) "…") output)
                       :type :warning :parent (workbench-window workbench)))
        (report workbench "~A" condition))))

(defun run-git-job (scm busy function &key done on-success)
  "Runs FUNCTION with the folder's project on a worker thread, showing BUSY meanwhile,
then reads the status again. DONE (a string, or a function of FUNCTION's value) is
reported on success, after ON-SUCCESS is called with that value."
  (let ((workbench (scm-workbench scm)))
    (when (scm-job scm) (error "Wait until Git is done: ~A" (scm-job scm)))
    (let ((root (or (explorer-folder workbench) (error "Open a folder first.")))
          (owner (part scm :tree))
          (backend *backend*))
      (setf (scm-job scm) busy)
      (refresh-scm-view scm)
      (sb-thread:make-thread
       (lambda ()
         (let ((*backend* backend) result failure)
           (handler-case (setf result (funcall function (xeon.folders:open-folder-project root)))
             (error (condition) (setf failure condition)))
           (call-in-ui-thread
            (lambda ()
              (setf (scm-job scm) nil)
              (refresh-source-control workbench)
              (refresh-scm-view scm)
              (if failure
                  (show-git-failure scm failure)
                  (with-reported-errors (workbench)
                    (when on-success (funcall on-success result))
                    (when done (report workbench "~A" (if (functionp done) (funcall done result) done))))))
            :owner owner)))
       :name "Xeon Git"))))

(defun change-paths (row &key unstage)
  "The paths to stage or unstage for ROW: both sides of a staged rename within the folder."
  (let ((original (xeon.folders:git-change-original (scm-row-change row))))
    (if (and unstage original (not (uiop:string-prefix-p "../" original)))
        (list (row-path row) original)
        (list (row-path row)))))

(defun conflict-markers-p (file)
  (ignore-errors
   (with-open-file (in file :external-format :utf-8 :element-type 'character)
     (loop for line = (read-line in nil) while line
           thereis (or (uiop:string-prefix-p "<<<<<<< " line) (uiop:string-prefix-p ">>>>>>> " line))))))

(defun stage-row (scm row)
  "Stages ROW's file, or for a conflict marks it resolved, checking for leftover markers."
  (let ((name (path-leaf (row-path row))))
    (when (and (eq :conflicts (scm-row-group row))
               (conflict-markers-p (row-file scm row))
               (not (confirm (format nil "~A still has conflict markers." name)
                             :detail "Marking it as resolved stages the file as it is, markers included."
                             :yes "Mark as Resolved" :type :warning
                             :parent (workbench-window (scm-workbench scm)))))
      (return-from stage-row nil))
    (run-git-job scm (format nil "Staging ~A…" name)
                 (lambda (project) (xeon.folders:perform-git-action project :stage (change-paths row)))
                 :done (when (eq :conflicts (scm-row-group row)) (format nil "Marked ~A as resolved." name)))))

(defun unstage-row (scm row)
  (run-git-job scm (format nil "Unstaging ~A…" (path-leaf (row-path row)))
               (lambda (project) (xeon.folders:perform-git-action project :unstage (change-paths row :unstage t)))))

(defun toggle-row-staged (scm row)
  (if (eq :staged (scm-row-group row)) (unstage-row scm row) (stage-row scm row)))

(defun selected-change-rows (tree)
  (remove-if #'group-row-p (selected-tree-items tree)))

(defun toggle-selected-staged (scm rows)
  "Stages selected changes, or unstages them when every selected row is staged."
  (if (null (rest rows))
      (toggle-row-staged scm (first rows))
      (let* ((unstage (every (lambda (row) (eq :staged (scm-row-group row))) rows))
             (targets (if unstage rows
                          (remove-if (lambda (row) (member (scm-row-group row) '(:staged :conflicts))) rows)))
             (paths (remove-duplicates (mapcan (lambda (row) (change-paths row :unstage unstage)) targets)
                                       :test #'equal)))
        (when paths
          (run-git-job scm (if unstage "Unstaging selected files…" "Staging selected files…")
                       (lambda (project)
                         (xeon.folders:perform-git-action project (if unstage :unstage :stage) paths)))))))

(defun selected-changes-menu (scm rows)
  (let ((unstage (every (lambda (row) (eq :staged (scm-row-group row))) rows)))
    (make-menu ""
               (scm-item scm (if unstage "Unstage Selected Files" "Stage Selected Files")
                         (lambda () (toggle-selected-staged scm rows))
                         :enabled (and (not (scm-job scm))
                                       (or unstage (some (lambda (row) (eq :changes (scm-row-group row))) rows)))))))

(defun stage-all (scm &key unstage)
  (run-git-job scm (if unstage "Unstaging all changes…" "Staging all changes…")
               (lambda (project)
                 (let ((paths (xeon.folders:git-all-paths project (if unstage :unstage-all :stage-all))))
                   (when paths
                     (xeon.folders:perform-git-action project (if unstage :unstage :stage) paths))))))

(defun clear-commit-message (scm)
  (let ((buffer (buffer (part scm :message))))
    (replace-buffer-range buffer 0 (length (buffer-text buffer)) "")))

(defun commit-changes (scm)
  "Commits the staged changes with the message typed."
  (let ((message (string-trim '(#\Space #\Tab #\Newline #\Return) (commit-message scm)))
        (workbench (scm-workbench scm)))
    (cond ((scm-job scm) (report workbench "Wait until Git is done: ~A" (scm-job scm)))
          ((null (scm-status scm)) (report workbench "Open a Git working tree first."))
          ((group-changes scm :conflicts) (report workbench "Resolve the merge conflicts before committing."))
          ((null (group-changes scm :staged)) (report workbench "Stage the changes to commit first."))
          ((zerop (length message))
           (report workbench "Enter a commit message.")
           (focus (part scm :message)))
          (t (run-git-job scm "Committing…"
                          (lambda (project)
                            (xeon.folders:perform-git-action project :commit nil :message message)
                            (xeon.folders:read-git-head-commit project))
                          :on-success (lambda (head) (declare (ignore head)) (clear-commit-message scm))
                          :done (lambda (head) (format nil "Committed ~A · ~A" (getf head :hash) (getf head :subject))))))))

(defun upstream-name (scm)
  (let ((status (scm-status scm)))
    (or (and status (xeon.folders:git-status-upstream status)) "the remote")))

(defun pull-changes (scm)
  (run-git-job scm (format nil "Pulling from ~A…" (upstream-name scm))
               (lambda (project) (xeon.folders:perform-git-action project :pull nil))
               :on-success (lambda (result) (declare (ignore result)) (refresh-explorer (scm-workbench scm)))
               :done (format nil "Pulled from ~A." (upstream-name scm))))

(defun push-changes (scm)
  (run-git-job scm (format nil "Pushing to ~A…" (upstream-name scm))
               (lambda (project) (xeon.folders:perform-git-action project :push nil))
               :done (format nil "Pushed to ~A." (upstream-name scm))))

(defun fetch-changes (scm)
  (run-git-job scm "Fetching…"
               (lambda (project) (xeon.folders:perform-git-action project :fetch nil))
               :done "Fetched the latest commits."))

(defun publish-branch (scm)
  (let ((branch (head-name scm)) (remote (publish-remote scm)))
    (unless remote (error "Add a remote to publish ~A to." branch))
    (run-git-job scm (format nil "Publishing ~A to ~A…" branch remote)
                 (lambda (project) (xeon.folders:perform-git-action project :publish nil :branch branch :remote remote))
                 :done (format nil "Published ~A to ~A." branch remote))))

(defun sync-repository (scm)
  "Does what the sync button says: publish, pull and push, or fetch."
  (let* ((status (scm-status scm))
         (ahead (and status (xeon.folders:git-status-ahead status)))
         (behind (and status (xeon.folders:git-status-behind status))))
    (case (sync-plan scm)
      (:publish (publish-branch scm))
      (:fetch (fetch-changes scm))
      (:diverged
       (message-box (format nil "~A and ~A have diverged." (head-name scm) (upstream-name scm))
                    :detail (format nil "There ~[are~;is~:;are~] ~:*~D commit~:P to pull and ~D to push. Xeon only pulls when it can fast-forward; merge or rebase in a terminal, then push."
                                    behind ahead)
                    :type :info :parent (workbench-window (scm-workbench scm))))
      (:sync
       (run-git-job scm (format nil "Syncing with ~A…" (upstream-name scm))
                    (lambda (project)
                      (when (plusp behind) (xeon.folders:perform-git-action project :pull nil))
                      (when (plusp ahead) (xeon.folders:perform-git-action project :push nil)))
                    :on-success (lambda (result) (declare (ignore result))
                                  (when (plusp behind) (refresh-explorer (scm-workbench scm))))
                    :done (format nil "Synced with ~A." (upstream-name scm)))))))

(defun switch-branch (scm branch)
  (run-git-job scm (format nil "Switching to ~A…" branch)
               (lambda (project) (xeon.folders:perform-git-action project :switch nil :branch branch))
               :on-success (lambda (result) (declare (ignore result)) (refresh-explorer (scm-workbench scm)))
               :done (format nil "Switched to ~A." branch)))

(defun ask-branch-name (scm message detail ok)
  (let ((name (prompt-text message :detail detail :ok ok
                                   :parent (workbench-window (scm-workbench scm)))))
    (and name (not (blank-p name)) (string-trim " " name))))

(defun create-branch (scm)
  (let* ((hash (getf (scm-head scm) :hash))
         (name (ask-branch-name scm "Create a branch"
                                (format nil "The new branch starts at ~:[the current commit~;~:*~A~] and takes your uncommitted changes with it."
                                        hash)
                                "Create Branch")))
    (when name
      (run-git-job scm (format nil "Creating ~A…" name)
                   (lambda (project) (xeon.folders:perform-git-action project :create-branch nil :branch name))
                   :done (format nil "Created ~A and switched to it." name)))))

(defun switch-to-named-branch (scm)
  (let ((name (ask-branch-name scm "Switch to a branch" "Enter the name of a local branch." "Switch")))
    (when name
      (unless (member name (scm-branches scm) :test #'string=)
        (error "There is no local branch named ~A." name))
      (switch-branch scm name))))

(defun initialize-repository (scm)
  (let ((workbench (scm-workbench scm)))
    (run-git-job scm "Creating a repository…"
                 (lambda (project) (xeon.folders:perform-git-action project :init nil))
                 :on-success (lambda (result)
                               (declare (ignore result))
                               ;; The explorer filters with Git ignore rules from now on.
                               (let ((explorer (workbench-explorer workbench)))
                                 (setf (explorer-project explorer)
                                       (xeon.folders:open-folder-project (explorer-folder workbench)))
                                 (refresh-explorer workbench)))
                 :done "Created a Git repository.")))

;;; Menus

(defun make-menu (title &rest items)
  (let ((menu (make-instance 'menu :title title)))
    (setf (menu-items menu) (remove nil items))
    menu))

(defun scm-item (scm label function &key (enabled t))
  (make-instance 'menu-item :label label :enabled (and enabled t)
                            :on-activate (action ((scm-workbench scm)) (funcall function))))

(defun separator-item () (make-instance 'menu-separator))

(defun branch-menu (scm &optional (title ""))
  (let* ((current (let ((status (scm-status scm))) (and status (xeon.folders:git-status-head status))))
         (idle (and (scm-status scm) (not (scm-job scm))))
         (branches (scm-branches scm))
         (shown (subseq branches 0 (min *branch-menu-limit* (length branches)))))
    (apply #'make-menu title
           (scm-item scm "New Branch…" (lambda () (create-branch scm)) :enabled idle)
           (when (> (length branches) *branch-menu-limit*)
             (scm-item scm "Switch to Branch…" (lambda () (switch-to-named-branch scm)) :enabled idle))
           (when shown (separator-item))
           (mapcar (lambda (branch)
                     (make-instance 'check-menu-item
                                    :label branch :checked (equal branch current) :enabled idle
                                    :on-activate (action ((scm-workbench scm))
                                                   (unless (equal branch current) (switch-branch scm branch)))))
                   shown))))

(defun more-menu (scm)
  (let* ((status (scm-status scm))
         (idle (and status (not (scm-job scm))))
         (upstream (and status (xeon.folders:git-status-upstream status)
                        (not (xeon.folders:git-status-gone-p status)))))
    (make-menu ""
               (scm-item scm "Commit" (lambda () (commit-changes scm)) :enabled (enabled (part scm :commit)))
               (separator-item)
               (scm-item scm "Pull" (lambda () (pull-changes scm)) :enabled (and idle upstream))
               (scm-item scm "Push" (lambda () (push-changes scm)) :enabled (and idle upstream))
               (scm-item scm "Fetch" (lambda () (fetch-changes scm)) :enabled (and idle (scm-remotes scm)))
               (when (eq :publish (sync-plan scm))
                 (scm-item scm "Publish Branch" (lambda () (publish-branch scm)) :enabled idle))
               (separator-item)
               (scm-item scm "Stage All Changes" (lambda () (stage-all scm))
                         :enabled (and idle (group-changes scm :changes)))
               (scm-item scm "Unstage All Changes" (lambda () (stage-all scm :unstage t))
                         :enabled (and idle (group-changes scm :staged)))
               (separator-item)
               (branch-menu scm "Branch"))))

(defun change-menu (scm row)
  (let* ((group (scm-row-group row))
         (file (row-file scm row))
         (exists (probe-file file))
         (idle (not (scm-job scm))))
    (make-menu ""
               (scm-item scm (if (eq group :staged) "Open Staged Changes" "Open Changes")
                         (lambda () (open-change scm row)))
               (scm-item scm "Open File" (lambda () (open-change-file scm row)) :enabled exists)
               (separator-item)
               (case group
                 (:staged (scm-item scm "Unstage" (lambda () (unstage-row scm row)) :enabled idle))
                 (:conflicts (scm-item scm "Mark as Resolved" (lambda () (stage-row scm row)) :enabled idle))
                 (t (scm-item scm "Stage" (lambda () (stage-row scm row)) :enabled idle)))
               (separator-item)
               (scm-item scm "Reveal in Finder" (lambda () (open-url (uiop:pathname-directory-pathname file)))
                         :enabled exists)
               (scm-item scm "Copy Path" (lambda () (setf (clipboard-text) (uiop:native-namestring file))))
               (scm-item scm "Copy Relative Path" (lambda () (setf (clipboard-text) (row-path row)))))))

(defun group-menu (scm row)
  (let ((idle (not (scm-job scm))))
    (case (scm-row-group row)
      (:changes (make-menu "" (scm-item scm "Stage All Changes" (lambda () (stage-all scm)) :enabled idle)))
      (:staged (make-menu "" (scm-item scm "Unstage All Changes" (lambda () (stage-all scm :unstage t))
                                       :enabled idle))))))

(defun popup-below (menu widget)
  (popup-menu menu widget 0 (widget-height widget)))

(defun show-branch-menu (workbench widget)
  (let ((scm (workbench-source-control workbench)))
    (when (and scm (scm-status scm))
      (popup-below (branch-menu scm) widget))))

;;; Building

(defclass commit-input (code-view)
  ((source-control :initarg :source-control :reader input-source-control))
  (:default-initargs :line-numbers-p nil :wrap t :vexpand nil :hexpand t)
  (:documentation "The commit message. Primary+Return commits."))

(defmethod measure ((view commit-input)) (values 160 52))

(defmethod handle-event ((view commit-input) (event key-event))
  (when (and (eq (event-signal event) :key-down) (not (event-consumed-p event))
             (eq (event-key event) :return)
             (equal (event-modifiers event) (list (primary-modifier))))
    (let ((scm (input-source-control view)))
      (with-reported-errors ((scm-workbench scm)) (commit-changes scm)))
    (consume event))
  (call-next-method))

(defun scm-icon-button (glyph label tooltip)
  (make-instance 'button :style :icon :label label :tooltip tooltip
                         :image (icon glyph :size 16) :control-size :small))

(defun make-no-repository-page (scm)
  (let* ((workbench (scm-workbench scm))
         (title (make-instance 'label :text "" :font (make-font :size 13 :weight :bold) :alignment :center))
         (text (make-instance 'label :text "" :text-color :secondary-text :wrap t :alignment :center
                                     :font (make-font :size 11)))
         (initialize (small-button "Initialize Repository" :tooltip "Run git init in the open folder"))
         (open (small-button "Open Folder…")))
    (setf (part scm :empty-title) title (part scm :empty-text) text (part scm :initialize) initialize)
    (on initialize :clicked (action (workbench) (initialize-repository scm)))
    (on open :clicked (action (workbench) (choose-and-open-folder workbench)))
    (ui (vbox :padding '(28 20 20 20) :spacing 6
          (hbox (spacer) (:widget (make-instance 'image-view :image (icon :source-control :size 32 :color :secondary-text))) (spacer))
          (:widget title)
          (:widget text)
          (hbox :padding '(6 0 0 0) :spacing 6 (spacer) (:widget initialize) (:widget open) (spacer))
          (spacer)))))

(defun make-scm-loading-page (scm)
  (let ((spinner (make-instance 'spinner :active nil)))
    (setf (part scm :spinner) spinner)
    (ui (vbox :padding '(28 20 20 20) :spacing 8
          (hbox (spacer) (:widget spinner) (spacer))
          (label :text "Reading the repository…" :text-color :secondary-text :font (make-font :size 11)
                 :alignment :center)
          (spacer)))))

(defun make-clean-page ()
  (ui (vbox :padding '(24 20 20 20) :spacing 6
        (hbox (spacer) (:widget (make-instance 'image-view :image (icon :check :size 28 :color :secondary-text))) (spacer))
        (label :text "No changes" :font (make-font :size 13 :weight :bold) :alignment :center)
        (label :text "The working tree matches the last commit." :text-color :secondary-text :wrap t
               :alignment :center :font (make-font :size 11))
        (spacer))))

(defun make-change-tree (scm)
  (make-instance 'tree-view
                 :key-function #'row-key
                 :children-function #'scm-row-children
                 :selection-mode :multiple
                 :columns (list (make-column "Name" #'row-name
                                             :icon-function (lambda (row) (change-row-icon scm row)))
                                (make-column "Folder" #'row-folder :width 56 :text-color :secondary-text)
                                (make-column "Status" #'row-count :width 24 :alignment :right
                                             :icon-function (lambda (row) (row-status-icon scm row))))))

(defun scm-row-at (tree event)
  (let ((row (item-at-point tree (event-x event) (event-y event))))
    (and (scm-row-p row) row)))

(defun install-tree-handlers (scm tree)
  (let ((workbench (scm-workbench scm)))
    ;; A single click previews a file's diff, as the explorer opens a file.
    (on tree :mouse-down
        (lambda (event)
          (let ((row (scm-row-at tree event)))
            (when (and row (not (group-row-p row)) (= 1 (event-click-count event))
                       (null (event-modifiers event)) (member (event-button event) '(1 :left)))
              (after 0 (lambda () (open-change scm row :pin nil)))))))
    (on tree :row-activated
        (lambda (event)
          (let ((row (event-item event)))
            (when (and (scm-row-p row) (not (group-row-p row)))
              (open-change scm row)))))
    ;; Space stages or unstages the selected files in one Git operation.
    (on tree :key-down
        (lambda (event)
          (when (and (eq :space (event-key event)) (null (event-modifiers event)))
            (let ((rows (selected-change-rows tree)))
              (when rows
                (with-reported-errors (workbench) (toggle-selected-staged scm rows))
                (consume event))))))
    (on tree :context-request
        (lambda (event)
          (let ((row (scm-row-at tree event)))
            (when row
              (unless (member row (selected-tree-items tree))
                (setf (selected-tree-item tree) row))
              (let* ((rows (selected-change-rows tree))
                     (menu (cond ((group-row-p row) (group-menu scm row))
                                 ((rest rows) (selected-changes-menu scm rows))
                                 (t (change-menu scm row)))))
                (when menu (popup-menu menu tree (event-x event) (event-y event))))
              (consume event)))))))

(defun make-source-control-panel (workbench)
  (let* ((scm (make-instance 'source-control :workbench workbench))
         (heading (make-instance 'label :text "SOURCE CONTROL" :font (make-font :size 11 :weight :bold)
                                        :text-color :secondary-text :valign :center))
         (count (make-instance 'label :text "" :font (make-font :size 11) :text-color :secondary-text
                                      :valign :center :tooltip "Changed files"))
         (refresh (scm-icon-button :refresh "Refresh" "Read the repository's status again"))
         (more (scm-icon-button :more "More Actions" "Pull, push, fetch, stage and branches"))
         (message (make-instance 'commit-input :source-control scm :buffer (make-text-buffer)
                                               :placeholder "Commit message"
                                               :font (editor-font (workbench-themes workbench))))
         (commit (make-instance 'button :label "Commit" :image (icon :check :size 14) :control-size :small
                                        :hexpand t :enabled nil))
         (commit-hint (hint-label ""))
         (tree (make-change-tree scm))
         (changes (make-instance 'stack))
         (tree-page (ui (vbox :spacing 0 (:widget tree :expand t))))
         (clean (make-clean-page))
         (branch (make-instance 'button :style :flat :label "" :image (icon :branch :size 14)
                                        :control-size :small))
         (sync (make-instance 'button :style :flat :label "" :image (icon :sync :size 14)
                                      :control-size :small :visible nil))
         (busy (make-instance 'spinner :active nil :visible nil))
         (activity (make-instance 'label :text "" :text-color :secondary-text :font (make-font :size 11)))
         (body (make-instance 'stack)))
    (setf (user-data sync) :sync
          (workbench-source-control workbench) scm
          (part scm :count) count (part scm :refresh) refresh (part scm :more) more
          (part scm :message) message (part scm :commit) commit (part scm :commit-hint) commit-hint
          (part scm :tree) tree (part scm :changes) changes (part scm :tree-page) tree-page
          (part scm :clean) clean (part scm :branch) branch (part scm :sync) sync
          (part scm :busy) busy (part scm :activity) activity (part scm :body) body)
    (add-page changes tree-page "Changes")
    (add-page changes clean "No Changes")
    (let ((loading (make-scm-loading-page scm))
          (no-repository (make-no-repository-page scm))
          (repository (ui (vbox :spacing 0
                            (vbox :padding '(4 12 8 12) :spacing 6
                              (frame (:widget message))
                              (hbox (:widget commit :expand t))
                              (:widget commit-hint))
                            (separator)
                            (:widget changes :expand t)
                            (separator)
                            (vbox :padding '(4 8 8 8) :spacing 2
                              (hbox :spacing 4 (:widget branch) (spacer) (:widget sync))
                              (hbox :padding '(0 4 0 4) :spacing 6 (:widget busy) (:widget activity :expand t)))))))
      (setf (part scm :loading) loading (part scm :no-repository) no-repository (part scm :repository) repository)
      (add-page body loading "Loading")
      (add-page body no-repository "No Repository")
      (add-page body repository "Repository"))
    (on (buffer message) :changed (lambda (e) (declare (ignore e)) (refresh-commit-controls scm)))
    (on commit :clicked (action (workbench) (commit-changes scm)))
    (on refresh :clicked (action (workbench) (refresh-source-control workbench)))
    (on more :clicked (action (workbench) (popup-below (more-menu scm) more)))
    (on branch :clicked (action (workbench) (show-branch-menu workbench branch)))
    (on sync :clicked (action (workbench) (sync-repository scm)))
    (install-tree-handlers scm tree)
    (refresh-scm-view scm)
    (ui (vbox :spacing 0
          (hbox :padding '(10 8 6 12) :spacing 6
            (:widget heading) (spacer) (:widget count) (:widget refresh) (:widget more))
          (separator)
          (:widget body :expand t)))))
