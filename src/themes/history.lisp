(in-package #:xeon.themes)

(defstruct workspace-history path files folders last-folder warning)

(defun load-workspace-history (&optional (path (merge-pathnames "workspace.sexp" (default-configuration-directory))))
  (let ((history (make-workspace-history :path path)))
    (when (probe-file path)
      (handler-case
          (let ((data (read-theme-data path 262144)))
            (unless (and (finite-list-p data) (= (length data) 8) (eq (first data) :version) (eql (second data) 1)
                         (eq (third data) :files) (eq (fifth data) :folders) (eq (seventh data) :last-folder))
              (error "Invalid workspace history."))
            (dolist (paths (list (fourth data) (sixth data)))
              (unless (and (finite-list-p paths) (<= (length paths) 20)
                           (every (lambda (p) (and (stringp p) (<= 1 (length p) 1024) (not (find #\Null p)))) paths))
                (error "Invalid recent paths.")))
            (unless (or (null (eighth data)) (member (eighth data) (sixth data) :test #'equal))
              (error "Invalid last folder."))
            (setf (workspace-history-files history) (fourth data)
                  (workspace-history-folders history) (sixth data)
                  (workspace-history-last-folder history) (eighth data)))
        (error (condition) (setf (workspace-history-warning history) (format nil "Workspace history ignored: ~A" condition)))))
    history))

(defun remember-workspace-path (history kind path)
  (let* ((name (uiop:native-namestring (truename path)))
         (old (ecase kind (:file (workspace-history-files history)) (:folder (workspace-history-folders history))))
         (paths (cons name (remove name old :test #+win32 #'equalp #-win32 #'equal))))
    (when (> (length name) 1024) (return-from remember-workspace-path nil))
    (setf paths (subseq paths 0 (min 20 (length paths))))
    (ecase kind
      (:file (setf (workspace-history-files history) paths))
      (:folder (setf (workspace-history-folders history) paths (workspace-history-last-folder history) name)))
    (when (workspace-history-path history)
      (write-data-file (workspace-history-path history)
                       (list :version 1 :files (workspace-history-files history)
                             :folders (workspace-history-folders history) :last-folder (workspace-history-last-folder history))))))

(defun rewrite-workspace-paths (history old &optional new (persist t))
  "Retarget recent entries after rename, or remove them after deletion."
  (let* ((old-name (uiop:native-namestring old)) (new-name (when new (uiop:native-namestring new)))
         (directory-p (uiop:directory-pathname-p old)))
    (labels ((same (a b) (funcall #+win32 #'equalp #-win32 #'equal a b))
             (rewrite (name)
               (when name
                 (if (or (same name old-name)
                         (and directory-p (>= (length name) (length old-name))
                              (same old-name (subseq name 0 (length old-name)))))
                     (when new-name (concatenate 'string new-name (subseq name (length old-name)))) name))))
      (setf (workspace-history-files history) (remove nil (mapcar #'rewrite (workspace-history-files history)))
            (workspace-history-folders history) (remove nil (mapcar #'rewrite (workspace-history-folders history)))
            (workspace-history-last-folder history) (rewrite (workspace-history-last-folder history))))
    (when (and persist (workspace-history-path history))
      (write-data-file (workspace-history-path history)
                       (list :version 1 :files (workspace-history-files history)
                             :folders (workspace-history-folders history) :last-folder (workspace-history-last-folder history))))))


(defun rewrite-workspace-entries (history pairs)
  "Apply (OLD . NEW) path changes, with NIL NEW for deletion, then persist once."
  (dolist (pair pairs) (rewrite-workspace-paths history (car pair) (cdr pair) nil))
  (when (workspace-history-path history)
    (write-data-file (workspace-history-path history)
                     (list :version 1 :files (workspace-history-files history)
                           :folders (workspace-history-folders history) :last-folder (workspace-history-last-folder history)))))

(defun load-chat-composer-height (path)
  (or (when (and path (probe-file path))
        (handler-case
            (let ((data (read-theme-data path 4096)))
              (when (and (finite-list-p data) (= (length data) 2)
                         (eql (first data) 1) (typep (second data) '(integer 80 10000)))
                (second data)))
          (error () nil)))
      210))

(defun save-chat-composer-height (path height)
  (when path
    (write-data-file path (list 1 (max 80 (min 10000 height))))))

(defun load-sidebar-state (path)
  "Read bounded, non-executable UI geometry; invalid state falls back to defaults."
  (when (and path (probe-file path))
    (handler-case
        (let ((data (read-theme-data path 4096)))
          (when (and (finite-list-p data) (or (and (= (length data) 5) (eql (first data) 1))
                         (and (= (length data) 6) (eql (first data) 2)
                              (member (sixth data) '(t nil))))
                     (member (second data) '(t nil)) (member (third data) '(t nil))
                     (typep (fourth data) '(integer 80 10000))
                     (typep (fifth data) '(integer 80 10000)))
            data))
      (error () nil))))

(defun save-sidebar-state (path left-visible right-visible left-width right-width &optional maximized-p)
  (when path
    (write-data-file path (list 2 (not (null left-visible)) (not (null right-visible))
                               (max 80 (min 10000 left-width))
                               (max 80 (min 10000 right-width)) (not (null maximized-p))))))
