(in-package #:xeon.ui)

;;; Native menus. LGT supplies the application, Edit and Window menus on macOS;
;;; Xeon adds File, View and Lisp. Availability is checked when a menu opens or a
;;; shortcut fires, never by polling.

(defmacro action ((workbench) &body body)
  `(lambda (event) (declare (ignore event))
     (with-reported-errors (,workbench) ,@body)))

(defun available (predicate)
  (lambda (item) (declare (ignore item)) (and (funcall predicate) t)))

(defun choose-and-open-file (workbench)
  (let ((paths (choose-file :mode :open :multiple t :parent (workbench-window workbench)
                            :directory (explorer-folder workbench))))
    (dolist (path paths)
      (with-reported-errors (workbench) (open-document workbench path)))))

(defun choose-and-open-folder (workbench)
  (let ((path (choose-directory :parent (workbench-window workbench)
                                :directory (explorer-folder workbench))))
    (when path (open-folder workbench path))))

(defun recent-items (workbench kind)
  (let* ((history (workbench-history workbench))
         (paths (ecase kind
                  (:file (xeon.themes:workspace-history-files history))
                  (:folder (xeon.themes:workspace-history-folders history)))))
    (if paths
        (loop for path in paths
              collect (let ((path path))
                        (make-instance 'menu-item
                                       :label (format nil "~A — ~A"
                                                      (let ((p (uiop:parse-native-namestring path)))
                                                        (if (eq kind :folder)
                                                            (or (car (last (pathname-directory (uiop:ensure-directory-pathname p)))) path)
                                                            (file-namestring p)))
                                                      path)
                                       :on-activate (action (workbench)
                                                      (if (eq kind :folder)
                                                          (open-folder workbench path)
                                                          (open-document workbench (uiop:parse-native-namestring path)))))))
        (list (make-instance 'menu-item :label (if (eq kind :folder) "No Recent Folders" "No Recent Files")
                                        :enabled nil)))))

(defun recent-menu (workbench title kind)
  (let ((menu (make-instance 'menu :title title)))
    (setf (menu-items menu) (recent-items workbench kind))
    (on menu :opening (lambda (e) (declare (ignore e))
                        (setf (menu-items menu) (recent-items workbench kind))))
    menu))

(defun repl-matches-document-p (workbench)
  (let ((d (active-document workbench))
        (tools (workbench-repl workbench)))
    (and d tools (lisp-document-p d) (repl-ready-p tools)
         (eq (document-language d) (getf (repl-snapshot tools) :dialect)))))

(defun make-menus (workbench)
  (let ((document-open (available (lambda () (active-document workbench))))
        (can-evaluate (available (lambda () (repl-matches-document-p workbench))))
        (can-compile (available (lambda ()
                                  (and (repl-matches-document-p workbench)
                                       (member :compile (getf (repl-snapshot (workbench-repl workbench))
                                                              :capabilities)))))))
    (ui (menu-bar
          (menu :title "File"
            (menu-item :label "New" :shortcut "Primary+N" :on-activate (action (workbench) (new-document workbench)))
            (menu-item :label "Open…" :shortcut "Primary+O" :on-activate (action (workbench) (choose-and-open-file workbench)))
            (menu-item :label "Open Folder…" :shortcut "Primary+Shift+O"
                       :on-activate (action (workbench) (choose-and-open-folder workbench)))
            (:widget (recent-menu workbench "Open Recent" :file))
            (:widget (recent-menu workbench "Open Recent Folder" :folder))
            (menu-separator)
            (menu-item :label "Save" :shortcut "Primary+S" :enabled-function document-open
                       :on-activate (action (workbench) (save-document workbench)))
            (menu-item :label "Save As…" :shortcut "Primary+Shift+S" :enabled-function document-open
                       :on-activate (action (workbench) (save-document-as workbench)))
            (menu-item :label "Save All" :shortcut "Primary+Alt+S"
                       :on-activate (action (workbench) (save-all-documents workbench)))
            (menu-separator)
            (menu-item :label "Close Tab" :shortcut "Primary+W" :enabled-function document-open
                       :on-activate (action (workbench) (close-document workbench)))
            (menu-item :role :preferences :on-activate (action (workbench) (show-settings workbench))))
          (menu :title "View"
            (menu-item :label "Toggle Explorer" :shortcut "Primary+B"
                       :on-activate (action (workbench) (toggle-sidebar workbench)))
            (menu-item :label "Toggle Runtime Tools" :shortcut "Primary+J"
                       :on-activate (action (workbench) (toggle-tools workbench)))
            (menu-separator)
            (menu-item :label "REPL" :shortcut "Primary+1" :on-activate (action (workbench) (select-tool workbench :repl)))
            (menu-item :label "Problems" :shortcut "Primary+2" :on-activate (action (workbench) (select-tool workbench :problems)))
            (menu-item :label "Debugger" :shortcut "Primary+3" :on-activate (action (workbench) (select-tool workbench :debugger)))
            (menu-item :label "Inspector" :shortcut "Primary+4" :on-activate (action (workbench) (select-tool workbench :inspector)))
            (menu-separator)
            (menu-item :label "Refresh Explorer" :on-activate (action (workbench) (refresh-explorer workbench))))
          (menu :title "Lisp"
            (menu-item :label "Evaluate Form" :shortcut "Primary+Return" :enabled-function can-evaluate
                       :on-activate (action (workbench) (send-editor-source workbench :form nil)))
            (menu-item :label "Evaluate Buffer" :shortcut "Primary+Shift+Return" :enabled-function can-evaluate
                       :on-activate (action (workbench) (send-editor-source workbench :buffer nil)))
            (menu-item :label "Compile Form" :shortcut "Primary+K" :enabled-function can-compile
                       :on-activate (action (workbench) (send-editor-source workbench :form t)))
            (menu-item :label "Compile Buffer" :shortcut "Primary+Shift+K" :enabled-function can-compile
                       :on-activate (action (workbench) (send-editor-source workbench :buffer t)))
            (menu-separator)
            (menu-item :label "Start Runtime" :enabled-function
                       (available (lambda () (member (repl-status (workbench-repl workbench)) '(:disconnected :error))))
                       :on-activate (action (workbench) (start-repl (workbench-repl workbench)) (select-tool workbench :repl)))
            (menu-item :label "Interrupt" :shortcut "Primary+." :enabled-function
                       (available (lambda () (member (repl-status (workbench-repl workbench)) '(:busy :input :debugging))))
                       :on-activate (action (workbench) (xeon.repl:repl-interrupt (tools-session (workbench-repl workbench)))))
            (menu-item :label "Disconnect" :enabled-function
                       (available (lambda () (not (eq :disconnected (repl-status (workbench-repl workbench))))))
                       :on-activate (action (workbench) (xeon.repl:disconnect-repl (tools-session (workbench-repl workbench))))))))))

(defun install-shortcuts (workbench)
  (declare (ignore workbench)))
