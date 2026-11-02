(in-package #:xeon.ui)

;;; The explorer: the opened folder as a native tree. Folders are read when they
;;; are first opened, on a worker thread, through the backend's folder project
;;; (which applies Git ignore rules). Nodes are keyed by path, so refreshing keeps
;;; open folders and the selection.

(defstruct (node (:constructor %make-node))
  key name path directory-p link-p kind
  (children nil) (state :unloaded))

(defun entry-node (entry)
  (let ((path (xeon.folders:entry-path entry)))
    (%make-node :key (namestring path) :name (xeon.folders:entry-name entry) :path path
                :directory-p (xeon.folders:entry-directory-p entry)
                :link-p (xeon.folders:entry-link-p entry)
                :kind (xeon.folders:entry-icon entry))))

(defun message-node (parent text)
  (%make-node :key (list :message (node-key parent)) :name text :state :message))

(defclass explorer ()
  ((workbench :initarg :workbench :reader explorer-workbench)
   (project :initform nil :accessor explorer-project)
   (root :initform nil :accessor explorer-root)
   (generation :initform 0 :accessor explorer-generation)
   (show-hidden :initform nil :accessor explorer-show-hidden)
   (tree :accessor explorer-tree)
   (heading :accessor explorer-heading)
   (panel :accessor explorer-panel)))

(defun visible-children (node)
  (cond ((not (node-directory-p node)) '())
        ((node-link-p node) '())
        ((eq (node-state node) :loaded) (node-children node))
        ((eq (node-state node) :failed) (node-children node))
        (t (list (message-node node "Loading…")))))

(defun node-icon (explorer node)
  (let ((palette (workbench-palette (explorer-workbench explorer))))
    (cond ((eq (node-state node) :message) nil)
          ((node-directory-p node)
           (file-icon palette (if (item-expanded-p (explorer-tree explorer) node) :folder-open :folder)))
          (t (file-icon palette (node-kind node))))))

(defun read-folder (explorer project path)
  (let ((hidden (explorer-show-hidden explorer)))
    (remove-if (lambda (entry)
                 (and (not hidden) (uiop:string-prefix-p "." (xeon.folders:entry-name entry))))
               (xeon.folders:list-folder-children project path :show-hidden hidden))))

(defun load-node (explorer node)
  "Reads NODE's folder on a worker thread and shows its children when they arrive."
  (when (and (node-directory-p node) (not (node-link-p node)) (eq (node-state node) :unloaded))
    (setf (node-state node) :loading)
    (let ((generation (explorer-generation explorer))
          (project (explorer-project explorer))
          (tree (explorer-tree explorer))
          (backend *backend*))
      (sb-thread:make-thread
       (lambda ()
         (let ((*backend* backend) (entries nil) (failure nil))
           (handler-case (setf entries (read-folder explorer project (node-path node)))
             (error (condition) (setf failure (princ-to-string condition))))
           (call-in-ui-thread
            (lambda ()
              (when (= generation (explorer-generation explorer))
                (if failure
                    (setf (node-state node) :failed
                          (node-children node) (list (message-node node "Unable to read this folder")))
                    (setf (node-state node) :loaded
                          (node-children node) (mapcar #'entry-node entries)))
                (refresh-tree tree node)
                (load-open-children explorer node)
                (when failure (report (explorer-workbench explorer) "~A" failure))))
            :owner tree)))
       :name "Xeon folder reader"))))

(defun open-folder (workbench path)
  "Shows the folder PATH in the explorer."
  (let* ((explorer (workbench-explorer workbench))
         (project (xeon.folders:open-folder-project path))
         (root-path (xeon.folders:project-root project))
         (name (or (car (last (pathname-directory root-path))) (namestring root-path)))
         (root (%make-node :key (namestring root-path) :name name :path root-path :directory-p t
                           :kind :folder)))
    (incf (explorer-generation explorer))
    (setf (explorer-project explorer) project
          (explorer-root explorer) root
          (text (explorer-heading explorer)) (string-upcase name))
    (setf (roots (explorer-tree explorer)) (list root))
    (load-node explorer root)
    (setf (item-expanded-p (explorer-tree explorer) root) t)
    (when (xeon.folders:project-warning project)
      (report workbench "~A" (xeon.folders:project-warning project)))
    (remember-folder workbench root-path)
    (follow-chat-folders workbench root-path)
    (projects-folder-changed workbench)
    (source-control-folder-changed workbench)
    (update-title workbench)
    root-path))

(defun explorer-folder (workbench)
  (let ((explorer (workbench-explorer workbench)))
    (and explorer (explorer-project explorer)
         (xeon.folders:project-root (explorer-project explorer)))))

(defun refresh-explorer (workbench)
  "Reads the folder again. The tree remembers open folders by key; they are read again
as their parents arrive."
  (let* ((explorer (workbench-explorer workbench))
         (old (explorer-root explorer)))
    (when old
      (incf (explorer-generation explorer))
      (let ((root (%make-node :key (node-key old) :name (node-name old) :path (node-path old)
                              :directory-p t :kind :folder)))
        (setf (explorer-root explorer) root
              (roots (explorer-tree explorer)) (list root))
        (load-node explorer root)))))

(defun load-open-children (explorer node)
  "Reads NODE's children that the tree remembers as open, as after a refresh."
  (let ((tree (explorer-tree explorer)))
    (dolist (child (node-children node))
      (when (and (node-directory-p child) (item-expanded-p tree child))
        (load-node explorer child)))))

(defun explorer-node-at (explorer event)
  (let ((item (item-at-point (explorer-tree explorer) (event-x event) (event-y event))))
    (and item (node-p item) (node-path item) item)))

(defun explorer-context-menu (workbench explorer node)
  (let ((path (node-path node)))
    (ui (menu
          (menu-item :label (if (node-directory-p node) "Expand" "Open")
                     :on-activate (lambda (e) (declare (ignore e)) (activate-node workbench explorer node)))
          (menu-separator)
          (menu-item :label "Reveal in Finder"
                     :on-activate (lambda (e) (declare (ignore e))
                                    (open-url (if (node-directory-p node) path
                                                  (uiop:pathname-directory-pathname path)))))
          (menu-item :label "Copy Path"
                     :on-activate (lambda (e) (declare (ignore e))
                                    (setf (clipboard-text) (uiop:native-namestring path))))
          (menu-separator)
          (menu-item :label "Refresh"
                     :on-activate (lambda (e) (declare (ignore e)) (refresh-explorer workbench)))))))

(defun activate-node (workbench explorer node)
  (cond ((not (node-path node)))
        ((node-directory-p node)
         (unless (node-link-p node)
           (let ((tree (explorer-tree explorer)))
             (setf (item-expanded-p tree node) (not (item-expanded-p tree node))))))
        (t (with-reported-errors (workbench) (open-document workbench (node-path node))))))

(defun make-explorer-panel (workbench)
  (let* ((explorer (make-instance 'explorer :workbench workbench))
         (heading (make-instance 'label :text "EXPLORER" :font (make-font :size 11 :weight :bold)
                                        :text-color :secondary-text :valign :center))
         (hidden (make-instance 'check-box :label "Show hidden / ignored" :control-size :small))
         (refresh (make-instance 'button :style :icon :label "Refresh" :tooltip "Refresh folder"
                                         :image (icon :refresh :size 16) :control-size :small))
         (tree (make-instance 'tree-view
                              :key-function #'node-key
                              :label-function #'node-name
                              :children-function #'visible-children
                              :icon-function (lambda (node) (node-icon explorer node))))
         (panel (ui (vbox :spacing 0
                      (hbox :padding '(10 12 6 12) (:widget heading))
                      (hbox :padding '(0 8 6 12) :spacing 6
                        (:widget hidden) (spacer) (:widget refresh))
                      (:widget tree :expand t)))))
    (setf (explorer-tree explorer) tree (explorer-heading explorer) heading
          (explorer-panel explorer) panel (workbench-explorer workbench) explorer)
    (on tree :expanded
        (lambda (event)
          (let ((node (event-item event)))
            (when (node-p node)
              (if (eq (node-state node) :unloaded)
                  (load-node explorer node)
                  (refresh-item tree node))))))
    (on tree :collapsed
        (lambda (event) (when (node-p (event-item event)) (refresh-item tree (event-item event)))))
    (on tree :row-activated
        (lambda (event)
          (let ((node (event-item event)))
            ;; Mouse activation is handled by the first click below. Keep Return.
            (when (and (node-p node) (zerop (event-click-count event)))
              (activate-node workbench explorer node)))))
    ;; Let native selection finish first. Capture the target expansion state so
    ;; native disclosure clicks do not toggle the folder twice.
    (on tree :mouse-down
        (lambda (event)
          (let ((node (explorer-node-at explorer event)))
            (when (and node (= 1 (event-click-count event))
                       (null (event-modifiers event)) (eq :left (event-button event)))
              (if (node-directory-p node)
                  (unless (node-link-p node)
                    (let ((expand (not (item-expanded-p tree node))))
                      (after 0 (lambda () (setf (item-expanded-p tree node) expand)))))
                  (after 0 (lambda () (activate-node workbench explorer node))))))))
    (on tree :context-request
        (lambda (event)
          (let ((node (explorer-node-at explorer event)))
            (when node
              (popup-menu (explorer-context-menu workbench explorer node) tree (event-x event) (event-y event))
              (consume event)))))
    (on hidden :toggled
        (lambda (event)
          (setf (explorer-show-hidden explorer) (event-value event))
          (refresh-explorer workbench)))
    (on refresh :clicked (lambda (event) (declare (ignore event)) (refresh-explorer workbench)))
    panel))
