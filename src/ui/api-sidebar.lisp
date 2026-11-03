(in-package #:xeon.ui)

;;; The API sidebar view and the environments editor.
;;;
;;; Collections are .http files: project ones in the open folder's .xeon/api, system
;;; ones in the api folder of Xeon's configuration directory. The view lists both, with
;;; each collection's requests, read when a collection is first shown and again after a
;;; save or a builder change, never while idle.
;;;
;;; Environments, secrets included, are kept in api-environments.sexp in the
;;; configuration directory (owner-only), never in a project, so requests hold only
;;; {{name}} placeholders. One environment is active per project folder.

(defvar *api-environment-file* nil
  "The environments file, or NIL for api-environments.sexp in Xeon's configuration directory.")

(defvar *api-system-directory* nil
  "The system collections folder, or NIL for api/ in Xeon's configuration directory.")

(defun api-environment-path ()
  (or *api-environment-file*
      (merge-pathnames "api-environments.sexp" (xeon.themes:default-configuration-directory))))

(defun api-system-directory ()
  (or *api-system-directory*
      (xeon.http:system-collection-directory (xeon.themes:default-configuration-directory))))

(defun api-project-directory (workbench)
  (let ((root (explorer-folder workbench)))
    (and root (xeon.http:project-collection-directory root))))

(defclass api-sidebar ()
  ((workbench :initarg :workbench :reader api-sidebar-workbench)
   (store :initform nil :accessor api-sidebar-store)
   (loaded :initform nil :accessor api-sidebar-loaded)
   (widgets :initform (make-hash-table))))

;;; Environments

(defun api-environment-store (workbench)
  "The environments, read from disk on first use."
  (let ((sidebar (workbench-api workbench)))
    (or (api-sidebar-store sidebar)
        (let ((store (xeon.http:load-environment-store (api-environment-path))))
          (setf (api-sidebar-store sidebar) store)
          (let ((warning (xeon.http:environment-store-warning store)))
            (when warning (report workbench "~A" warning)))
          store))))

(defun current-api-environment (workbench)
  (xeon.http:active-environment (api-environment-store workbench) (explorer-folder workbench)))

(defun environment-combos (workbench)
  (remove nil (cons (part (workbench-api workbench) :environment)
                    (mapcar (lambda (page) (part page :environment)) (api-pages workbench)))))

(defun refresh-environment-choices (workbench)
  "Brings every environment menu up to date with the store and the active choice."
  (let* ((store (api-environment-store workbench))
         (root (explorer-folder workbench))
         (items (cons :none (xeon.http:project-environments store root)))
         (index (position (or (xeon.http:active-environment store root) :none) items)))
    (dolist (combo (environment-combos workbench))
      (with-handlers-blocked (combo :changed)
        (unless (equal items (coerce (items combo) 'list)) (setf (items combo) items))
        (unless (eql index (selected-index combo)) (setf (selected-index combo) index))))))

(defun choose-api-environment (workbench environment)
  (xeon.http:set-active-environment (api-environment-store workbench) (explorer-folder workbench)
                                    (if (eq environment :none) nil environment))
  (refresh-environment-choices workbench)
  (report workbench "~:[No API environment~;API environment: ~:*~A~]"
          (and (not (eq environment :none)) (xeon.http:environment-name environment))))

;;; Tree items

(defstruct (api-node (:constructor api-node (kind label &key path index method scope)))
  "An item of the API tree. KIND is :SCOPE, :COLLECTION, :REQUEST or :HINT."
  kind label path index method scope)

(defun api-node-key (node)
  (list (api-node-kind node) (api-node-scope node)
        (and (api-node-path node) (namestring (api-node-path node))) (api-node-index node)
        (and (eq (api-node-kind node) :hint) (api-node-label node))))

(defparameter *method-colors*
  '(("GET" . :system-green) ("POST" . :system-orange) ("PUT" . :system-blue) ("PATCH" . :system-purple)
    ("DELETE" . :system-red)))

(defun api-node-icon (node)
  (case (api-node-kind node)
    (:scope (icon (if (eq (api-node-scope node) :project) :folder-special :computer) :size 16 :color :secondary-text))
    (:collection (icon :api :size 16 :color :system-teal))
    (:request (icon :http :size 16 :color (or (cdr (assoc (api-node-method node) *method-colors* :test #'string-equal))
                                              :secondary-text)))
    (t nil)))

(defun api-node-text (node)
  (if (eq (api-node-kind node) :request)
      (format nil "~A  ~A" (api-node-method node) (api-node-label node))
      (api-node-label node)))

(defun api-filter (sidebar)
  (let ((filter (string-trim " " (text (part sidebar :filter)))))
    (and (plusp (length filter)) filter)))

(defun open-collection-text (workbench path)
  "PATH's text from its open tab, which may be ahead of the file, or NIL."
  (let ((d (find-if (lambda (d) (let ((p (document-path d)))
                                  (and p (equal (ignore-errors (truename p)) (ignore-errors (truename path))))))
                    (workbench-documents workbench))))
    (and d (xeon.core:buffer-text (document-buffer d)))))

(defun collection-entries (workbench path)
  "PATH's requests, or a hint node explaining why they cannot be listed."
  (handler-case
      (let ((text (or (open-collection-text workbench path)
                      (progn
                        (when (> (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))
                                 (* 2 1024 1024))
                          (error "Too large to list; open it to edit."))
                        (uiop:read-file-string path :external-format :utf-8)))))
        (xeon.http:parse-collection text))
    (error (condition)
      (list (api-node :hint (format nil "Cannot list requests: ~A" condition))))))

(defun request-nodes (workbench path scope)
  (let ((entries (collection-entries workbench path)))
    (if (and entries (typep (first entries) 'api-node))
        entries
        (loop for entry in entries for i from 0
              collect (api-node :request (xeon.http:entry-name entry) :path path :index i :scope scope
                                         :method (xeon.http:request-method (xeon.http:entry-request entry)))))))

(defun node-matches-p (node filter)
  (search filter (api-node-text node) :test #'char-equal))

(defun api-node-children (sidebar node)
  (let ((workbench (api-sidebar-workbench sidebar))
        (filter (api-filter sidebar)))
    (case (api-node-kind node)
      (:scope
       (let ((directory (api-node-path node)) (scope (api-node-scope node)))
         (if (null directory)
             (list (api-node :hint "Open a folder to keep requests with its project"))
             (let ((collections
                     (loop for path in (xeon.http:list-collections directory)
                           for collection = (api-node :collection (pathname-name path) :path path :scope scope)
                           when (or (null filter) (node-matches-p collection filter)
                                    (some (lambda (r) (and (eq (api-node-kind r) :request) (node-matches-p r filter)))
                                          (request-nodes workbench path scope)))
                             collect collection)))
               (or collections
                   (list (api-node :hint (if filter "No matching requests" "No collections yet · + adds one"))))))))
      (:collection
       (let ((requests (request-nodes workbench (api-node-path node) (api-node-scope node))))
         (if (and filter (not (node-matches-p node filter)))
             (remove-if-not (lambda (r) (or (eq (api-node-kind r) :hint) (node-matches-p r filter))) requests)
             requests)))
      (t '()))))

(defun api-roots (workbench)
  (let ((root (explorer-folder workbench)))
    (list (api-node :scope (if root
                               (format nil "PROJECT · ~A" (car (last (pathname-directory root))))
                               "PROJECT")
                    :scope :project :path (api-project-directory workbench))
          (api-node :scope "SYSTEM" :scope :system :path (api-system-directory)))))

(defun refresh-api-sidebar (workbench)
  "Reads the collections again, keeping open items open. Only once the view has been shown."
  (let ((sidebar (workbench-api workbench)))
    (when (and sidebar (api-sidebar-loaded sidebar))
      (let ((tree (part sidebar :tree))
            (roots (api-roots workbench)))
        (if (equalp roots (roots tree))
            (refresh-tree tree)
            (progn (setf (roots tree) roots)
                   (dolist (r roots) (expand-item tree r))))
        (when (api-filter sidebar) (expand-item tree nil :all t))))))

(defun ensure-api-loaded (workbench)
  "Lists the collections and environments the first time the view is shown."
  (let ((sidebar (workbench-api workbench)))
    (when (and sidebar (not (api-sidebar-loaded sidebar)))
      (setf (api-sidebar-loaded sidebar) t)
      (refresh-environment-choices workbench)
      (refresh-api-sidebar workbench))))

(defun api-folder-changed (workbench)
  "The open folder changed: its collections and environments replace the old ones'."
  (let ((sidebar (workbench-api workbench)))
    (when sidebar
      (refresh-api-sidebar workbench)
      (when (api-sidebar-store sidebar)
        (refresh-environment-choices workbench)))))

;;; Commands

(defun find-api-page-for (workbench path)
  (let ((d (find-if (lambda (d) (let ((p (document-path d)))
                                  (and p (equal (ignore-errors (truename p)) (ignore-errors (truename path))))))
                    (workbench-documents workbench))))
    (and d (find-document-page workbench d))))

(defun open-api-request (workbench path &optional index)
  "Opens the collection at PATH and shows its INDEXth request."
  (open-document workbench path)
  (let ((page (find-api-page-for workbench path)))
    (when (and (typep page 'api-page) index)
      (api-select-request page index))
    page))

(defun new-api-collection (workbench scope &key name)
  "Asks for a name, unless NAME, and creates a collection in SCOPE, :PROJECT or :SYSTEM."
  (let ((directory (if (eq scope :project)
                       (or (api-project-directory workbench)
                           (error "Open a folder to keep requests with its project."))
                       (api-system-directory))))
    (let ((name (or name
                    (prompt-text (if (eq scope :project) "New Project Collection" "New System Collection")
                                 :detail (if (eq scope :project)
                                             "Saved in the project's .xeon/api folder, to share with the project."
                                             "Saved in Xeon's settings, for every project.")
                                 :default "requests" :ok "Create" :parent (workbench-window workbench)))))
      (when (and name (not (blank-p name)))
        (let ((path (xeon.http:create-collection directory name)))
          (refresh-api-sidebar workbench)
          (open-api-request workbench path 0)
          path)))))

(defun new-api-request (workbench)
  "File › New API Request: a new collection, with the project when a folder is open."
  (new-api-collection workbench (if (explorer-folder workbench) :project :system)))

(defun delete-api-collection (workbench path)
  (when (confirm (format nil "Delete the collection “~A”?" (pathname-name path))
                 :detail "The .http file is deleted, with every request in it."
                 :yes "Delete" :parent (workbench-window workbench))
    (let ((page (find-api-page-for workbench path)))
      (when page (close-document workbench (page-document page) t)))
    (delete-file path)
    (refresh-api-sidebar workbench)
    (report workbench "Deleted ~A." (file-namestring path))))

(defun with-api-request-page (workbench node function)
  "Opens NODE's collection on its request and calls FUNCTION with the API tab."
  (let ((page (open-api-request workbench (api-node-path node) (api-node-index node))))
    (when (typep page 'api-page)
      (when (api-pro-p page) (api-set-mode page nil))
      (funcall function page))))

(defun api-node-menu (workbench node)
  (let ((path (api-node-path node)))
    (ecase (api-node-kind node)
      (:scope
       (ui (menu
             (menu-item :label "New Collection…" :enabled (and path t)
                        :on-activate (action (workbench) (new-api-collection workbench (api-node-scope node))))
             (menu-item :label "Reveal in Finder" :enabled (and path (uiop:directory-exists-p path) t)
                        :on-activate (action (workbench) (open-url path))))))
      (:collection
       (ui (menu
             (menu-item :label "Open" :on-activate (action (workbench) (open-api-request workbench path 0)))
             (menu-item :label "New Request"
                        :on-activate (action (workbench)
                                       (let ((page (open-api-request workbench path)))
                                         (when (typep page 'api-page)
                                           (when (api-pro-p page) (api-set-mode page nil))
                                           (api-new-request page)))))
             (menu-separator)
             (menu-item :label "Copy Path" :on-activate (action (workbench) (setf (clipboard-text) (uiop:native-namestring path))))
             (menu-item :label "Reveal in Finder"
                        :on-activate (action (workbench) (open-url (uiop:pathname-directory-pathname path))))
             (menu-separator)
             (menu-item :label "Delete Collection…" :on-activate (action (workbench) (delete-api-collection workbench path))))))
      (:request
       (ui (menu
             (menu-item :label "Open" :on-activate (action (workbench) (open-api-request workbench path (api-node-index node))))
             (menu-item :label "Send"
                        :on-activate (action (workbench) (with-api-request-page workbench node #'api-send)))
             (menu-separator)
             (menu-item :label "Duplicate"
                        :on-activate (action (workbench) (with-api-request-page workbench node #'api-duplicate-request)))
             (menu-item :label "Delete…"
                        :on-activate (action (workbench) (with-api-request-page workbench node #'api-delete-request))))))
      (:hint nil))))

(defun api-add-menu (workbench)
  (ui (menu
        (menu-item :label "New Project Collection…" :enabled (and (explorer-folder workbench) t)
                   :on-activate (action (workbench) (new-api-collection workbench :project)))
        (menu-item :label "New System Collection…"
                   :on-activate (action (workbench) (new-api-collection workbench :system)))
        (menu-separator)
        (menu-item :label "Environments…" :on-activate (action (workbench) (open-environments workbench))))))

(defun activate-api-node (workbench node)
  (case (api-node-kind node)
    (:request (open-api-request workbench (api-node-path node) (api-node-index node)))
    (:collection (open-api-request workbench (api-node-path node)))))

(defun restyle-api (workbench)
  (let ((sidebar (workbench-api workbench)))
    (when (and sidebar (api-sidebar-loaded sidebar))
      (refresh-tree (part sidebar :tree))))
  (restyle-api-pages workbench))

(defun make-api-panel (workbench)
  (let* ((sidebar (make-instance 'api-sidebar :workbench workbench))
         (heading (make-instance 'label :text "API" :font (make-font :size 11 :weight :bold)
                                        :text-color :secondary-text :valign :center))
         (add (make-instance 'button :style :icon :label "New" :tooltip "New collection or environment"
                                     :image (icon :add :size 16) :control-size :small))
         (refresh (make-instance 'button :style :icon :label "Refresh" :tooltip "Read the collections again"
                                         :image (icon :refresh :size 16) :control-size :small))
         (environment (make-environment-combo workbench))
         (manage (make-instance 'button :style :icon :label "Environments" :tooltip "Edit environments and secrets"
                                        :image (icon :key :size 16) :control-size :small))
         (filter (make-instance 'search-entry :placeholder "Filter requests" :control-size :small))
         (tree (make-instance 'tree-view
                              :key-function #'api-node-key
                              :children-function (lambda (node) (api-node-children sidebar node))
                              :columns (list (make-column "Request" #'api-node-text
                                                          :icon-function #'api-node-icon)))))
    (setf (workbench-api workbench) sidebar)
    (setf (part sidebar :environment) environment
          (part sidebar :filter) filter
          (part sidebar :tree) tree)
    (on add :clicked (lambda (e) (declare (ignore e))
                       (popup-menu (api-add-menu workbench) add 0 (or (widget-height add) 0))))
    (on refresh :clicked (action (workbench) (refresh-api-sidebar workbench)))
    (on manage :clicked (action (workbench) (open-environments workbench)))
    (on filter :changed (lambda (e) (declare (ignore e)) (refresh-api-sidebar workbench)))
    (on tree :row-activated (lambda (e) (with-reported-errors (workbench) (activate-api-node workbench (event-item e)))))
    (on tree :context-request
        (lambda (e)
          (let ((node (item-at-point tree (event-x e) (event-y e))))
            (when (and (api-node-p node) (not (eq (api-node-kind node) :hint)))
              (popup-menu (api-node-menu workbench node) tree (event-x e) (event-y e))
              (consume e)))))
    (ui (vbox :spacing 0
          (hbox :padding '(10 12 6 12) :spacing 6 (:widget heading) (spacer) (:widget refresh) (:widget add))
          (hbox :padding '(0 12 6 12) :spacing 6 (:widget environment :expand t) (:widget manage))
          (hbox :padding '(0 12 8 12) (:widget filter :expand t))
          (separator)
          (:widget tree :expand t)))))

;;; The environments editor

(defclass environments-page (vbox)
  ((workbench :initarg :workbench :reader page-workbench)
   (environment :initform nil :accessor shown-environment)
   (dirty :initform nil :accessor environments-dirty)
   (loading :initform nil :accessor environments-loading)
   (rows :initform '() :accessor environment-rows)
   (widgets :initform (make-hash-table) :reader page-widgets))
  (:default-initargs :spacing 0))

(defun environments-page (workbench)
  (find-if (lambda (p) (typep p 'environments-page)) (all-tab-pages workbench)))

(defun environment-scope-label (environment)
  (if (eq :system (xeon.http:environment-scope environment))
      "System"
      (format nil "Project · ~A" (car (last (pathname-directory
                                              (uiop:ensure-directory-pathname (xeon.http:environment-scope environment))))))))

(defun environments-mark-dirty (page)
  (unless (or (environments-loading page) (environments-dirty page))
    (setf (environments-dirty page) t)
    (set-enabled (part page :save) t)
    (set-enabled (part page :revert) t)
    (setf (text (part page :message)) "Unsaved changes")))

(defun environments-ready-p (page)
  (or (not (environments-dirty page))
      (progn (setf (text (part page :message)) "Save or Revert the changes first.") nil)))

(defun environment-value-field (secret text)
  (make-instance (if secret 'password-entry 'entry) :text text :hexpand t :control-size :small
                 :placeholder (if secret "Secret value" "Value")))

(defun add-environment-row (page &optional (name "") (value "") secret)
  (let* ((box (part page :variables))
         (row (make-instance 'hbox :spacing 6))
         (name-field (make-instance 'entry :text name :placeholder "Name" :width-chars 18 :control-size :small))
         (holder (make-instance 'hbox :hexpand t))
         (value-field (environment-value-field secret value))
         (secret-box (make-instance 'check-box :label "Secret" :checked secret
                                               :tooltip "Hide the value here and in failure messages"))
         (remove (make-instance 'button :style :icon :label "Remove" :tooltip "Remove this variable"
                                        :image (icon :close :size 14) :control-size :small))
         (record (list row name-field value-field secret-box)))
    (add holder value-field :expand t)
    (add row name-field) (add row holder :expand t) (add row secret-box) (add row remove)
    (add box row)
    (setf (environment-rows page) (append (environment-rows page) (list record)))
    (flet ((watch (field) (on field :changed (lambda (e) (declare (ignore e)) (environments-mark-dirty page)))))
      (watch name-field) (watch value-field)
      (on secret-box :toggled
          (lambda (e)
            ;; Swap the field, keeping its text, so a secret is never shown in clear.
            (let* ((old (third record))
                   (new (environment-value-field (event-value e) (text old))))
              (remove-child holder old) (destroy old)
              (add holder new :expand t)
              (setf (third record) new)
              (watch new)
              (environments-mark-dirty page)))))
    (on remove :clicked
        (lambda (e) (declare (ignore e))
          (setf (environment-rows page) (remove record (environment-rows page)))
          (remove-child box row) (destroy row)
          (environments-mark-dirty page)))
    record))

(defun environment-row-variables (page)
  (loop for (nil name value secret) in (environment-rows page)
        unless (and (blank-p (text name)) (blank-p (text value)))
          collect (list (string-trim " " (text name)) (text value) (and (checked secret) t))))

(defun show-environment (page environment)
  (setf (environments-loading page) t)
  (unwind-protect
       (progn
         (setf (shown-environment page) environment)
         (dolist (row (copy-list (children (part page :variables))))
           (remove-child (part page :variables) row) (destroy row))
         (setf (environment-rows page) '())
         (cond (environment
                (setf (text (part page :name)) (xeon.http:environment-name environment)
                      (text (part page :scope)) (environment-scope-label environment))
                (dolist (v (xeon.http:environment-variables environment))
                  (add-environment-row page (first v) (second v) (third v)))
                (select-page (part page :detail) (part page :editor)))
               (t (select-page (part page :detail) (part page :empty))))
         (setf (environments-dirty page) nil)
         (set-enabled (part page :save) nil)
         (set-enabled (part page :revert) nil)
         (set-enabled (part page :delete) environment)
         (setf (text (part page :message)) ""))
    (setf (environments-loading page) nil)))

(defun refresh-environment-list (page &optional (select (shown-environment page)))
  (let* ((workbench (page-workbench page))
         (table (part page :list))
         (items (xeon.http:project-environments (api-environment-store workbench) (explorer-folder workbench))))
    (with-handlers-blocked (table :selection-changed)
      (setf (items table) items)
      (let ((index (position select items)))
        (setf (selected-indices table) (if index (list index) '()))))
    (show-environment page (and (member select items) select))))

(defun save-environment (page)
  (let ((environment (shown-environment page))
        (workbench (page-workbench page)))
    (when environment
      (xeon.http:update-environment (api-environment-store workbench) environment
                                    :name (text (part page :name))
                                    :variables (environment-row-variables page))
      (refresh-environment-list page environment)
      (refresh-environment-choices workbench)
      (setf (text (part page :message)) "Saved")
      (report workbench "Saved the ~A environment." (xeon.http:environment-name environment)))))

(defun create-environment (page scope)
  (when (environments-ready-p page)
    (let* ((workbench (page-workbench page))
           (root (explorer-folder workbench)))
      (when (and (eq scope :project) (null root))
        (error "Open a folder to add environments for its project."))
      (let ((name (prompt-text (if (eq scope :project) "New Project Environment" "New System Environment")
                               :detail (if (eq scope :project)
                                           "Its variables are used only with this project folder."
                                           "Its variables can be used with any project.")
                               :default "Development" :ok "Create" :parent (workbench-window workbench))))
        (when (and name (not (blank-p name)))
          (let ((environment (xeon.http:add-environment (api-environment-store workbench) name
                                                        (if (eq scope :project) root :system))))
            (refresh-environment-list page environment)
            (refresh-environment-choices workbench)
            (focus (part page :name))
            environment))))))

(defun delete-environment (page)
  (let ((environment (shown-environment page))
        (workbench (page-workbench page)))
    (when (and environment
               (confirm (format nil "Delete the ~A environment?" (xeon.http:environment-name environment))
                        :detail "Its variables and secrets are deleted. Requests keep their {{name}} placeholders."
                        :yes "Delete" :parent (workbench-window workbench)))
      (xeon.http:remove-environment (api-environment-store workbench) environment)
      (refresh-environment-list page nil)
      (refresh-environment-choices workbench))))

(defun make-environments-page (workbench)
  (let* ((page (make-instance 'environments-page :workbench workbench))
         (list (make-instance 'table-view :headers t :sortable nil
                                          :columns (list (make-column "Environment" #'xeon.http:environment-name)
                                                         (make-column "Scope" #'environment-scope-label
                                                                      :text-color :secondary-text))))
         (add (small-button "New" :image (icon :add :size 14) :tooltip "Add a system or project environment"))
         (delete (small-button "Delete" :enabled nil :tooltip "Delete the selected environment"))
         (name (make-instance 'entry :placeholder "Environment name" :hexpand t))
         (scope (hint-label ""))
         (variables (make-instance 'vbox :spacing 6))
         (add-variable (small-button "Add Variable" :image (icon :add :size 14)))
         (save (small-button "Save" :enabled nil))
         (revert (small-button "Revert" :enabled nil))
         (message (hint-label ""))
         (detail (make-instance 'stack))
         (scroll (make-instance 'scrolled-window))
         (editor (ui (vbox :spacing 10 :padding '(14 16 12 16)
                       (grid :columns 2 :column-spacing 10 :row-spacing 6
                         (label :text "Name" :halign :end :valign :center) (:widget name)
                         (label :text "Scope" :halign :end :valign :center) (:widget scope))
                       (separator)
                       (hbox :spacing 8
                         (label :text "Variables" :font (make-font :size 12 :weight :bold) :valign :center)
                         (spacer) (:widget add-variable))
                       (:widget scroll :expand t)
                       (hbox :spacing 8 (:widget message :expand t) (:widget revert) (:widget save)))))
         (empty-title (message-title))
         (empty-text (message-text))
         (empty (centered-message :key empty-title empty-text)))
    (add scroll (ui (vbox :spacing 0 (:widget variables) (spacer))))
    (setf (text empty-title) "No environment selected"
          (text empty-text) "Choose New to add one. Requests use its variables as {{name}}; mark tokens and passwords Secret.")
    (flet ((keep (key widget) (setf (part page key) widget)))
      (keep :list list) (keep :delete delete) (keep :name name) (keep :scope scope) (keep :variables variables)
      (keep :save save) (keep :revert revert) (keep :message message) (keep :detail detail)
      (keep :editor editor) (keep :empty empty))
    (add-page detail empty "Empty") (add-page detail editor "Environment")
    (add page (ui (hbox :padding '(10 14 10 14) :spacing 10
                    (vbox :spacing 2
                      (label :text "API Environments" :font (make-font :size 13 :weight :bold))
                      (:widget (hint-label (format nil "Stored only in ~A, readable by you alone, never in project files."
                                                   (uiop:native-namestring (api-environment-path)))))))))
    (add page (make-instance 'separator))
    (add page (setf (part page :split)
                    (ui (paned
                          (vbox :spacing 0
                            (:widget list :expand t)
                            (separator)
                            (hbox :padding '(6 10 6 10) :spacing 6 (:widget add) (:widget delete) (spacer)))
                          (:widget detail))))
         :expand t)
    (on list :selection-changed
        (lambda (e)
          (let ((environment (event-value e)))
            (if (environments-ready-p page)
                (show-environment page environment)
                (with-handlers-blocked (list :selection-changed)
                  (let ((index (position (shown-environment page) (items list))))
                    (setf (selected-indices list) (if index (list index) '()))))))))
    (on add :clicked
        (lambda (e) (declare (ignore e))
          (popup-menu (ui (menu
                            (menu-item :label "New System Environment…"
                                       :on-activate (action (workbench) (create-environment page :system)))
                            (menu-item :label "New Project Environment…" :enabled (and (explorer-folder workbench) t)
                                       :on-activate (action (workbench) (create-environment page :project)))))
                      add 0 (or (widget-height add) 0))))
    (on delete :clicked (action (workbench) (delete-environment page)))
    (on add-variable :clicked (lambda (e) (declare (ignore e))
                                (let ((record (add-environment-row page)))
                                  (environments-mark-dirty page)
                                  (focus (second record)))))
    (on name :changed (lambda (e) (declare (ignore e)) (environments-mark-dirty page)))
    (on save :clicked (lambda (e) (declare (ignore e))
                        (handler-case (save-environment page)
                          (error (condition) (setf (text message) (princ-to-string condition))))))
    (on revert :clicked (lambda (e) (declare (ignore e)) (show-environment page (shown-environment page))))
    page))

(defun open-environments (workbench &optional environment)
  "Shows the environments tab, opening it the first time."
  (let ((page (environments-page workbench))
        (tabs (workbench-tabs workbench)))
    (unless page
      (setf page (make-environments-page workbench))
      (add tabs page :tab-label "Environments" :tab-tooltip (uiop:native-namestring (api-environment-path))
                     :tab-icon (icon :key :size 14))
      (setf (divider-position (part page :split)) 240)
      (refresh-environment-list page (or environment (current-api-environment workbench))))
    (show-page workbench page)
    page))
