(in-package #:xeon.ui)

;;; Documents in the editor tabs: new, open, save, close, with the usual
;;; unsaved-changes protection.

;;; Each tab's page holds a document's editor and, for Markdown, an optional preview
;;; beside it.

(defclass document-page (paned)
  ((editor :initarg :editor :reader page-editor)
   (preview :initform nil :accessor page-preview)
   (preview-timer :initform nil :accessor page-preview-timer))
  (:default-initargs :orientation :horizontal))

(defun page-document (page) (view-document (page-editor page)))

(defun active-page (workbench)
  (let ((page (selected-page (workbench-tabs workbench))))
    (and page (typep page 'document-page) (not (destroyed-p page)) page)))

(defun active-editor (workbench)
  (let ((page (active-page workbench)))
    (and page (page-editor page))))

(defun active-document (workbench)
  (let ((view (active-editor workbench)))
    (and view (view-document view))))

(defun find-document-page (workbench d)
  (find-if (lambda (page) (and (typep page 'document-page) (eq (page-document page) d)))
           (children (workbench-tabs workbench))))

(defun document-tab-tooltip (d)
  (let ((path (document-path d)))
    (cond (path (uiop:native-namestring path))
          ((document-description d))
          (t "Not saved yet"))))

(defun update-document-tab (workbench d)
  (let ((page (find-document-page workbench d))
        (tabs (workbench-tabs workbench)))
    (when page
      (unless (equal (tab-label tabs page) (document-name d))
        (setf (tab-label tabs page) (document-name d)))
      (setf (tab-tooltip tabs page) (document-tab-tooltip d))
      ;; The unsaved-changes dot is Xeon's own accessory in the tab.
      (let ((modified (and (document-modified-p d) t))
            (accessory (tab-accessory tabs page)))
        (unless (eq modified (and accessory t))
          (setf (tab-accessory tabs page)
                (when modified
                  (make-instance 'label :text "●" :text-color :accent
                                        :font (make-font :size 9) :valign :center))))))))

(defun document-changed (workbench d)
  (update-document-tab workbench d)
  (let ((page (find-document-page workbench d)))
    (when page (schedule-preview workbench page)))
  (when (eq d (active-document workbench))
    (update-title workbench)
    (refresh-status workbench)))

(defvar *background-document-operation* nil
  "True while assistant tools update documents without changing the user's tab or focus.")

(defun add-document (workbench buffer)
  (when (member (pathname-type (or (xeon.core:buffer-pathname buffer) "")) '("http" "rest") :test #'equalp)
    (return-from add-document (page-document (open-api-workspace workbench :buffer buffer))))
  (let* ((d (make-instance 'document :workbench workbench :buffer buffer
                                     :on-change (lambda (d) (document-changed workbench d))))
         (view (make-editor-view workbench d))
         (page (make-instance 'document-page :editor view))
         (tabs (workbench-tabs workbench)))
    (add page view)
    (setf (workbench-documents workbench) (append (workbench-documents workbench) (list d)))
    (add tabs page :tab-label (document-name d) :tab-tooltip (document-tab-tooltip d))
    (on view :selection-changed (lambda (e) (declare (ignore e))
                                  (when (eq view (active-editor workbench)) (refresh-position workbench))))
    (unless *background-document-operation*
      (select-page tabs page)
      (focus view))
    d))

(defun show-document (workbench d)
  (let ((page (and d (find-document-page workbench d))))
    (when (and page (not *background-document-operation*))
      (select-page (workbench-tabs workbench) page)
      (focus (page-editor page)))
    d))

(defun new-document (workbench)
  "Opens a new untitled document."
  (let* ((n (incf (workbench-untitled-count workbench)))
         (name (if (= n 1) "untitled" (format nil "untitled-~D" n)))
         (buffer (xeon.core:new-buffer (workbench-manager workbench) :name name :history-limit 0)))
    (add-document workbench buffer)))

(defun disposable-document (workbench)
  "The only document, when it is an empty unchanged untitled one that opening a file can replace."
  (let ((documents (workbench-documents workbench)))
    (and (= 1 (length documents))
         (xeon.core:disposable-untitled-buffer-p (document-buffer (first documents)))
         (first documents))))

(defun open-document (workbench path)
  "Opens the file PATH, or shows it if it is already open."
  (let* ((path (uiop:ensure-pathname path :want-file t))
         (existing (find-if (lambda (d)
                              (let ((p (document-path d)))
                                (and p (equal (ignore-errors (truename p)) (ignore-errors (truename path))))))
                            (workbench-documents workbench))))
    (if existing
        (show-document workbench existing)
        (let ((scratch (and (not *background-document-operation*) (disposable-document workbench)))
              (buffer (xeon.core:open-managed-file (workbench-manager workbench) path)))
          (prog1 (add-document workbench buffer)
            (when scratch (close-document workbench scratch t))
            (remember-file workbench path))))))

(defun trim-on-save-p (workbench &optional document)
  ;; HTTP bodies are payloads: an editor preference must not change their bytes.
  (and (not (typep (and document (find-document-page workbench document)) 'api-page))
       (xeon.themes:trim-trailing-whitespace-on-save (workbench-themes workbench))))

(defun save-document (workbench &optional (d (active-document workbench)))
  "Saves D. Returns true if it was saved."
  (unless (api-draft-ready workbench d) (return-from save-document nil))
  (cond ((null d) nil)
        ((null (document-path d)) (save-document-as workbench d))
        (t
         (handler-case
             (progn (xeon.core:save-buffer (document-buffer d) :trim-whitespace (trim-on-save-p workbench d))
                    (saved workbench d)
                    t)
           (xeon.core:file-conflict (condition)
             (declare (ignore condition))
             (when (eq :overwrite
                       (message-box (format nil "~A changed on disk." (document-name d))
                                    :detail "Another program changed or removed the file since Xeon read it. Overwrite it with this version?"
                                    :type :warning :parent (workbench-window workbench)
                                    :buttons '(("Cancel" . :cancel) ("Overwrite" . :overwrite))))
               (xeon.core:save-buffer (document-buffer d) :overwrite-external t
                                                          :trim-whitespace (trim-on-save-p workbench d))
               (saved workbench d)
               (return-from save-document t))
             nil)))))

(defun save-document-as (workbench &optional (d (active-document workbench)))
  "Asks where to save D, then saves it there. Returns true if it was saved."
  (unless (api-draft-ready workbench d) (return-from save-document-as nil))
  (when d
    (let ((path (choose-file :mode :save :parent (workbench-window workbench)
                             :name (document-name d)
                             :directory (or (and (document-path d) (uiop:pathname-directory-pathname (document-path d)))
                                            (explorer-folder workbench)))))
      (when path
        (xeon.core:save-buffer-as (document-buffer d) path :if-exists :overwrite
                                                           :trim-whitespace (trim-on-save-p workbench d))
        (saved workbench d)
        (remember-file workbench path)
        ;; The name decides the language.
        (restart-highlighting d)
        t))))

(defun saved (workbench d)
  (update-document-tab workbench d)
  (when (typep (find-document-page workbench d) 'api-page) (refresh-api-sidebar workbench))
  (update-title workbench)
  (refresh-status workbench)
  (refresh-source-control workbench)
  (report workbench "Saved ~A" (document-name d)))

(defun save-all-documents (workbench)
  (unless (every (lambda (d) (api-draft-ready workbench d)) (workbench-documents workbench))
    (return-from save-all-documents nil))
  (dolist (d (workbench-documents workbench))
    (when (document-needs-saving-p d)
      (unless (save-document workbench d) (return)))))

(defun confirm-closing (workbench d)
  "Asks whether to save D's changes. True when D may close."
  (unless (api-draft-ready workbench d) (return-from confirm-closing nil))
  (if (not (document-needs-saving-p d))
      t
      (progn
        (show-document workbench d)
        (case (message-box (format nil "Do you want to save the changes to ~A?" (document-name d))
                           :detail "Your changes will be lost if you don't save them."
                           :type :warning :buttons :save-discard-cancel
                           :parent (workbench-window workbench))
          (:save (with-reported-errors (workbench) (save-document workbench d)))
          (:discard t)
          (t nil)))))

(defun close-document (workbench &optional (d (active-document workbench)) force)
  "Closes D, asking to save its changes unless FORCE."
  (let ((page (and d (find-document-page workbench d))))
    (when (and page (or force (confirm-closing workbench d)))
      (close-page (workbench-tabs workbench) page :force t)
      t)))

(defun close-active-tab (workbench)
  "Closes the selected tab: a document, asking about unsaved changes, or a chat."
  (let ((page (selected-page (workbench-tabs workbench))))
    (cond ((typep page 'document-page) (close-document workbench (page-document page)))
          (page (close-page (workbench-tabs workbench) page)))))

(defun forget-document (workbench d)
  "After D's tab closed: release it, and keep one document open."
  (setf (workbench-documents workbench) (remove d (workbench-documents workbench)))
  (release-document d)
  (ignore-errors (xeon.core:close-buffer (workbench-manager workbench) (document-buffer d)
                                         :if-modified :discard))
  (unless (children (workbench-tabs workbench))
    (new-document workbench))
  (update-title workbench)
  (refresh-status workbench))

(defun confirm-quitting (workbench)
  (and (or (notany (lambda (page)
                     (let ((session (chat-session page)))
                       (and session
                            (member (getf (xeon.assistants:session-snapshot session) :status)
                                    '(:starting :busy :waiting)))))
                   (chat-pages workbench))
           (confirm "An AI agent is still working. Quit anyway?"
                    :detail "Quitting will interrupt active agents, including those waiting for your input."
                    :yes "Quit Anyway" :parent (workbench-window workbench)))
       (every (lambda (d) (confirm-closing workbench d)) (workbench-documents workbench))))

;;; Recent files and folders

(defun remember-file (workbench path)
  (ignore-errors (xeon.themes:remember-workspace-path (workbench-history workbench) :file path)))

(defun remember-folder (workbench path)
  (ignore-errors (xeon.themes:remember-workspace-path (workbench-history workbench) :folder path)))
