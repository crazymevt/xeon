(in-package #:xeon.ui)

;;; The window: activity rail, explorer sidebar, editor tabs, runtime tools and
;;; status bar.

(defun update-title (workbench)
  (let ((d (active-document workbench)))
    (setf (title (workbench-window workbench))
          (format nil "~@[~A — ~]Xeon" (and d (document-name d))))))

;;; Status bar

(defclass status-bar ()
  ((document :accessor status-document)
   (message :accessor status-message)
   (position :accessor status-position)
   (language :accessor status-language)
   (message-timer :initform nil :accessor status-message-timer)
   (box :accessor status-box)))

(defun status-label (&rest initargs)
  (apply #'make-instance 'label :text "" :text-color :secondary-text :font (make-font :size 11)
                                :valign :center initargs))

(defun language-label (id)
  (or (second (assoc id (xeon.syntax:language-choices))) (string-capitalize id)))

(defun detected-language (d)
  (let ((buffer (document-buffer d)))
    (let ((override (xeon.core:buffer-language-override buffer)))
      (unwind-protect
           (progn (setf (xeon.core:buffer-language-override buffer) nil)
                  (xeon.syntax:detect-language buffer))
        (setf (xeon.core:buffer-language-override buffer) override)))))

(defun refresh-language-choice (workbench)
  (let* ((status (workbench-status workbench))
         (combo (status-language status))
         (d (active-document workbench)))
    (with-handlers-blocked (combo :changed)
      (if (null d)
          (setf (enabled combo) nil)
          (let* ((override (xeon.core:buffer-language-override (document-buffer d)))
                 (ids (cons :automatic (mapcar #'first (xeon.syntax:language-choices))))
                 (automatic (format nil "~A (Automatic)" (language-label (detected-language d)))))
            (setf (label-function combo)
                  (lambda (id) (if (eq id :automatic) automatic (language-label id))))
            (unless (equal ids (items combo)) (setf (items combo) ids))
            (setf (enabled combo) t
                  (selected-index combo) (or (and override (position override ids)) 0)))))))

(defun refresh-position (workbench)
  (let* ((view (active-editor workbench))
         (label (status-position (workbench-status workbench))))
    (setf (text label)
          (if view
              (let* ((buffer (buffer view)) (p (cursor view))
                     (selected (abs (- (anchor view) p))))
                (format nil "Ln ~:D, Col ~D~[~:;  (~:*~:D selected)~]"
                        (1+ (position-line buffer p)) (1+ (position-column buffer p)) selected))
              ""))))

(defun refresh-status (workbench)
  (let ((d (active-document workbench))
        (status (workbench-status workbench)))
    (setf (text (status-document status))
          (if d
              (format nil "~A  ·  ~A  ·  ~:D character~:P"
                      (let ((path (document-path d)))
                        (if path (document-name d) "Untitled document"))
                      (cond ((xeon.core:buffer-read-only-p (document-buffer d)) "Read only")
                            ((null (document-path d)) "New document")
                            ((document-modified-p d) "Edited")
                            (t "Saved"))
                      (xeon.core:buffer-length (document-buffer d)))
              ""))
    (refresh-position workbench)
    (refresh-language-choice workbench)))

(defun refresh-language-status (workbench)
  (declare (ignore workbench)))

(defun set-status-message (workbench text)
  (let ((status (workbench-status workbench)))
    (when (status-message-timer status) (stop-timer (status-message-timer status)))
    (setf (text (status-message status)) text
          (status-message-timer status)
          (after 8 (lambda ()
                     (setf (status-message-timer status) nil)
                     (unless (destroyed-p (status-message status))
                       (setf (text (status-message status)) "")))))))

(defun make-status-bar (workbench)
  (let* ((status (make-instance 'status-bar))
         (language (make-instance 'combo-box :control-size :small :tooltip "Document language"))
         (box (ui (hbox :surface :status :padding '(3 12 3 12) :spacing 16
                    (:widget (setf (status-document status) (status-label)))
                    (:widget (setf (status-message status) (status-label :text-color :accent)))
                    (spacer)
                    (:widget (setf (status-position status) (status-label)))
                    (:widget language)))))
    (setf (status-language status) language (status-box status) box
          (workbench-status workbench) status)
    (on language :changed
        (lambda (event)
          (let ((d (active-document workbench))
                (id (event-value event)))
            (when (and d id)
              (xeon.syntax:set-buffer-language (document-buffer d) (unless (eq id :automatic) id))
              (restart-highlighting d)
              (refresh-language-choice workbench)))))
    box))

;;; Panes

(defun pane-shown-p (pane) (visible pane))

(defun toggle-sidebar (workbench &optional (show nil supplied))
  (let* ((pane (workbench-sidebar workbench))
         (show (if supplied show (not (pane-shown-p pane)))))
    (unless (eq show (pane-shown-p pane))
      (setf (visible pane) show)
      ;; A native split view keeps a hidden pane's space; move the divider too.
      (setf (divider-position (workbench-outer-split workbench))
            (if show (workbench-sidebar-width workbench) 0))
      (update-layout (workbench-window workbench))
      (refresh-rail workbench))
    show))

(defun toggle-tools (workbench &optional (show nil supplied))
  (let* ((pane (workbench-tools workbench))
         (show (if supplied show (not (pane-shown-p pane)))))
    (unless (eq show (pane-shown-p pane))
      (setf (visible pane) show)
      (if show
          (place-tools-divider workbench)
          (let ((split (workbench-inner-split workbench)))
            (when (plusp (widget-width split))
              (setf (divider-position split) (widget-width split)))))
      (update-layout (workbench-window workbench))
      (refresh-rail workbench))
    show))

(defun show-tools (workbench) (toggle-tools workbench t))

(defun place-tools-divider (workbench)
  (let* ((split (workbench-inner-split workbench))
         (width (widget-width split)))
    (when (and width (> width 0))
      (setf (divider-position split)
            (max 320 (- width (workbench-tools-width workbench)))))))

;;; Rail

(defun rail-button (glyph label tooltip action)
  (let ((button (make-instance 'button :style :icon :label label :tooltip tooltip
                                       :image (icon glyph :size 24) :control-size :large
                                       :width 40 :height 40)))
    (setf (user-data button) glyph)
    (on button :clicked (lambda (e) (declare (ignore e)) (funcall action)))
    button))

(defun rail-buttons (workbench)
  (remove-if-not (lambda (w) (typep w 'button)) (children (workbench-rail workbench))))

(defun refresh-rail (workbench)
  "Shows the rail icons of shown panes bright, and the others dimmed."
  (let ((dim (if (palette-dark-p (workbench-palette workbench)) "#7C7C7C" "#8D96A3")))
    (dolist (button (rail-buttons workbench))
      (let* ((glyph (user-data button))
             (active (case glyph
                       (:explorer (visible (workbench-sidebar workbench)))
                       (:terminal (visible (workbench-tools workbench))))))
        (setf (image button) (if (or active (eq glyph :settings))
                                 (icon glyph :size 24)
                                 (icon glyph :size 24 :color dim)))))))

(defun make-rail (workbench)
  (let ((settings (rail-button :settings "Settings" "Settings" (lambda () (show-settings workbench)))))
    (ui (vbox :surface :activity :padding '(10 8 12 8) :spacing 12
          (:widget (rail-button :explorer "Explorer" (format nil "Explorer · ~A" (shortcut-label "Primary+B"))
                                (lambda () (toggle-sidebar workbench))))
          (:widget (rail-button :terminal "Runtime tools" (format nil "Toggle runtime tools · ~A" (shortcut-label "Primary+J"))
                                (lambda () (toggle-tools workbench))))
          (spacer)
          (:widget settings)))))

;;; Tabs

(defun tab-context-menu (workbench d)
  (ui (menu
        (menu-item :label "Close Tab"
                   :on-activate (lambda (e) (declare (ignore e)) (close-document workbench d)))
        (menu-item :label "Close Other Tabs"
                   :on-activate (lambda (e) (declare (ignore e))
                                  (dolist (other (remove d (workbench-documents workbench)))
                                    (unless (close-document workbench other) (return)))))
        (menu-separator)
        (menu-item :label "Copy Path" :enabled (and (document-path d) t)
                   :on-activate (lambda (e) (declare (ignore e))
                                  (setf (clipboard-text) (uiop:native-namestring (document-path d)))))
        (menu-item :label "Reveal in Finder" :enabled (and (document-path d) t)
                   :on-activate (lambda (e) (declare (ignore e))
                                  (open-url (uiop:pathname-directory-pathname (document-path d))))))))

(defun make-editor-tabs (workbench)
  (let* ((tabs (make-instance 'tabbed-pane :reorderable t))
         (add-button (make-instance 'button :style :icon :label "New Document"
                                            :tooltip (format nil "New document · ~A" (shortcut-label "Primary+N"))
                                            :image (icon :add :size 16) :control-size :small)))
    (add (tab-bar-end (tab-bar tabs)) add-button)
    (on add-button :clicked (lambda (e) (declare (ignore e)) (new-document workbench)))
    (on tabs :page-changed
        (lambda (e)
          (let ((page (event-value e)))
            (when (typep page 'editor-view)
              (ignore-errors
               (xeon.core:switch-buffer (workbench-manager workbench) (document-buffer (view-document page))))))
          (update-title workbench)
          (refresh-status workbench)))
    (on tabs :page-close-request
        (lambda (e)
          (let ((page (event-value e)))
            (when (typep page 'editor-view)
              (unless (confirm-closing workbench (view-document page))
                (veto e))))))
    (on tabs :page-closed
        (lambda (e)
          (let ((page (event-value e)))
            (when (typep page 'editor-view)
              (release-editor-view page)
              (let ((d (view-document page)))
                (setf (document-views d) (remove page (document-views d)))
                (unless (document-views d) (forget-document workbench d)))))))
    (on tabs :tab-context-request
        (lambda (e)
          (let ((page (event-page e)))
            (when (typep page 'editor-view)
              (popup-menu (tab-context-menu workbench (view-document page))
                          (tab-bar tabs) (event-x e) (event-y e))
              (consume e)))))
    tabs))

;;; Window frame persistence

(defun frame-path ()
  (merge-pathnames "window.sexp" (xeon.themes:default-configuration-directory)))

(defun save-frame (workbench)
  (ignore-errors
   (let ((path (frame-path)))
     (ensure-directories-exist path)
     (with-open-file (out path :direction :output :if-exists :supersede)
       (with-standard-io-syntax
         (prin1 (list :frame (coerce (window-frame-string (workbench-window workbench)) '(simple-array character (*)))
                      :sidebar (workbench-sidebar-width workbench)
                      :tools (workbench-tools-width workbench)
                      :sidebar-shown (and (visible (workbench-sidebar workbench)) t)
                      :tools-shown (and (visible (workbench-tools workbench)) t))
                out))))))

(defun load-frame ()
  (ignore-errors
   (with-open-file (in (frame-path))
     (with-standard-io-syntax
       (let ((*read-eval* nil)) (read in))))))

;;; Theme

(defun apply-theme (workbench)
  "Applies the selected theme and editor settings to the window and every view."
  (let* ((themes (workbench-themes workbench))
         (palette (resolve-palette themes))
         (styles (syntax-text-styles palette))
         (font (editor-font themes))
         (tab-width (xeon.themes:editor-tab-size themes)))
    (setf (workbench-palette workbench) palette)
    (setf (appearance (workbench-window workbench)) (theme-appearance themes palette))
    (dolist (d (workbench-documents workbench))
      (dolist (view (document-views d))
        (setf (text-styles view) styles (font view) font (tab-width view) tab-width)))
    (let ((tools (workbench-repl workbench)))
      (when tools
        (dolist (name '(:transcript :input))
          (setf (font (tool tools name)) font))
        (setf (text-styles (tool tools :transcript)) styles)))
    (let ((explorer (workbench-explorer workbench)))
      (when (and explorer (explorer-root explorer))
        (refresh-tree (explorer-tree explorer))))
    (refresh-rail workbench)
    (let ((tools (workbench-repl workbench)))
      (when tools
        (let ((shown (find-if (lambda (entry) (checked (second entry))) (tools-switches tools))))
          (when shown (select-tool workbench (first shown) :show nil)))))))

;;; Building the window

(defun default-folder (file)
  "The launch directory, unless FILE lies outside it; then FILE's own directory."
  (let ((cwd (uiop:getcwd)))
    (if (and file (not (uiop:subpathp (merge-pathnames file cwd) cwd)))
        (uiop:pathname-directory-pathname (merge-pathnames file cwd))
        cwd)))

(defun make-workbench (&key folder file (themes (make-theme-manager))
                         (history (xeon.themes:load-workspace-history)) (remember-frame t))
  "Builds a Xeon window, opening FOLDER in the explorer and FILE in the editor.
REMEMBER-FRAME NIL neither restores nor saves the window frame and pane layout."
  (let* ((workbench (make-instance 'workbench :themes themes :history history))
         (*workbench* workbench))
    (setf (workbench-palette workbench) (resolve-palette themes))
    (let* ((rail (make-rail workbench))
           (sidebar (ui (vbox :surface :sidebar :spacing 0
                          (:widget (make-explorer-panel workbench) :expand t))))
           (tabs (make-editor-tabs workbench))
           (tools (ui (vbox :surface :sidebar :spacing 0
                        (:widget (make-tools-panel workbench) :expand t))))
           (inner (ui (paned (:widget tabs) (:widget tools))))
           (outer (ui (paned :position (workbench-sidebar-width workbench)
                        (:widget sidebar) (:widget inner))))
           (status (make-status-bar workbench))
           (window (ui (window :title "Xeon" :width 1280 :height 800
                         :appearance (theme-appearance themes (workbench-palette workbench))
                         (vbox :spacing 0
                           (hbox :spacing 0 :vexpand t
                             (:widget rail)
                             (separator :orientation :vertical)
                             (:widget outer :expand t))
                           (separator)
                           (:widget status))))))
      (setf (workbench-window workbench) window (workbench-rail workbench) rail
            (workbench-sidebar workbench) sidebar (workbench-tabs workbench) tabs
            (workbench-tools workbench) tools (workbench-inner-split workbench) inner
            (workbench-outer-split workbench) outer)
      (setf (menu-bar window) (make-menus workbench))
      (install-shortcuts workbench)
      (on outer :divider-moved
          (lambda (e) (when (visible sidebar) (setf (workbench-sidebar-width workbench) (max 160 (event-value e))))))
      (on inner :divider-moved
          (lambda (e)
            (when (visible tools)
              (let ((width (widget-width inner)))
                (setf (workbench-tools-width workbench) (max 280 (- width (event-value e))))))))
      (on window :close-request
          (lambda (e) (unless (confirm-quitting workbench) (veto e))))
      (on window :closed
          (lambda (e) (declare (ignore e))
            (when remember-frame (save-frame workbench))
            (stop-tools workbench)
            (dolist (d (workbench-documents workbench)) (release-document d))))
      ;; Restore the last window frame and pane widths.
      (let ((saved (and remember-frame (load-frame))))
        (when (getf saved :sidebar) (setf (workbench-sidebar-width workbench) (getf saved :sidebar)))
        (when (getf saved :tools) (setf (workbench-tools-width workbench) (getf saved :tools)))
        (when (getf saved :frame) (ignore-errors (restore-window-frame window (getf saved :frame))))
        (setf (divider-position outer) (workbench-sidebar-width workbench))
        (when (and saved (not (getf saved :sidebar-shown))) (toggle-sidebar workbench nil))
        (when (and saved (not (getf saved :tools-shown))) (toggle-tools workbench nil)))
      (on window :shown
          (lambda (e) (declare (ignore e))
            (when (visible tools) (place-tools-divider workbench))))
      (with-reported-errors (workbench)
        (open-folder workbench (or folder (default-folder file))))
      (if file
          (or (with-reported-errors (workbench) (open-document workbench file))
              (new-document workbench))
          (new-document workbench))
;; Workaround for an LGT Cocoa bug: a tree fits its column to the table before
      ;; the table has its final frame, leaving names cut short. Moving the divider
      ;; once the window has settled lays the sidebar out again with real sizes.
      (after 0.3 (lambda ()
                   (when (and (not (destroyed-p outer)) (visible sidebar))
                     (let ((width (workbench-sidebar-width workbench)))
                       (setf (divider-position outer) (1+ width))
                       (setf (divider-position outer) width)))))
      (select-tool workbench :repl :show nil)
      (refresh-rail workbench)
      (update-title workbench)
      (refresh-status workbench)
      workbench)))
