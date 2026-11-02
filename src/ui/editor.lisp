(in-package #:xeon.ui)

;;; The editor view is a native code view on a document's LGT buffer. It adds
;;; editing policy only: language-aware Return, smart Home and completion.

(defclass editor-view (code-view)
  ((document :initarg :document :reader view-document)
   (completion :initform nil :accessor view-completion)
   (completion-timer :initform nil :accessor view-completion-timer)
   (accepting :initform nil :accessor view-accepting))
  (:default-initargs :line-numbers-p t :tab-width 4))

(defmethod measure ((view editor-view)) (values 240 160))

(defun push-selection (view)
  "Gives the backend buffer the view's selection before a backend command."
  (let ((buffer (document-buffer (view-document view))))
    (xeon.core:set-selection buffer (cursor view)
                             (unless (= (cursor view) (anchor view)) (anchor view)))))

(defun pull-selection (view)
  "Shows the backend buffer's selection after a backend command."
  (let* ((buffer (document-buffer (view-document view)))
         (point (xeon.core:buffer-point buffer))
         (anchor (or (xeon.core:buffer-anchor buffer) point)))
    (set-selection view anchor point)))

(defmacro with-backend-selection ((view) &body body)
  "Runs BODY on the backend buffer with VIEW's selection, then shows the result in VIEW."
  `(progn (push-selection ,view)
          (multiple-value-prog1 (progn ,@body)
            (pull-selection ,view))))

(defun plain-key-p (event &rest allowed)
  (null (set-difference (event-modifiers event) allowed)))

(defmethod handle-event ((view editor-view) (event key-event))
  (when (view-completion view)
    (handle-completion-key (view-completion view) event))
  (when (and (eq (event-signal event) :key-down) (not (event-consumed-p event)) (editable view))
    (let ((d (view-document view)))
      (case (event-key event)
        (:return
         (when (plain-key-p event)
           (with-backend-selection (view)
             (xeon.syntax:insert-indented-newline
              (document-syntax d)
              :tab-size (xeon.themes:editor-tab-size (workbench-themes (document-workbench d)))))
           (consume event)))
        (:home
         (when (plain-key-p event :shift)
           (with-backend-selection (view)
             (xeon.core:smart-home (document-buffer d) :extend (modifier-down-p event :shift)))
           (consume event)))
        (#\Space
         (when (equal (event-modifiers event) '(:control))
           (show-completions view :automatic nil)
           (consume event))))))
  (call-next-method))

(defmethod handle-event :after ((view editor-view) (event text-input-event))
  (when (and (eq (event-signal event) :text-input) (not (event-consumed-p event)))
    (schedule-completions view)))

;;; Completion: candidates from the backend's lexical scan of the document,
;;; shown in LGT's completion popup, which leaves focus in the editor.

(defun ensure-completion-popup (view)
  (or (view-completion view)
      (let ((popup (make-instance 'completion-popup :max-visible-rows 10
                                                    :detail-function (constantly ""))))
        (on popup :accept
            (lambda (event)
              (let ((start (getf (user-data popup) :start)))
                (setf (view-accepting view) t)
                (unwind-protect
                     (progn (set-selection view start (cursor view))
                            (replace-selection view (event-value event)))
                  (setf (view-accepting view) nil)))))
        (setf (view-completion view) popup))))

(defun schedule-completions (view)
  (unless (view-accepting view)
    (when (view-completion-timer view)
      (stop-timer (view-completion-timer view)))
    ;; The caret settles after the native edit; read it on the next UI turn.
    (setf (view-completion-timer view)
          (after 0 (lambda ()
                     (setf (view-completion-timer view) nil)
                     (show-completions view :automatic t))))))

(defun show-completions (view &key automatic)
  (when (and (not (destroyed-p view)) (editable view)
             (lisp-document-p (view-document view))
             (or (not automatic) (eq view (focused-widget))))
    (multiple-value-bind (items start)
        (ignore-errors
         (xeon.syntax:buffer-completions (document-buffer (view-document view)) (cursor view)
                                         :minimum-prefix (if automatic 2 1) :limit 60))
      (let ((popup (ensure-completion-popup view)))
        (setf (user-data popup) (list :start start))
        (if items
            (show-completion-popup popup view :items items)
            (when (surface-shown-p popup) (close-surface popup)))))))

(defun release-editor-view (view)
  (when (view-completion-timer view)
    (stop-timer (view-completion-timer view)))
  (when (view-completion view)
    (destroy (view-completion view))
    (setf (view-completion view) nil)))

(defun make-editor-view (workbench document)
  (let ((view (make-instance 'editor-view
                             :document document
                             :buffer (document-text document)
                             :editable (not (xeon.core:buffer-read-only-p (document-buffer document)))
                             :font (editor-font (workbench-themes workbench))
                             :tab-width (xeon.themes:editor-tab-size (workbench-themes workbench))
                             :text-styles (syntax-text-styles (workbench-palette workbench)))))
    (push view (document-views document))
    (restart-highlighting document)
    view))
