(in-package #:xeon.ui)

(defclass documentation-view (web-view)
  ((reference :initform (make-instance 'xeon.core:hyperspec-buffer)
              :reader documentation-reference)))

(defun refresh-documentation-view (workbench view)
  (declare (ignore workbench))
  (setf (html view) (xeon.core:documentation-page (documentation-reference view) :interactive :lgt)
        (tab-label (parent view) view)
        (xeon.core:buffer-name (documentation-reference view))))

(defun show-documentation-record (workbench pair)
  (let* ((tabs (workbench-tabs workbench))
         (view (or (find-if (lambda (page) (typep page 'documentation-view)) (all-tab-pages workbench))
                   (let ((view (make-instance 'documentation-view :local-messages-p t)))
                     (on view :navigation-request
                         (lambda (event)
                           (when (or (event-new-window-p event)
                                     (member (event-navigation-kind event) '(:link :form)))
                             (veto event))))
                     (on view :message
                         (lambda (event)
                           (with-reported-errors (workbench)
                             (let ((message (event-value event))
                                   (reference (documentation-reference view)))
                               (cond ((equal message "action:back")
                                      (xeon.core:navigate-documentation reference :back))
                                     ((equal message "action:forward")
                                      (xeon.core:navigate-documentation reference :forward))
                                     ((and (stringp message) (uiop:string-prefix-p "entry:" message))
                                      (xeon.core:visit-documentation-entry
                                       reference (xeon.core:reference-docset reference) (subseq message 6))))
                               (refresh-documentation-view workbench view)))))
                     (add tabs view :tab-label "Language Reference")
                     view))))
    (xeon.core:visit-documentation-entry (documentation-reference view)
                                        (car pair) (docset:record-id (cdr pair)))
    (refresh-documentation-view workbench view)
    (show-page workbench view)
    (focus view)
    view))

(defun source-documentation-menu (workbench view)
  (let* ((target (xeon.core:hyperspec-target (document-buffer (view-document view))))
         (records (and target (xeon.core:documentation-records target)))
         (menu (make-instance 'menu)))
    ;; Keep every match: libraries can document the same symbol in several ways.
    (setf (menu-items menu)
          (if records
              (loop for pair in records collect
                (let ((record pair))
                  (make-instance 'menu-item
                    :label (format nil "Look Up ~A — ~A (~A)" target
                                   (docset:docset-title (car pair)) (docset:record-id (cdr pair)))
                    :on-activate (lambda (event)
                                   (declare (ignore event))
                                   (with-reported-errors (workbench)
                                     (show-documentation-record workbench record))))))
              (list (make-instance 'menu-item :label "Look Up in HyperSpec — No Documentation Found"
                                             :enabled nil))))
    menu))
