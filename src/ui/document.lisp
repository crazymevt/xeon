(in-package #:xeon.ui)

;;; A document pairs a backend buffer with the LGT text buffer its views show.
;;;
;;; The LGT buffer is what native views edit, and it owns undo, so typing merges
;;; into undo steps as in any Mac text view. Every change is mirrored into the
;;; backend buffer, which saving, syntax, completion and the REPL read. Backend
;;; edits (smart newlines, formatting) are mirrored back into the LGT buffer,
;;; where they become undo steps too.

(defclass document ()
  ((workbench :initarg :workbench :reader document-workbench)
   (buffer :initarg :buffer :reader document-buffer)
   (text :reader document-text)
   (syntax :reader document-syntax)
   (syncing :initform nil :accessor document-syncing)
   (listener :initform nil :accessor document-listener)
   (highlight-timer :initform nil :accessor document-highlight-timer)
   (views :initform '() :accessor document-views)
   ;; For a generated document such as a diff, what it shows, for its tab's tooltip.
   (description :initarg :description :initform nil :accessor document-description)
   (on-change :initarg :on-change :initform nil :accessor document-on-change)))

(defmethod initialize-instance :after ((d document) &key)
  (let* ((buffer (document-buffer d))
         (text (make-text-buffer :text (xeon.core:buffer-text buffer))))
    (setf (slot-value d 'text) text
          (slot-value d 'syntax) (xeon.syntax:make-syntax-session buffer))
    ;; The LGT buffer keeps the undo history.
    (setf (xeon.core:buffer-history-limit buffer) 0)
    (on text :changed (lambda (event) (mirror-text-edit d event)))
    (setf (document-listener d)
          (xeon.core:add-change-listener buffer
            (lambda (source change)
              (declare (ignore source))
              (backend-changed d change))))))

(defun document-name (d)
  (xeon.core:buffer-name (document-buffer d)))

(defun document-path (d)
  (xeon.core:buffer-pathname (document-buffer d)))

(defun document-modified-p (d)
  (xeon.core:buffer-modified-p (document-buffer d)))

(defun document-needs-saving-p (d)
  (xeon.core:buffer-needs-saving-p (document-buffer d)))

(defun document-language (d)
  (xeon.syntax:syntax-language (document-syntax d)))

(defun lisp-document-p (d)
  (member (document-language d) '(:common-lisp :clojure :racket)))

(defun mirror-text-edit (d event)
  "A native or LGT edit: apply it to the backend buffer."
  (unless (document-syncing d)
    (setf (document-syncing d) t)
    (unwind-protect
         (xeon.core:replace-range (document-buffer d) (event-start event) (event-old-end event)
                                  (event-inserted-text event))
      (setf (document-syncing d) nil))))

(defun backend-changed (d change)
  "Every backend change, mirrored or not: restyle, and mirror backend edits into the views."
  (let ((start (xeon.core:buffer-change-start change)))
    (unless (document-syncing d)
      (setf (document-syncing d) t)
      (unwind-protect
           (with-undo-group ((document-text d))
             (replace-buffer-range (document-text d) start
                                   (+ start (length (xeon.core:buffer-change-removed change)))
                                   (xeon.core:buffer-change-inserted change)))
        (setf (document-syncing d) nil)))
    (invalidate-highlighting d start)
    (when (document-on-change d)
      (funcall (document-on-change d) d))))

(defun release-document (d)
  (when (document-highlight-timer d)
    (stop-timer (document-highlight-timer d))
    (setf (document-highlight-timer d) nil))
  (when (document-listener d)
    (xeon.core:remove-change-listener (document-buffer d) (document-listener d))
    (setf (document-listener d) nil)))

;;; Highlighting. The backend lexer works in bounded blocks; each block's runs
;;; replace the runs of the text it covers. Blocks run on later UI turns, a few
;;; milliseconds at a time, so typing never waits for a large file.

(defparameter *highlight-block* 8192)
(defparameter *highlight-slice-seconds* 0.006)

(defun style-runs (start styles)
  (let ((runs '()) (n (length styles)) (i 0))
    (loop while (< i n)
          do (let* ((kind (aref styles i))
                    (end (or (position kind styles :start i :test-not #'=) n)))
               (unless (zerop kind)
                 (push (list (+ start i) (+ start end) :style (aref *syntax-styles* kind)) runs))
               (setf i end)))
    (nreverse runs)))

(defun invalidate-highlighting (d position)
  (let ((session (document-syntax d)))
    (xeon.syntax:invalidate-syntax session position)
    (schedule-highlighting d)))

(defun restart-highlighting (d)
  "Lexes the whole document again, as after a language change or for a new view."
  (dolist (view (document-views d))
    (setf (text-runs view) '()))
  (invalidate-highlighting d 0))

(defun schedule-highlighting (d)
  (unless (document-highlight-timer d)
    (setf (document-highlight-timer d)
          (after 0 (lambda ()
                     (setf (document-highlight-timer d) nil)
                     (when (step-highlighting d)
                       (schedule-highlighting d)))))))

(defun step-highlighting (d)
  "Lexes blocks for a few milliseconds. True while text remains."
  (let ((session (document-syntax d))
        (deadline (+ (get-internal-real-time)
                     (* *highlight-slice-seconds* internal-time-units-per-second))))
    (when (eq :plain (xeon.syntax:syntax-language session))
      (dolist (view (document-views d))
        (setf (text-runs view) '()))
      (return-from step-highlighting nil))
    (loop
      (multiple-value-bind (start bytes chunk styles)
          (xeon.syntax:next-syntax-block session *highlight-block*)
        (declare (ignore bytes))
        (unless chunk (return nil))
        (let ((end (+ start (length styles)))
              (runs (style-runs start styles)))
          (dolist (view (document-views d))
            (unless (destroyed-p view)
              (replace-text-runs view start (min end (text-length view)) runs)))))
      (unless (xeon.syntax:syntax-pending-p session) (return nil))
      (when (> (get-internal-real-time) deadline) (return t)))))
