(in-package #:xeon.core)

(defclass hyperspec-buffer (buffer)
  ((docset :initform nil :accessor reference-docset)
   (entry :initform nil :accessor reference-entry)
   (back :initform nil :accessor reference-back)
   (forward :initform nil :accessor reference-forward)))

(defvar *documentation-docsets* :unread)
(defun documentation-docsets ()
  "Load the same format and discovery sources as Cadre, once on first lookup."
  (when (eq *documentation-docsets* :unread)
    (setf *documentation-docsets*
          (docset:find-docsets
           :systems (adjoin "lgt" docset:*library-systems* :test #'equal)
           :directories (cons (asdf:system-relative-pathname "xeon" "data/docsets/")
                              docset:*docset-directories*))))
  *documentation-docsets*)

(defun documentation-records (token)
  "Resolve package/name, then fall back to matching names as Cadre does."
  (let* ((colon (position #\: token :from-end t))
         (name (if colon (subseq token (1+ colon)) token))
         (prefix (and colon (string-right-trim ":" (subseq token 0 colon))))
         (package (if (or (null prefix) (member prefix '("" "CL" "KEYWORD") :test #'string-equal))
                      "COMMON-LISP" prefix))
         (docsets (documentation-docsets)))
    (or (docset:lookup-symbol docsets name package)
        (loop for ds in docsets append
          (loop for record in (docset:find-records ds name) collect (cons ds record))))))

(defun hyperspec-target (buffer)
  "Resolve a selection or token against loaded docsets without reading Lisp."
  (labels ((delimiter-p (c) (find c '(#\Space #\Tab #\Newline #\Return #\( #\) #\" #\' #\` #\, #\;)))
           (char-at (p) (char (buffer-substring buffer p (1+ p)) 0)))
    (multiple-value-bind (start end) (selection-range buffer)
      (when (= start end)
        (when (and (plusp start) (or (= start (buffer-length buffer)) (delimiter-p (char-at start))))
          (decf start))
        (setf end start)
        (loop while (and (plusp start) (not (delimiter-p (char-at (1- start))))) do (decf start))
        (loop while (and (< end (buffer-length buffer)) (not (delimiter-p (char-at end)))) do (incf end)))
      (when (<= 1 (- end start) 256)
        (let ((token (string-upcase (string-trim '(#\Space #\Tab #\( #\)) (buffer-substring buffer start end)))))
          (when (documentation-records token) token))))))

(defun visit-documentation-entry (buffer ds id &key (record t))
  (let ((entry (docset:docset-entry ds id)))
    (unless entry (error "No documentation entry ~A in ~A" id (docset:docset-title ds)))
    (when (and record (reference-entry buffer))
      (push (cons (reference-docset buffer) (docset:entry-id (reference-entry buffer))) (reference-back buffer))
      (setf (reference-forward buffer) nil))
    (setf (reference-docset buffer) ds (reference-entry buffer) entry
          (buffer-name buffer) (format nil "~A · ~A" (docset:docset-title ds) (first (docset:entry-names entry))))
    entry))

(defun navigate-documentation (buffer direction)
  (let* ((back-p (eq direction :back))
         (target (if back-p (pop (reference-back buffer)) (pop (reference-forward buffer)))))
    (when target
      (let ((here (cons (reference-docset buffer) (docset:entry-id (reference-entry buffer)))))
        (if back-p (push here (reference-forward buffer)) (push here (reference-back buffer))))
      (visit-documentation-entry buffer (car target) (cdr target) :record nil))))
