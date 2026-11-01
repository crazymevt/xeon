(in-package #:xeon.core)

(defgeneric insert-text (buffer text &optional position))
(defmethod insert-text ((buffer buffer) text &optional (position (buffer-point buffer)))
  (replace-range buffer position position text))
(defgeneric delete-range (buffer start end))
(defmethod delete-range ((buffer buffer) start end) (replace-range buffer start end ""))

(defgeneric replace-selection (buffer text))
(defmethod replace-selection ((buffer buffer) text)
  (with-edit-group (buffer)
    (multiple-value-bind (start end) (selection-range buffer)
      (replace-range buffer start end text)
      (set-selection buffer (+ start (length text)))))
  buffer)

(defun paired-delimiter (opener)
  "Return the closing delimiter for a selection-wrapping opener, or NIL."
  (case opener (#\' #\') (#\" #\") (#\[ #\]) (#\( #\)) (#\{ #\})))

(defgeneric wrap-selection (buffer opener))
(defmethod wrap-selection ((buffer buffer) opener)
  "Enclose selected text, sharing its stored source. Keep selection and direction.
Return BUFFER when wrapped, NIL for an empty selection or unsupported opener."
  (multiple-value-bind (start end) (selection-range buffer)
    (let ((closer (paired-delimiter opener)) (point (buffer-point buffer)) (anchor (buffer-anchor buffer)))
      (when (and closer (< start end))
        (with-edit-group (buffer)
          ;; Insert at the end first so the original start remains valid. The
          ;; persistent text storage shares the entire enclosed source/history.
          (insert-text buffer (string closer) end)
          (insert-text buffer (string opener) start)
          (set-selection buffer (1+ point) (1+ anchor)))
        buffer))))

(defgeneric delete-backward (buffer &optional count))
(defmethod delete-backward ((buffer buffer) &optional (count 1))
  (check-type count (integer 0 *))
  (multiple-value-bind (start end) (selection-range buffer)
    (if (< start end) (replace-selection buffer "")
        (delete-range buffer (max 0 (- start count)) start))))
(defgeneric delete-forward (buffer &optional count))
(defmethod delete-forward ((buffer buffer) &optional (count 1))
  (check-type count (integer 0 *))
  (multiple-value-bind (start end) (selection-range buffer)
    (if (< start end) (replace-selection buffer "")
        (delete-range buffer start (min (buffer-length buffer) (+ end count))))))

(defgeneric move-point (buffer distance &key extend))
(defmethod move-point ((buffer buffer) distance &key extend)
  (check-type distance integer)
  (set-selection buffer (max 0 (min (buffer-length buffer) (+ (buffer-point buffer) distance)))
                 (when extend (or (buffer-anchor buffer) (buffer-point buffer)))))

(defgeneric position-line-column (buffer position))
(defmethod position-line-column ((buffer buffer) position)
  (check-range buffer position position)
  (let* ((root (%root buffer)) (line (text-newlines-before root position))
         (start (if (zerop line) 0 (1+ (text-nth-newline root (1- line))))))
    (values line (- position start))))

(defgeneric line-column-position (buffer line column))
(defmethod line-column-position ((buffer buffer) line column)
  (check-type line (integer 0 *)) (check-type column (integer 0 *))
  (let ((root (%root buffer)))
    (when (> line (text-lines root))
      (error 'invalid-buffer-range :buffer buffer :start line :end column))
    (let ((start (if (zerop line) 0 (1+ (text-nth-newline root (1- line)))))
          (end (if (< line (text-lines root)) (text-nth-newline root line) (text-size root))))
      (when (> column (- end start))
        (error 'invalid-buffer-range :buffer buffer :start line :end column))
      (+ start column))))

(defun horizontal-whitespace-p (character)
  (member character '(#\Space #\Tab)))

(defun smart-home (buffer &key extend)
  "Move to the first nonblank character, or column zero when already there.
Whitespace-only lines move to column zero. EXTEND preserves the selection anchor."
  (let* ((point (buffer-point buffer))
         (root (%root buffer))
         (line (position-line-column buffer point))
         (start (line-column-position buffer line 0))
         (end (if (< line (text-lines root)) (text-nth-newline root line) (text-size root)))
         (code start))
    (when (and (> end start) (eql (text-char-at root (1- end)) #\Return)) (decf end))
    (loop while (and (< code end) (horizontal-whitespace-p (text-char-at root code)))
          do (incf code))
    (set-selection buffer (if (or (= point code) (= code end)) start code)
                   (when extend (or (buffer-anchor buffer) point)))))

(defun trim-trailing-whitespace (buffer)
  "Remove spaces and tabs at every line end, preserving CRLF and final newlines.
Return the number of changed lines. Unchanged text is shared with history."
  (let ((count 0))
    (with-edit-group (buffer)
      (loop for line downfrom (text-lines (%root buffer)) to 0 do
        (let* ((root (%root buffer))
               (start (line-column-position buffer line 0))
               (end (if (< line (text-lines root)) (text-nth-newline root line) (text-size root))))
          (when (and (> end start) (char= (text-char-at root (1- end)) #\Return)) (decf end))
          (let ((trim end))
            (loop while (and (> trim start) (horizontal-whitespace-p (text-char-at root (1- trim)))) do (decf trim))
            (when (< trim end) (delete-range buffer trim end) (incf count))))))
    count))

(defun join-lines (buffer)
  "Join selected lines, or the current and next line, with one space.
Discard adjacent spaces/tabs and CRLF separators. A selection ending at a
line start excludes that line. Return the number of removed line breaks."
  (multiple-value-bind (start end) (selection-range buffer)
    (let* ((first (position-line-column buffer start))
           (last (if (= start end) (1+ first)
                     (position-line-column buffer (1- end)))))
      (setf last (min last (text-lines (%root buffer))))
      (with-edit-group (buffer)
        (loop for line downfrom (1- last) to first do
          (let* ((root (%root buffer)) (newline (text-nth-newline root line))
                 (left newline) (right (1+ newline))
                 (line-start (line-column-position buffer line 0)))
            (when (and (> left line-start) (char= (text-char-at root (1- left)) #\Return)) (decf left))
            (loop while (and (> left line-start) (horizontal-whitespace-p (text-char-at root (1- left)))) do (decf left))
            (loop while (and (< right (text-size root)) (horizontal-whitespace-p (text-char-at root right))) do (incf right))
            (replace-range buffer left right " "))))
      (max 0 (- last first)))))
