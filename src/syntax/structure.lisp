(in-package #:xeon.syntax)

(defun matching-brackets (session position)
  "Return adjacent bracket position, partner, and :MATCHED/:UNMATCHED/:PENDING.
Uses the incremental lexical index; never scans the document on caret moves."
  (syntax-pending-p session)
  (let ((matches (syntax-session-matches session)))
    (dolist (candidate (list (1- position) position))
      (multiple-value-bind (partner present) (gethash candidate matches)
        (when present
          (return-from matching-brackets
            (values candidate partner
                    (cond (partner :matched)
                          ((and (syntax-pending-p session)
                                (find (char (xeon.core:buffer-substring (syntax-session-buffer session) candidate (1+ candidate)) 0) "([{")) :pending)
                          (t :unmatched)))))))
    (values nil nil (when (and (syntax-pending-p session) (>= position (syntax-position session))) :pending))))

(defun syntax-state-at (session position)
  ;; Normally the idle lexer has already reached the caret. Bound catch-up work
  ;; when Enter arrives immediately after opening/jumping into a large file.
  (syntax-pending-p session)
  (let* ((points (syntax-session-checkpoints session))
         (index (loop with low = 0 with high = (length points)
                      while (< low high) for middle = (floor (+ low high) 2)
                      do (if (<= (checkpoint-position (aref points middle)) position)
                             (setf low (1+ middle)) (setf high middle))
                      finally (return (max 0 (1- low)))))
         (point (aref points index)) (start (checkpoint-position point))
         (state (copy-lex-state (checkpoint-state point))))
    (when (> (- position start) 65536) (return-from syntax-state-at nil))
    (loop while (< start position) do
      (let* ((end (min position (+ start 4096)))
             (text (xeon.core:buffer-substring (syntax-session-buffer session) start (min position (+ end 256)))))
        (multiple-value-bind (chunk styles next) (style-chunk text (- end start) state (syntax-language session) start)
          (declare (ignore styles)) (setf state next) (incf start (length chunk)))))
    state))

(defun line-prefix (buffer position)
  (multiple-value-bind (line column) (xeon.core:position-line-column buffer position)
    (declare (ignore column))
    ;; Leading whitespace and form lookahead are bounded even for giant lines.
    (let ((start (xeon.core:line-column-position buffer line 0)))
      (values (xeon.core:buffer-substring buffer start (min position (+ start 4096))) start))))

(defun whitespace-prefix (text)
  (subseq text 0 (position-if-not (lambda (c) (find c '(#\Space #\Tab))) text)))
(defun visual-width (text tab-size)
  (loop with column = 0 for c across text do
    (incf column (if (eql c #\Tab) (- tab-size (mod column tab-size)) 1)) finally (return column)))

(defun markup-element-indent (buffer stack tab-size)
  "Use the opener line's indentation, accounting for nested tags on that line."
  (multiple-value-bind (prefix start) (line-prefix buffer (second (first stack)))
    (let ((extra 0))
      (loop for entry in (rest stack)
            while (and (>= (second entry) start) (< extra 4096)) do (incf extra tab-size))
      (min 4096 (+ (visual-width (whitespace-prefix prefix) tab-size) extra)))))

(defun markup-closing-name (text start)
  "Read a bounded closing tag, requiring a name boundary and a final >."
  (when (and start (< (+ start 2) (length text))
             (string= "</" text :start2 start :end2 (+ start 2)))
    (let* ((name-start (+ start 2))
           (end (position-if (lambda (c) (or (spacep c) (find c "/>"))) text :start name-start)))
      (when (and end (<= (- end name-start) 64) (position #\> text :start end))
        (string-downcase (subseq text name-start end))))))

(defun markup-newline-indentation (buffer state position end-position base tab-size)
  "Return handled-p, inner/outer indentation, and skipped horizontal whitespace."
  (declare (ignore position))
  (let* ((stack (lex-state-markup-stack state)) (mode (lex-state-mode state))
         (embedded (lex-state-embedded state))
         (literal (or (eq mode :markup-comment)
                      (and (eq mode :string) (eq (lex-state-return-mode state) :markup-tag))))
         (preserved (markup-whitespace-preserved-p state))
         (suffix (xeon.core:buffer-substring buffer end-position
                                                  (min (xeon.core:buffer-length buffer) (+ end-position 4096))))
         (closing (position-if-not (lambda (c) (find c '(#\Space #\Tab))) suffix))
         (closing-name (unless (or literal preserved) (markup-closing-name suffix closing)))
         (closing-stack (when closing-name (member closing-name stack :key #'first :test #'equal))))
    (cond
      ((or literal preserved) (values t base nil 0))
      ((and (eq mode :markup-tag) (not (lex-state-markup-closing state)))
       ;; Attribute continuation outside a quoted value. The pending opener is
       ;; not on the completed-element stack until its final > is consumed.
       (multiple-value-bind (prefix start) (line-prefix buffer (lex-state-markup-start state))
         (declare (ignore start))
         (let* ((outer (visual-width (whitespace-prefix prefix) tab-size))
                (end-tag (and closing (or (eql (char suffix closing) #\>)
                                         (and (< (1+ closing) (length suffix))
                                              (string= "/>" suffix :start2 closing :end2 (+ closing 2)))))))
           (values t (make-string (+ outer (if end-tag 0 tab-size)) :initial-element #\Space) nil 0))))
      (closing-stack
       (values t (make-string (+ (markup-element-indent buffer stack tab-size) tab-size) :initial-element #\Space)
               (make-string (markup-element-indent buffer closing-stack tab-size) :initial-element #\Space) closing))
      ;; Within guest code, existing bracket and literal rules take precedence.
      ((and embedded (or (lex-state-bracket-stack state) (member mode '(:string :block-comment :character))))
       (values nil nil nil 0))
      (stack (values t (make-string (+ (markup-element-indent buffer stack tab-size) tab-size) :initial-element #\Space) nil 0))
      (t (values t base nil 0)))))

(defun newline-indentation (session position &key (tab-size 4) (end-position position))
  "Return indentation before/after a split pair. All positions count characters.
Lisp body forms indent two columns; other blocks use TAB-SIZE. An unavailable
lexical suffix falls back to the current line's leading whitespace."
  (check-type tab-size (integer 1 32))
  (let* ((buffer (syntax-session-buffer session)) (state (syntax-state-at session position))
         (language (if state (or (lex-state-embedded state) (syntax-language session)) (syntax-language session))))
    (multiple-value-bind (prefix line-start) (line-prefix buffer position)
      (when (and state (eq (syntax-language session) :markup))
        (multiple-value-bind (handled inner outer skipped)
            (markup-newline-indentation buffer state position end-position (whitespace-prefix prefix) tab-size)
          (when handled (return-from newline-indentation (values inner outer skipped)))))
      (let* ((base (whitespace-prefix prefix)) (width (visual-width base tab-size))
             (open (when state (first (lex-state-bracket-stack state))))
             (inside-literal (and state (member (lex-state-mode state) '(:string :block-comment :markup-comment :fence :bar-symbol :character :js-regex :c-raw-string :preprocessor :cdata))))
             (indent width) (outer nil))
        (cond
          (inside-literal)
          (open
           (multiple-value-bind (opener-prefix opener-line-start) (line-prefix buffer (car open))
             (declare (ignore opener-line-start))
             (let* ((column (visual-width opener-prefix tab-size))
                    (base-column (visual-width (whitespace-prefix opener-prefix) tab-size)))
               (setf outer (if (and (not (lisp-language-p language)) (eql (cdr open) #\{)) base-column column)
                     indent (cond
                              ((lisp-language-p language)
                               (if (eql (cdr open) #\()
                                   (lisp-form-indentation buffer (car open) position language column tab-size (rest (lex-state-bracket-stack state)))
                                   (1+ column)))
                              ((eql (cdr open) #\{) (+ base-column tab-size))
                              (t (1+ column)))))))
          ;; A complete Lisp form leaves no enclosing indentation context.
          ;; Returning to top level must not inherit the closing line's spaces.
          ((and state (lisp-language-p language)) (setf indent 0))
          ((and (eq language :python) state (eql (lex-state-last-code state) #\:)
                (>= (or (lex-state-last-code-position state) -1) line-start))
           (incf indent tab-size)))
        (let* ((suffix (xeon.core:buffer-substring buffer end-position (min (xeon.core:buffer-length buffer) (+ end-position 4096))))
               (closing (position-if-not (lambda (c) (find c '(#\Space #\Tab))) suffix))
               (split (and open outer (not inside-literal) closing
                           (eql (char suffix closing) (char ")]}" (position (cdr open) "([{"))))))
          (values (if (= indent width) base (make-string indent :initial-element #\Space))
                  (when split (make-string outer :initial-element #\Space))
                  (if split closing 0)))))))

(defun buffer-newline (buffer position)
  "Prefer the current/previous line's terminator, reading only a bounded range."
  (multiple-value-bind (prefix start) (line-prefix buffer position)
    (declare (ignore prefix))
    (let* ((text (if (plusp start)
                     (xeon.core:buffer-substring buffer (max 0 (- start 2)) start)
                     (xeon.core:buffer-substring buffer position (min (xeon.core:buffer-length buffer) (+ position 4096)))))
           (newline (position #\Newline text)))
      (if (and newline (plusp newline) (eql (char text (1- newline)) #\Return))
          (coerce '(#\Return #\Newline) 'string) (string #\Newline)))))

(defun insert-indented-newline (session &key (tab-size 4))
  "Insert an indented newline as one backend undo group, replacing selection."
  (let ((buffer (syntax-session-buffer session)))
    (multiple-value-bind (start end) (xeon.core:selection-range buffer)
      (multiple-value-bind (indent outer skipped) (newline-indentation session start :tab-size tab-size :end-position end)
        (let* ((newline (buffer-newline buffer start))
               (text (concatenate 'string newline indent (when outer (concatenate 'string newline outer)))))
          (xeon.core:with-edit-group (buffer)
            (xeon.core:replace-range buffer start (+ end skipped) text)
            (xeon.core:set-selection buffer (+ start (length newline) (length indent)))))))
    buffer))

(defun current-form-range (session &optional (position (xeon.core:buffer-point (syntax-session-buffer session))))
  "Return the innermost enclosing form, or adjacent top-level atom/string.
Uses lexical checkpoints and bounded lookahead; never invokes the Lisp reader."
  (unless (member (syntax-language session) '(:common-lisp :clojure :racket))
    (error "Current-form evaluation supports Common Lisp, Clojure and Racket."))
  (check-type position (integer 0 *))
  (let* ((buffer (syntax-session-buffer session)) (size (xeon.core:buffer-length buffer)))
    (when (> position size) (error "Caret is outside the document."))
    (labels ((at (i) (when (<= 0 i (1- size)) (char (xeon.core:buffer-substring buffer i (1+ i)) 0)))
             (prefix (start)
               ;; Preserve reader constructors such as #(), #S() and #hash(),
               ;; along with contiguous quote/unquote prefixes.
               (let ((tag start))
                 (loop while (and (plusp tag) (not (token-delimiter-p (at (1- tag)) (syntax-language session)))) do
                   (decf tag) (when (> (- start tag) 256) (return)))
                 (when (eql (at tag) #\#) (setf start tag)))
               (loop while (and (plusp start) (find (at (1- start)) "'`,@#")) do (decf start)) start)
             (finish (start end)
               (values start end)))
      (let* ((probe (if (and (plusp position) (find (at (1- position)) ")]}")) (1- position) position))
             (state (syntax-state-at session probe)))
        (unless state (error "Syntax indexing has not reached the caret; try again shortly."))
        (when (member (lex-state-mode state) '(:line-comment :block-comment)) (error "The caret is in a comment."))
        (let ((open (if (and (member (lex-state-mode state) '(:normal :token)) (find (at probe) "([{"))
                        probe (car (first (lex-state-bracket-stack state))))))
          (when open
            (let ((close (gethash open (syntax-session-matches session))))
              (unless close
                (when (> (- position open) 65536) (error "The form is not fully indexed; use Evaluate Buffer or Evaluate Selection."))
                (multiple-value-bind (chunk styles next brackets)
                    (style-chunk (xeon.core:buffer-substring buffer open (min size (+ open 65536)))
                                 (min 65536 (- size open)) (syntax-state-at session open) (syntax-language session) open)
                  (declare (ignore chunk styles next))
                  (setf close (first (find open brackets :key #'third)))))
              (unless close (error "The form is incomplete or not fully indexed; use Evaluate Buffer or Evaluate Selection."))
              (return-from current-form-range (finish (prefix open) (1+ close)))))
          (let ((start (if (eq (lex-state-mode state) :string) (lex-state-string-start state) probe)))
            (when (or (null (at start)) (spacep (at start)))
              (when (and (plusp start) (at (1- start)) (not (spacep (at (1- start))))) (decf start)))
            (when (or (null (at start)) (spacep (at start)) (eql (at start) #\;)) (error "No form at the caret."))
            (if (eql (at start) #\")
                (let ((escaped nil))
                  (loop for end from (1+ start) below (min size (+ start 65536)) for c = (at end) do
                    (cond (escaped (setf escaped nil)) ((eql c #\\) (setf escaped t))
                          ((eql c #\") (return-from current-form-range (finish (prefix start) (1+ end))))))
                  (error "The string is incomplete or needs a larger scan; use Evaluate Buffer or Evaluate Selection."))
                (progn
                  (loop while (and (plusp start) (not (token-delimiter-p (at (1- start)) (syntax-language session)))) do
                    (decf start) (when (> (- probe start) 65536) (error "The token needs a larger scan; use Evaluate Buffer or Evaluate Selection.")))
                  (let ((end start))
                    (loop while (and (at end) (not (token-delimiter-p (at end) (syntax-language session)))) do
                      (incf end) (when (> (- end start) 65536) (error "The token needs a larger scan; use Evaluate Buffer or Evaluate Selection.")))
                    (when (= start end) (error "No form at the caret."))
                    (finish (prefix start) end))))))))))
