(in-package #:xeon.syntax)

(defparameter *lisp-indentation-overrides* (make-hash-table :test #'equal))
(defparameter *lisp-body-rules*
  '((:common-lisp
     (0 "progn" "locally" "cond" "tagbody" "with-standard-io-syntax")
     (1 "lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "when" "unless"
        "block" "catch" "dolist" "dotimes" "with-open-file" "with-open-stream"
        "with-input-from-string" "with-output-to-string" "with-compilation-unit"
        "case" "ecase" "ccase" "typecase" "etypecase" "ctypecase"
        "with-simple-restart" "handler-bind" "handler-case" "restart-bind" "restart-case"
        "unwind-protect" "prog1" "defstruct" "defpackage" "eval-when")
     (2 "defun" "defmacro" "defmethod" "destructuring-bind" "multiple-value-bind"
        "with-slots" "with-accessors" "do" "do*" "progv" "prog2"))
    (:clojure (0 "defn" "defn-" "defmacro" "fn" "def" "let" "letfn" "if" "if-let"
                 "if-not" "when" "when-let" "when-not" "cond" "do" "doseq" "dotimes"
                 "for" "loop" "binding" "with-open" "try" "catch" "finally"))
    (:racket (0 "define" "define-values" "define-syntax" "define-syntax-rule" "lambda" "λ"
                "let" "let*" "letrec" "let-values" "let*-values" "when" "unless" "cond"
                "begin" "begin0" "for" "for/list" "for/fold" "with-handlers" "parameterize"))))

(defun indentation-form-name (language name)
  (let ((name (if (eq language :common-lisp) (string-downcase name) name)))
    (cond ((eq language :common-lisp) (subseq name (if (position #\: name :from-end t) (1+ (position #\: name :from-end t)) 0)))
          ((and (eq language :clojure) (<= 13 (length name)) (string= "clojure.core/" name :end2 13)) (subseq name 13))
          (t name))))

(defun set-lisp-indentation-rule (language name rule)
  "Register (:BODY argument-count), (:FIXED column-offset), or NIL to remove an override.
Rules are backend data; they never evaluate source or invoke a macro expander."
  (unless (and (lisp-language-p language) (stringp name) (plusp (length name))) (error "Invalid Lisp indentation form."))
  (unless (or (null rule) (and (listp rule) (= (length rule) 2) (member (first rule) '(:body :fixed))
                              (typep (second rule) '(integer 0 64))))
    (error "Invalid Lisp indentation rule: ~S" rule))
  (let ((key (list language (if (eq language :common-lisp) (string-downcase name) name))))
    (if rule (setf (gethash key *lisp-indentation-overrides*) (copy-list rule))
        (remhash key *lisp-indentation-overrides*)))
  rule)

(defun lisp-indentation-rule (language name)
  (let ((original (if (eq language :common-lisp) (string-downcase name) name))
        (name (indentation-form-name language name)))
    (or (gethash (list language original) *lisp-indentation-overrides*)
        (gethash (list language name) *lisp-indentation-overrides*)
        (when (and (eq language :common-lisp) (equal name "loop")) :loop)
        (when (and (member language '(:common-lisp :racket)) (equal name "if")) '(:fixed 4))
        (when (and (eq language :common-lisp) (member name '("defclass" "define-condition") :test #'equal)) '(:fixed 2))
        (loop for group in (rest (assoc language *lisp-body-rules*))
              when (member name (rest group) :test #'equal) return (list :body (first group))))))

(defun lisp-form-items (buffer opener position language)
  "Read bounded lexical datums at the current form's depth, without the Lisp reader.
Return (kind name position) items and success-p; nested forms/strings stay opaque."
  (when (> (- position opener) 65536) (return-from lisp-form-items (values nil nil)))
  (let ((state (make-lex-state)) (cursor (1+ opener)) (depth 0) (items nil)
        (item-start nil) (kind nil) (last-style nil) (bar nil) (escaped nil))
    (labels ((finish (end)
               (when item-start
                 (push (list kind (when (and (eq kind :atom) (<= (- end item-start) 256))
                                    (xeon.core:buffer-substring buffer item-start end)) item-start) items))
               (setf item-start nil kind nil bar nil escaped nil))
             (begin (at type) (unless item-start (setf item-start at kind type))))
      (loop while (< cursor position) do
        (let* ((end (min position (+ cursor 4096)))
               (text (xeon.core:buffer-substring buffer cursor (min position (+ end 256)))))
          (multiple-value-bind (chunk styles) (style-chunk text (- end cursor) state language cursor)
            (loop for c across chunk for style across styles for at from cursor do
              (when (and (eql last-style 2) (/= style 2) (zerop depth)) (finish at))
              (cond
                ((= style 3) (when (and (zerop depth) item-start) (finish at)))
                ((= style 2)
                 (when (zerop depth)
                   (when (and item-start (eq kind :atom)
                              (not (and (< (- at item-start) 3)
                                        (member (xeon.core:buffer-substring buffer item-start at) '("'" "#'") :test #'equal))))
                     (finish at))
                   (begin at :literal)))
                ((and (member style '(0 1)) (find c "([{")) (begin at :form) (incf depth))
                ((and (member style '(0 1)) (find c ")]}"))
                 (when (zerop depth) (return-from lisp-form-items (values nil nil)))
                 (decf depth) (when (zerop depth) (finish (1+ at))))
                ((zerop depth)
                 (cond ((and (spacep c) (not bar) (not escaped)) (finish at))
                       (t (begin at :atom)
                          (cond (escaped (setf escaped nil))
                                ((and (= style 5) (eql c #\\)) (setf escaped t))
                                ((and (= style 5) (eql c #\|)) (setf bar (not bar))))))))
              (setf last-style style))
            (incf cursor (length chunk)))))
      (when (plusp depth) (return-from lisp-form-items (values nil nil)))
      (finish position)
      (values (nreverse items) t))))

(defun indentation-item-column (buffer item tab-size)
  (visual-width (line-prefix buffer (third item)) tab-size))

(defun loop-indentation (buffer items column tab-size)
  "Align LOOP clauses and forms of DO/INITIALLY/FINALLY without evaluating LOOP."
  (let* ((first (second items))
         (base (if (and first (eq (first first) :atom)) (indentation-item-column buffer first tab-size) (+ column 2)))
         (conditions nil) (body nil) (body-first nil) (action nil))
    (dolist (item (rest items))
      (let ((word (when (and (eq (first item) :atom) (second item))
                    (indentation-form-name :common-lisp (second item)))))
        (cond
          ((member word '("if" "when" "unless") :test #'equal)
           (loop while (eq (first conditions) :done) do (pop conditions))
           (push :test conditions) (setf body nil body-first nil action nil))
          ((member word '("else" "and") :test #'equal)
           (when conditions (setf (first conditions) :action))
           (setf body nil body-first nil action nil))
          ((equal word "end")
           (pop conditions)
           (when conditions (setf (first conditions) :done))
           (setf body nil body-first nil action nil))
          ((member word '("do" "doing" "initially" "finally") :test #'equal)
           (when (member word '("initially" "finally") :test #'equal) (setf conditions nil))
           (setf body (+ (indentation-item-column buffer item tab-size) (length word) 1)
                 body-first t action nil))
          ((member word '("collect" "collecting" "append" "appending" "nconc" "nconcing"
                          "count" "counting" "sum" "summing" "maximize" "maximizing"
                          "minimize" "minimizing" "return" "thereis" "always" "never") :test #'equal)
           (setf body nil body-first nil action t))
          ((member word '("for" "as" "with" "repeat" "while" "until" "named") :test #'equal)
           (setf conditions nil body nil body-first nil action nil))
          ((eq (first conditions) :test) (setf (first conditions) :action))
          (body-first (setf body (indentation-item-column buffer item tab-size) body-first nil))
          (action (when conditions (setf (first conditions) :done)) (setf action nil)))))
    (let* ((depth (count :action conditions))
           (clause (+ base (* 2 depth)))
           (closing (+ base (* 2 (max 0 (- depth (if (eq (first conditions) :action) 1 0)))))))
      (values (min 4096 (or body clause)) (min 4096 clause) base (min 4096 closing)))))

(defun lisp-list-context (buffer opener language parents)
  "Recognize binding/lambda lists and local function definitions from parent forms."
  (when (and (eq language :common-lisp) parents)
    (let* ((parent (car (first parents)))
           (items (lisp-form-items buffer parent opener language))
           (head (and (eq (first (first items)) :atom) (second (first items))))
           (name (and head (indentation-form-name language head)))
           (argument (length (rest items))))
      (cond
        ((or (and (= argument 1) (member name '("defun" "defmacro") :test #'equal))
             (and (equal name "lambda") (zerop argument))
             (and (equal name "defmethod") (>= argument 1)
                  (not (find :form (rest items) :key #'first)))
             (and (zerop argument)
                  (member name '("let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "do" "do*"
                                 "multiple-value-bind" "destructuring-bind" "with-open-file" "with-open-stream"
                                 "with-input-from-string" "with-output-to-string" "with-slots" "with-accessors"
                                 "handler-bind" "restart-bind") :test #'equal))) :data)
        ((or (equal name "cond")
             (and (>= argument 1) (member name '("case" "ecase" "ccase" "typecase" "etypecase" "ctypecase") :test #'equal))) :clause)
        ((and (>= argument 1) (member name '("handler-case" "restart-case") :test #'equal)) :local-function)
        ((rest parents)
         (let* ((grand (car (second parents)))
                (grand-items (lisp-form-items buffer grand parent language))
                (grand-name (second (first grand-items))))
           (cond ((and grand-name (member (indentation-form-name language grand-name) '("flet" "labels" "macrolet") :test #'equal)
                       (null (rest grand-items))) :local-function)
                 ((and (zerop argument) (rest (rest parents)))
                  (let* ((great (car (third parents)))
                         (great-items (lisp-form-items buffer great grand language))
                         (great-name (second (first great-items))))
                    (when (and great-name (null (rest great-items))
                               (member (indentation-form-name language great-name) '("flet" "labels" "macrolet") :test #'equal)) :data))))))))))

(defun lisp-form-indentation (buffer opener position language column tab-size &optional parents)
  (multiple-value-bind (items complete) (lisp-form-items buffer opener position language)
    (unless complete (return-from lisp-form-indentation (+ column 2)))
    (let* ((head (when (and items (eq (first (first items)) :atom)) (second (first items))))
           (context (lisp-list-context buffer opener language parents))
           (rule (case context (:local-function '(:body 1)) (:clause '(:fixed 2))
                   (otherwise (when head (lisp-indentation-rule language head))))))
      (cond ((eq context :data) (1+ column))
            ((and (null rule) (null head) items) (indentation-item-column buffer (first items) tab-size))
            ((eq rule :loop) (loop-indentation buffer items column tab-size))
            ((eq (first rule) :fixed) (+ column (second rule)))
            ((eq (first rule) :body) (+ column (if (< (length (rest items)) (second rule)) 4 2)))
            ((second items)
             (multiple-value-bind (opener-line ignored) (xeon.core:position-line-column buffer opener)
               (declare (ignore ignored))
               (if (= opener-line (xeon.core:position-line-column buffer (third (second items))))
                   (indentation-item-column buffer (second items) tab-size) (+ column 2))))
            (t (+ column 2))))))
