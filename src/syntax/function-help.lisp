(in-package #:xeon.syntax)

;; SBCL-only introspection is isolated here; source text is never read/evaluated.
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-introspect))

(defun builtin-function-help (name)
  (let* ((colon (position #\: name))
         (package (if colon (find-package (string-upcase (subseq name 0 colon)))
                      (find-package :cl)))
         (bare (if colon (string-left-trim ":" (subseq name colon)) name))
         (symbol (when package (find-symbol (string-upcase bare) package))))
    (when (and symbol (fboundp symbol))
      (handler-case
          (let* ((function (or (macro-function symbol) (symbol-function symbol)))
                 (args (sb-introspect:function-lambda-list function)))
            (values (format nil "~A ~A" name
                            (let ((*print-case* :downcase) (*print-pretty* nil)
                                  (*print-length* 40) (*print-level* 8))
                              (write-to-string args)))
                    (documentation symbol 'function)))
        (error () nil)))))

(defun help-token (text styles position)
  (when (and (< position (length text))
             (completion-character-p (char text position))
             (not (member (aref styles position) '(2 3 4))))
    (let ((start position) (end position))
      (loop while (and (> start 0) (completion-character-p (char text (1- start)))) do (decf start))
      (loop while (and (< end (length text)) (completion-character-p (char text end))) do (incf end))
      (values (subseq text start end) start end))))

(defun local-function-help (text styles brackets name language)
  (labels ((skip-space (at)
             (loop while (and (< at (length text)) (spacep (char text at))) do (incf at)
                   finally (return at)))
           (partner (at)
             (first (find at brackets :key #'third))))
    (loop for i below (length text)
          when (and (eql (char text i) #\() (= (aref styles i) 0)) do
      (multiple-value-bind (kind start end) (help-token text styles (skip-space (1+ i)))
        (declare (ignore start))
        (when (and (eq language :racket) (string-equal kind "define"))
          (let ((head (skip-space end)))
            (when (and (< head (length text)) (eql (char text head) #\())
              (multiple-value-bind (candidate begin finish)
                  (help-token text styles (skip-space (1+ head)))
                (declare (ignore begin))
                (let ((close (partner head)))
                  (when (and candidate (string= candidate name) close (< (- close head) 2048))
                    (return-from local-function-help
                      (values (format nil "~A (~A)" name
                                      (string-trim '(#\Space #\Tab #\Newline)
                                                   (subseq text finish close))) nil))))))))
        (when (member kind '("defun" "defmacro" "defn" "defn-") :test #'string-equal)
          (let ((at (skip-space end)))
            (multiple-value-bind (candidate start finish) (help-token text styles at)
              (declare (ignore start))
              (when (and candidate (if (eq language :common-lisp)
                                      (string-equal candidate name) (string= candidate name)))
                (let* ((args (skip-space finish)) (doc nil))
                  ;; Clojure puts the docstring before the argument vector.
                  (when (and (< args (length text)) (eql (char text args) #\"))
                    (let ((end (1+ args)))
                      (loop while (and (< end (length text)) (= (aref styles end) 2)) do (incf end))
                      (setf doc (subseq text args end) args (skip-space end))))
                  (when (and (< args (length text)) (find (char text args) "(["))
                    (let ((close (partner args)))
                      (when (and close (< (- close args) 2048))
                        (let ((after (skip-space (1+ close))))
                          (when (and (null doc) (< after (length text)) (eql (char text after) #\"))
                            (let ((end (1+ after)))
                              (loop while (and (< end (length text)) (= (aref styles end) 2)) do (incf end))
                              (setf doc (subseq text after end)))))
                        (return-from local-function-help
                          (values (format nil "~A ~A" name (subseq text args (1+ close))) doc))))))))))))))

(defun function-help-at (session position &key hover)
  "Return signature, docstring and symbol position. Bounded to 64 Ki characters.
Uses lexical context, not the Lisp reader. Unknown/incomplete contexts return NIL."
  (let* ((buffer (syntax-session-buffer session)) (language (syntax-language session))
         (length (xeon.core:buffer-length buffer)))
    (unless (and (member language '(:common-lisp :clojure :racket)) (<= 0 position length))
      (return-from function-help-at nil))
    ;; Starting at zero preserves reader/string/comment context. Beyond this
    ;; bound fail closed rather than interpret a fragment in the wrong context.
    (when (> position 65536) (return-from function-help-at nil))
    (let ((text (xeon.core:buffer-substring buffer 0 (min length 65536))))
      (multiple-value-bind (chunk styles state brackets)
          (style-chunk text (length text) (make-lex-state) language 0)
        (declare (ignore chunk state))
        (let* ((context (syntax-state-at session position))
               (stack (and context (lex-state-bracket-stack context)))
               (at (if hover position
                       (let ((open (find #\( stack :key #'cdr)))
                         (when open
                           (loop for i from (1+ (car open)) below (length text)
                                 unless (spacep (char text i)) return i))))))
          (when (and at (or hover (and context (member (lex-state-mode context) '(:normal :token)))))
            (multiple-value-bind (name start end) (help-token text styles at)
              (declare (ignore end))
              (when name
                (multiple-value-bind (signature doc) (local-function-help text styles brackets name language)
                  (unless signature
                    (when (eq language :common-lisp)
                      (multiple-value-setq (signature doc) (builtin-function-help name))))
                  (values signature doc start name))))))))))
