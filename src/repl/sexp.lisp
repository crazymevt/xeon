;;;; sexp.lisp — reading and writing the s-expressions Swank exchanges
;;;;
;;;; Messages from the Lisp being edited are read with this small reader, not
;;;; CL:READ: it never evaluates anything (no #.) and never interns symbols
;;;; outside the KEYWORD package. Other symbols become REMOTE-SYMBOLs, which
;;;; keep their text. The writer prints Lisp data, with REMOTE-SYMBOLs for
;;;; symbols in the other image's packages (swank:connection-info).

(in-package #:xeon.repl)

(defstruct (remote-symbol (:constructor remote-symbol (name)))
  "A symbol in the other Lisp, by its printed name, such as \"swank:autodoc\"."
  (name "" :type string))

(defmethod print-object ((symbol remote-symbol) stream)
  (if *print-escape*
      (format stream "#<remote ~a>" (remote-symbol-name symbol))
      (write-string (remote-symbol-name symbol) stream)))

(defun remote-symbol-base-name (symbol)
  "SYMBOL's name without its package, in upper case."
  (let ((name (remote-symbol-name symbol)))
    (string-upcase (string-trim "|" (subseq name (1+ (or (position #\: name :from-end t) -1)))))))

(defun remote-symbol= (symbol name)
  "True if SYMBOL (a remote symbol) is named NAME, ignoring its package and case."
  (and (remote-symbol-p symbol) (string-equal (remote-symbol-base-name symbol) name)))

;;; Writing

(defun write-sexp (object &optional (stream *standard-output*))
  (etypecase object
    (null (write-string "nil" stream))
    ((eql t) (write-string "t" stream))
    (keyword (write-char #\: stream) (write-string (string-downcase (symbol-name object)) stream))
    (symbol
     ;; Only Common Lisp symbols (quote, function) are allowed bare: every
     ;; Lisp has them. Anything else must be a remote symbol.
     (unless (eq (symbol-package object) (find-package :common-lisp))
       (error "Cannot send the symbol ~s; use a remote symbol." object))
     (write-string (string-downcase (symbol-name object)) stream))
    (remote-symbol (write-string (remote-symbol-name object) stream))
    (string (write-char #\" stream)
            (loop for c across object
                  do (when (member c '(#\" #\\)) (write-char #\\ stream))
                     (write-char c stream))
            (write-char #\" stream))
    (integer (format stream "~d" object))
    (ratio (format stream "~d/~d" (numerator object) (denominator object)))
    (float (let ((*read-default-float-format* 'double-float))
             (format stream "~f" (coerce object 'double-float))))
    (character (format stream "#\\~a" (or (char-name object) object)))
    (cons (write-char #\( stream)
          (loop for (head . tail) on object
                do (write-sexp head stream)
                   (cond ((null tail))
                         ((consp tail) (write-char #\Space stream))
                         (t (write-string " . " stream) (write-sexp tail stream))))
          (write-char #\) stream)))
  object)

(defun sexp-to-string (object)
  (with-output-to-string (s) (write-sexp object s)))

;;; Reading

(define-condition sexp-read-error (error)
  ((text :initarg :text :reader sexp-read-error-text)
   (position :initarg :position :reader sexp-read-error-position))
  (:report (lambda (c s) (format s "Malformed s-expression at ~d" (sexp-read-error-position c)))))

(defun sexp-delimiter-p (c)
  (or (whitespace-char-p c) (member c '(#\( #\) #\" #\' #\; #\`))))

(defun read-sexp (string &optional (start 0))
  "Read one s-expression from STRING at START. Returns it and the position after it."
  (let ((i start) (n (length string)))
    (labels ((fail () (error 'sexp-read-error :text string :position i))
             (peek () (if (< i n) (char string i) (fail)))
             (skip-space ()
               (loop while (< i n)
                     do (let ((c (char string i)))
                          (cond ((whitespace-char-p c) (incf i))
                                ((char= c #\;) (loop while (and (< i n) (char/= (char string i) #\Newline))
                                                     do (incf i)))
                                (t (return))))))
             (read-object ()
               (let ((*wire-depth* (1+ *wire-depth*)))
                 (when (> *wire-depth* 128) (fail))
                 (read-raw-object)))
             (read-raw-object ()
               (skip-space)
               (let ((c (peek)))
                 (case c
                   (#\( (incf i) (read-list))
                   (#\) (fail))
                   (#\" (incf i) (read-string))
                   (#\' (incf i) (list 'quote (read-object)))
                   (#\` (incf i) (read-object))
                   (#\# (read-dispatch))
                   (t (read-atom)))))
             (read-list ()
               (let ((items '()) (tail nil))
                 (loop
                   (skip-space)
                   (let ((c (peek)))
                     (cond ((char= c #\)) (incf i) (return))
                           ((and (char= c #\.) (< (1+ i) n) (sexp-delimiter-p (char string (1+ i))))
                            (incf i) (setf tail (read-object)) (skip-space)
                            (unless (char= (peek) #\)) (fail))
                            (incf i) (return))
                           (t (push (read-object) items)))))
                 (let ((list (nreverse items)))
                   (when tail (setf (cdr (last list)) tail))
                   list)))
             (read-string ()
               (with-output-to-string (out)
                 (loop (let ((c (peek)))
                         (incf i)
                         (case c
                           (#\" (return))
                           (#\\ (write-char (peek) out) (incf i))
                           (t (write-char c out)))))))
             (read-dispatch ()
               (incf i)
               (let ((c (peek)))
                 (case c
                   (#\\ (incf i)
                    (let ((start i))
                      (incf i)
                      (loop while (and (< i n) (not (sexp-delimiter-p (char string i)))) do (incf i))
                      (let ((name (subseq string start i)))
                        (or (if (= (length name) 1) (char name 0) (name-char name)) #\?))))
                   (#\( (incf i) (coerce (read-list) 'vector))
                   (#\: (incf i) (read-atom))
                   ((#\t #\f)              ; Racket's #t, #f, #true and #false
                    (let ((start i))
                      (loop while (and (< i n) (not (sexp-delimiter-p (char string i)))) do (incf i))
                      (and (member (subseq string start i) '("t" "true") :test #'string=) t)))
                   (#\< ;; An unreadable object: keep its text.
                    (let ((end (position #\> string :start i)))
                      (unless end (fail))
                      (prog1 (subseq string (1- i) (1+ end)) (setf i (1+ end)))))
                   (t (fail)))))
             (read-atom ()
               (let ((text (with-output-to-string (out)
                             (loop while (and (< i n) (not (sexp-delimiter-p (char string i))))
                                   do (let ((c (char string i)))
                                        (cond ((char= c #\\) (incf i) (write-char (peek) out) (incf i))
                                              ((char= c #\|)
                                               (write-char c out) (incf i)
                                               (loop until (char= (peek) #\|)
                                                     do (write-char (char string i) out) (incf i))
                                               (write-char #\| out) (incf i))
                                              (t (write-char c out) (incf i))))))))
                 (when (string= text "") (fail))
                 (cond ((number-string-p text)
                        (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                          (read-from-string text)))
                       ((char= (char text 0) #\:)
                        (intern (string-upcase (string-left-trim ":" text)) :keyword))
                       ((string-equal text "nil") nil)
                       ((string-equal text "t") t)
                       (t (remote-symbol text))))))
      (values (read-object) i))))
