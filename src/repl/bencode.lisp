;;;; bencode.lisp — the encoding nREPL messages travel in
;;;;
;;;; Bencode has four kinds of value: integers (i42e), byte strings (5:hello,
;;;; the length in bytes), lists (l…e) and dictionaries (d…e, keys sorted).
;;;; Here strings are UTF-8 Lisp strings, lists are lists, and dictionaries
;;;; are alists with string keys, in the order they arrive. To encode, an
;;;; alist (a list whose first element is a cons) or a hash table is a
;;;; dictionary; any other list is a list.

(in-package #:xeon.repl)

(define-condition bencode-error (error)
  ((message :initarg :message :reader bencode-error-message))
  (:report (lambda (c s) (write-string (bencode-error-message c) s))))

(defun bencode-error (format &rest arguments)
  (error 'bencode-error :message (apply #'format nil format arguments)))

;;; Encoding

(defun dictionary-entries (value)
  "VALUE's (key . value) pairs, keys as strings, sorted as bencode requires."
  (let ((pairs (if (hash-table-p value)
                   (loop for k being the hash-keys of value using (hash-value v) collect (cons k v))
                   (copy-list value))))
    (sort (mapcar (lambda (pair)
                    (cons (if (stringp (car pair)) (car pair) (string-downcase (string (car pair)))) (cdr pair)))
                  pairs)
          #'string< :key #'car)))

(defun bencode (value)
  "VALUE's bencoding, as a vector of octets."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((octets (vector) (loop for b across vector do (vector-push-extend b out)))
             (ascii (string) (octets (sb-ext:string-to-octets string :external-format :ascii)))
             (encode (value)
               (cond ((integerp value) (ascii (format nil "i~de" value)))
                     ((stringp value)
                      (let ((bytes (sb-ext:string-to-octets value :external-format :utf-8)))
                        (ascii (format nil "~d:" (length bytes)))
                        (octets bytes)))
                     ((and value (symbolp value)) (encode (string-downcase (symbol-name value))))
                     ((or (hash-table-p value) (and (consp value) (consp (first value))))
                      (ascii "d")
                      (loop for (key . v) in (dictionary-entries value)
                            do (encode key) (encode v))
                      (ascii "e"))
                     ((listp value)
                      (ascii "l")
                      (dolist (v value) (encode v))
                      (ascii "e"))
                     (t (bencode-error "Can't bencode ~s" value)))))
      (encode value))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; Decoding

(defun bdecode-with (next-byte)
  "Read one bencoded value, getting octets by calling NEXT-BYTE, which
signals END-OF-FILE when there are no more."
  (labels ((decimal (b terminator)
             (let ((negative nil) (n 0) (digits 0))
               (when (= b (char-code #\-)) (setf negative t b (funcall next-byte)))
               (loop until (= b terminator)
                     do (unless (<= (char-code #\0) b (char-code #\9))
                          (bencode-error "Bad digit ~s in bencode" (code-char b)))
                        (setf n (+ (* n 10) (- b (char-code #\0))))
                        (incf digits)
                        (when (> digits 16) (bencode-error "Bencode integer too long"))
                        (setf b (funcall next-byte)))
               (when (zerop digits) (bencode-error "A bencoded number has no digits"))
               (if negative (- n) n)))
           (value (first)
             (let ((*wire-depth* (1+ *wire-depth*)))
               (when (> *wire-depth* 128) (bencode-error "Bencode nesting too deep"))
               (raw-value first)))
           (raw-value (first)
             (case (code-char first)
               (#\i (decimal (funcall next-byte) (char-code #\e)))
               (#\l (loop for b = (funcall next-byte)
                          until (= b (char-code #\e))
                          collect (value b)))
               (#\d (loop for b = (funcall next-byte)
                          until (= b (char-code #\e))
                          collect (let ((key (value b)))
                                    (unless (stringp key) (bencode-error "A dictionary key isn't a string: ~s" key))
                                    (cons key (value (funcall next-byte))))))
               (t (let* ((length (decimal first (char-code #\:)))
                         (bytes (if (or (minusp length) (> length 1048576))
                                    (bencode-error "A bencoded string has a negative length: ~d" length)
                                    (make-array length :element-type '(unsigned-byte 8)))))
                    (dotimes (i length) (setf (aref bytes i) (funcall next-byte)))
                    (sb-ext:octets-to-string bytes :external-format '(:utf-8 :replacement #\?)))))))
    (value (funcall next-byte))))

(defun bdecode (octets)
  "The value bencoded in OCTETS, a vector."
  (let ((i 0))
    (bdecode-with (lambda ()
                    (if (< i (length octets))
                        (prog1 (aref octets i) (incf i))
                        (error 'end-of-file :stream nil))))))

(defun bdecode-stream (stream)
  "Read one bencoded value from STREAM, a stream of octets."
  (let ((count 0))
    (bdecode-with (lambda ()
      (when (> (incf count) 1048576) (bencode-error "Bencode message exceeds 1 MiB"))
      (read-byte stream)))))

(defun dict-get (dictionary key)
  "The value of KEY (a string) in DICTIONARY, a decoded alist."
  (cdr (assoc key dictionary :test #'string=)))
