(in-package #:xeon.http)

;;; Response presentation: content-type detection and token-level reformatting of
;;; JSON and HTML/XML. Neither formatter builds values, so numbers, key order,
;;; duplicate keys and entity text are kept exactly; malformed input yields NIL and
;;; callers show the raw text instead.

(defparameter *json-space*
  (list #\Space #\Tab #\Newline #\Return (code-char #xFEFF))
  "Whitespace between JSON tokens, and a byte order mark.")

(defparameter *pretty-limit* (* 4 1024 1024)
  "Larger bodies are shown raw only; reformatting them would stall the UI.")

(defun response-header-value (response name)
  "The last NAME header of RESPONSE's final header block, or NIL. Redirects and 1xx
responses leave earlier blocks in the raw headers; the last block describes the body."
  (let ((value nil))
    (dolist (line (uiop:split-string (response-headers-text response) :separator '(#\Newline)))
      (let* ((line (string-right-trim '(#\Return) line))
             (colon (position #\: line)))
        (cond ((uiop:string-prefix-p "HTTP/" line) (setf value nil))
              ((and colon (string-equal name (string-trim " " (subseq line 0 colon))))
               (setf value (string-trim " 	" (subseq line (1+ colon))))))))
    value))

(defun response-header-pairs (response)
  "(name . value) pairs of RESPONSE's final header block, in order."
  (let ((pairs '()))
    (dolist (line (uiop:split-string (response-headers-text response) :separator '(#\Newline)))
      (let* ((line (string-right-trim '(#\Return) line))
             (colon (position #\: line)))
        (cond ((uiop:string-prefix-p "HTTP/" line) (setf pairs '()))
              (colon (push (cons (string-trim " " (subseq line 0 colon))
                                 (string-trim " 	" (subseq line (1+ colon))))
                           pairs)))))
    (nreverse pairs)))

(defun media-type (content-type)
  (when content-type
    (string-downcase (string-trim " " (subseq content-type 0 (or (position #\; content-type)
                                                                 (length content-type)))))))

(defun content-kind (content-type text)
  "One of :JSON, :HTML, :XML or :TEXT, from CONTENT-TYPE, then the text itself."
  (let ((type (media-type content-type))
        (start (and text (position-if-not (lambda (c) (member c *json-space*))
                                          text))))
    (cond ((and type (or (string= type "application/json") (uiop:string-suffix-p type "+json")
                         (string= type "text/json")))
           :json)
          ((and type (or (string= type "text/html") (string= type "application/xhtml+xml"))) :html)
          ((and type (or (uiop:string-suffix-p type "/xml") (uiop:string-suffix-p type "+xml"))) :xml)
          ((and type (plusp (length type))) :text)
          ((null start) :text)
          ((find (char text start) "{[") :json)
          ((and (char= (char text start) #\<)
                (let ((head (string-downcase (subseq text start (min (length text) (+ start 15))))))
                  (or (uiop:string-prefix-p "<!doctype html" head) (uiop:string-prefix-p "<html" head))))
           :html)
          ((and (char= (char text start) #\<) (uiop:string-prefix-p "<?xml" (subseq text start))) :xml)
          (t :text))))

(defun pretty-json (text &key (indent 2))
  "TEXT re-indented, or NIL unless it is one balanced JSON value."
  (when (> (length text) *pretty-limit*) (return-from pretty-json nil))
  (let ((depth 0) (stack '()) (i 0) (n (length text)) (seen nil)
        (out (make-string-output-stream)))
    (labels ((newline () (write-char #\Newline out)
               (loop repeat (* depth indent) do (write-char #\Space out)))
             (fail () (return-from pretty-json nil))
             (skip-space ()
               (loop while (and (< i n) (member (char text i) *json-space*))
                     do (incf i))))
      (loop
        (skip-space)
        (when (>= i n) (return))
        (when (and seen (null stack)) (fail))
        (let ((c (char text i)))
          (case c
            ((#\{ #\[)
             (write-char c out) (incf i) (push c stack) (setf seen t)
             (skip-space)
             (cond ((and (< i n) (char= (char text i) (if (char= c #\{) #\} #\])))
                    (write-char (char text i) out) (incf i) (pop stack))
                   (t (incf depth) (newline))))
            ((#\} #\])
             (unless (and stack (char= (first stack) (if (char= c #\}) #\{ #\[))) (fail))
             (pop stack) (decf depth) (newline) (write-char c out) (incf i))
            (#\, (unless stack (fail)) (write-char c out) (incf i) (newline))
            (#\: (unless (and stack (char= (first stack) #\{)) (fail)) (write-string ": " out) (incf i))
            (#\"
             (let ((start i))
               (incf i)
               (loop (when (>= i n) (fail))
                     (let ((d (char text i)))
                       (cond ((char= d #\\) (incf i 2))
                             ((char= d #\") (incf i) (return))
                             ((char< d #\Space) (fail))
                             (t (incf i)))))
               (when (> i n) (fail))
               (write-string text out :start start :end i) (setf seen t)))
            (t
             (let ((start i))
               (loop while (and (< i n) (or (alphanumericp (char text i)) (find (char text i) "+-.")))
                     do (incf i))
               (when (= start i) (fail))
               (let ((token (subseq text start i)))
                 (unless (or (member token '("true" "false" "null") :test #'string=)
                             (every (lambda (ch) (or (digit-char-p ch) (find ch "+-.eE"))) token))
                   (fail))
                 (write-string token out) (setf seen t))))))))
    (when (or stack (not seen)) (return-from pretty-json nil))
    (get-output-stream-string out)))

;;; Markup

(defparameter *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr"))

(defparameter *raw-elements* '("script" "style" "pre" "textarea"))

(defparameter *implied-end-elements*
  '(("p" "address" "article" "aside" "blockquote" "div" "dl" "fieldset" "footer" "form" "h1" "h2" "h3"
     "h4" "h5" "h6" "header" "hr" "main" "nav" "ol" "p" "pre" "section" "table" "ul")
    ("li" "li") ("dt" "dt" "dd") ("dd" "dt" "dd") ("option" "option" "optgroup")
    ("tr" "tr") ("td" "td" "th" "tr") ("th" "td" "th" "tr"))
  "HTML elements whose end tag may be left out: (element . elements whose start ends it).")

(defun implied-end-p (open-name new-name)
  (member new-name (rest (assoc open-name *implied-end-elements* :test #'string=)) :test #'string=))

(defun markup-tokens (text)
  "(kind start end name) tokens: :open, :close, :self, :other (comments, doctype, PIs) and :text."
  (let ((tokens '()) (i 0) (n (length text)))
    (flet ((tag-name (start)
             (let ((end (or (position-if (lambda (c) (or (member c '(#\Space #\Tab #\Newline #\Return #\/ #\>))))
                                         text :start start)
                            n)))
               (string-downcase (subseq text start end)))))
      (loop while (< i n) do
        (let ((lt (position #\< text :start i)))
          (when (and lt (> lt i)) (push (list :text i lt nil) tokens))
          (unless lt (push (list :text i n nil) tokens) (return))
          (setf i lt)
          (cond ((uiop:string-prefix-p "<!--" (subseq text i (min n (+ i 4))))
                 (let ((end (search "-->" text :start2 (+ i 4))))
                   (unless end (return-from markup-tokens nil))
                   (push (list :other i (+ end 3) nil) tokens) (setf i (+ end 3))))
                ((uiop:string-prefix-p "<![CDATA[" (subseq text i (min n (+ i 9))))
                 (let ((end (search "]]>" text :start2 i)))
                   (unless end (return-from markup-tokens nil))
                   (push (list :other i (+ end 3) nil) tokens) (setf i (+ end 3))))
                ((and (< (1+ i) n) (find (char text (1+ i)) "!?"))
                 (let ((end (position #\> text :start i)))
                   (unless end (return-from markup-tokens nil))
                   (push (list :other i (1+ end) nil) tokens) (setf i (1+ end))))
                ((and (< (1+ i) n) (char= (char text (1+ i)) #\/))
                 (let ((end (position #\> text :start i)))
                   (unless end (return-from markup-tokens nil))
                   (push (list :close i (1+ end) (tag-name (+ i 2))) tokens) (setf i (1+ end))))
                ((and (< (1+ i) n) (alpha-char-p (char text (1+ i))))
                 ;; Find the closing > outside quoted attribute values.
                 (let ((j (1+ i)) (quote nil))
                   (loop (when (>= j n) (return-from markup-tokens nil))
                         (let ((c (char text j)))
                           (cond (quote (when (char= c quote) (setf quote nil)))
                                 ((find c "\"'") (setf quote c))
                                 ((char= c #\>) (return))))
                         (incf j))
                   (let* ((name (tag-name (1+ i)))
                          (self (char= (char text (1- j)) #\/)))
                     (push (list (if (or self (member name *void-elements* :test #'string=)) :self :open)
                                 i (1+ j) name)
                           tokens)
                     (setf i (1+ j))
                     ;; Raw text elements run to their own end tag, unparsed.
                     (when (and (not self) (member name *raw-elements* :test #'string=))
                       (let ((end (search (format nil "</~A" name) text :start2 i :test #'char-equal)))
                         (unless end (return-from markup-tokens nil))
                         (when (> end i) (push (list :raw i end nil) tokens))
                         (setf i end))))))
                (t (push (list :text i (1+ i) nil) tokens) (incf i)))))
      (nreverse tokens))))

(defun pretty-markup (text &key (indent 2) html)
  "TEXT with one element per line, nested by indentation, or NIL when it cannot be
tokenized. Text runs are trimmed and short inline elements stay on one line. With HTML,
unclosed elements such as <p> and <li> are tolerated."
  (when (> (length text) *pretty-limit*) (return-from pretty-markup nil))
  (let ((tokens (markup-tokens text)) (depth 0) (open '()) (first t)
        (out (make-string-output-stream)))
    (unless tokens (return-from pretty-markup nil))
    (labels ((line (string)
               (unless first (write-char #\Newline out))
               (setf first nil)
               (loop repeat (* indent depth) do (write-char #\Space out))
               (write-string string out))
             (piece (token) (subseq text (second token) (third token)))
             (collapse (string)
               (let ((words (remove "" (uiop:split-string string :separator '(#\Space #\Tab #\Newline #\Return))
                                    :test #'string=)))
                 (format nil "~{~A~^ ~}" words))))
      (loop for rest on tokens
            for token = (first rest)
            do (ecase (first token)
                 (:text (let ((s (collapse (piece token)))) (when (plusp (length s)) (line s))))
                 (:raw (let ((s (string-trim '(#\Newline #\Return) (piece token))))
                         (when (plusp (length (string-trim " 	" s)))
                           (write-char #\Newline out) (write-string s out))))
                 ((:self :other) (line (piece token)))
                 (:open
                  ;; In HTML, <p>, <li> and the like end where their next sibling starts.
                  (when html
                    (loop while (and open (implied-end-p (first open) (fourth token)))
                          do (pop open) (decf depth)))
                  ;; <tag>short text</tag> stays on one line.
                  (let ((next (second rest)) (after (third rest)))
                    (cond ((and next after (eq (first next) :text) (eq (first after) :close)
                                (string= (fourth after) (fourth token))
                                (< (- (third after) (second token)) 120))
                           (line (format nil "~A~A~A" (piece token) (collapse (piece next)) (piece after)))
                           (pop (cdr rest)) (pop (cdr rest)))
                          ((and next (eq (first next) :close) (string= (fourth next) (fourth token)))
                           (line (format nil "~A~A" (piece token) (piece next)))
                           (pop (cdr rest)))
                          ((and html next after (eq (first next) :text)
                                (or (and (eq (first after) :open)
                                         (implied-end-p (fourth token) (fourth after)))
                                    (and (eq (first after) :close)
                                         (member (fourth after) open :test #'string=)))
                                (< (- (third next) (second token)) 120))
                           (line (format nil "~A~A" (piece token) (collapse (piece next))))
                           (push (fourth token) open) (incf depth)
                           (pop (cdr rest)))
                          (t (line (piece token)) (push (fourth token) open) (incf depth)))))
                 (:close
                  (let ((at (position (fourth token) open :test #'string=)))
                    (cond ((and at (or html (zerop at)))
                           (decf depth (1+ at)) (setf open (nthcdr (1+ at) open)))
                          ((not html) (return-from pretty-markup nil))))
                  (line (piece token)))))
      (when (and open (not html)) (return-from pretty-markup nil))
      (get-output-stream-string out))))

(defun pretty-response-text (text kind)
  "TEXT reformatted for KIND, or NIL when it is not or cannot be."
  (ecase kind
    (:json (pretty-json text))
    (:html (pretty-markup text :html t))
    (:xml (pretty-markup text))
    (:text nil)))
