(in-package #:xeon.http)

(defun query-encode (text)
  "Encode a query component as UTF-8, retaining explicit {{variable}} templates."
  (with-output-to-string (out)
    (loop for i from 0 below (length text) do
      (let ((end (and (<= (+ i 2) (length text)) (string= "{{" text :start2 i :end2 (+ i 2))
                      (search "}}" text :start2 (+ i 2)))))
        (if end
            (progn (write-string text out :start i :end (+ end 2)) (setf i (1+ end)))
            (loop for byte across (babel:string-to-octets (string (char text i)) :encoding :utf-8) do
              (if (or (<= 65 byte 90) (<= 97 byte 122) (<= 48 byte 57) (find byte '(45 46 95 126)))
                  (write-char (code-char byte) out) (format out "%~2,'0X" byte))))))))

(defun query-decode (text)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length text) for c = (char text i) do
      (cond
        ((char= c #\%)
         (let ((a (and (< (+ i 2) (length text)) (digit-char-p (char text (1+ i)) 16)))
               (b (and (< (+ i 2) (length text)) (digit-char-p (char text (+ i 2)) 16))))
           (unless (and a b) (fail-request "Malformed percent escape in query"))
           (vector-push-extend (+ (* 16 a) b) bytes) (incf i 2)))
        ((char= c #\+) (vector-push-extend 32 bytes))
        (t (loop for byte across (babel:string-to-octets (string c) :encoding :utf-8)
                 do (vector-push-extend byte bytes)))))
    (handler-case (babel:octets-to-string bytes :encoding :utf-8)
      (error () (fail-request "Query parameters are not UTF-8; edit the URL in Pro mode")))))

(defun url-parameters (url)
  "Return ordered decoded pairs, retaining duplicates and valueless keys."
  (let* ((fragment (position #\# url)) (end (or fragment (length url)))
         (question (position #\? url :end end)))
    (when (and question (< (1+ question) end))
      (loop for part in (uiop:split-string (subseq url (1+ question) end) :separator "&")
            for equal = (position #\= part)
            collect (cons (query-decode (subseq part 0 equal))
                          (when equal (query-decode (subseq part (1+ equal)))))))))

(defun with-url-parameters (url pairs)
  (let* ((fragment (position #\# url)) (end (or fragment (length url)))
         (question (position #\? url :end end)))
    (concatenate 'string (subseq url 0 (or question end))
      (if pairs
          (format nil "?~{~A~^&~}"
            (mapcar (lambda (pair)
                      (concatenate 'string (query-encode (car pair))
                        (when (cdr pair) (concatenate 'string "=" (query-encode (cdr pair)))))) pairs)) "")
      (subseq url end))))

(defun replace-entry-request (buffer index request &key expected-revision)
  "One undoable edit to one request. Keep its exact preamble and other entries.
Refuse stale forms and ambiguous ### body delimiters instead of losing data."
  (when (and expected-revision (/= expected-revision (xeon.core:buffer-revision buffer)))
    (fail-request "Source changed while the builder was being edited; return to Pro mode"))
  (let* ((source (xeon.core:buffer-text buffer))
         (entries (parse-collection source)) (entry (nth index entries))
         (old (and entry (entry-request entry)))
         (method (request-method request)) (url (request-url request))
         (headers (request-headers request)) (body (request-body request)))
    (unless entry (fail-request "Select an existing request"))
    (unless (and (http-token-p method) (stringp url) (plusp (length url))
                 (notany (lambda (c) (find c '(#\Return #\Newline #\Null))) url))
      (fail-request "Choose a method and enter a single-line URL"))
    (dolist (header headers)
      (unless (and (http-token-p (car header)) (stringp (cdr header))
                   (notany (lambda (c) (find c '(#\Return #\Newline #\Null))) (cdr header)))
        (fail-request "Headers need a valid name and a single-line value")))
    (unless (or (null body) (stringp body)) (fail-request "Builder bodies must be text"))
    (when (some (lambda (line) (uiop:string-prefix-p "###" (fourth line))) (source-lines (or body "")))
      (fail-request "A body line starting with ### is a collection delimiter"))
    ;; Merely opening, sending, or switching modes must never normalize a file.
    (unless (and (equal method (request-method old)) (equal url (request-url old))
                 (equal headers (request-headers old)) (equal (or body "") (or (request-body old) "")))
      (let* ((start (+ (entry-start entry)
                      (first (find-if
                              (lambda (line)
                                (let ((value (fourth line)))
                                  (and (plusp (length value)) (not (find (char value 0) "#@")))))
                              (source-lines (subseq source (entry-start entry) (entry-end entry)))))))
             (crlf (and (position #\Newline source :start start :end (entry-end entry))
                        (let ((p (position #\Newline source :start start :end (entry-end entry))))
                          (and (> p start) (char= (char source (1- p)) #\Return)))))
             (newline (if crlf (format nil "~C~%" #\Return) (string #\Newline)))
             (replacement (with-output-to-string (out)
                            (format out "~A ~A~A" method url newline)
                            (dolist (h headers) (format out "~A: ~A~A" (car h) (cdr h) newline))
                            (write-string newline out) (when body (write-string body out))
                            (when (and (< (entry-end entry) (length source)) body (plusp (length body))
                                       (not (char= (char body (1- (length body))) #\Newline)))
                              (write-string newline out)))))
        (xeon.core:replace-range buffer start (entry-end entry) replacement)
        (xeon.core:set-selection buffer start)))
    request))

(defun basic-authorization (username password)
  "UTF-8 Basic auth value; callers must keep credentials out of saved collections."
  (when (find #\: username) (fail-request "Basic auth usernames cannot contain a colon"))
  (let ((bytes (babel:string-to-octets (concatenate 'string username ":" password) :encoding :utf-8))
        (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (concatenate 'string "Basic "
      (with-output-to-string (out)
        (loop for i from 0 below (length bytes) by 3
              for n = (min 3 (- (length bytes) i))
              for bits = (+ (ash (aref bytes i) 16)
                            (if (> n 1) (ash (aref bytes (1+ i)) 8) 0)
                            (if (> n 2) (aref bytes (+ i 2)) 0)) do
                (write-char (char alphabet (ldb (byte 6 18) bits)) out)
                (write-char (char alphabet (ldb (byte 6 12) bits)) out)
                (write-char (if (> n 1) (char alphabet (ldb (byte 6 6) bits)) #\=) out)
                (write-char (if (> n 2) (char alphabet (ldb (byte 6 0) bits)) #\=) out))))))

(defun entry-request-line (source entry)
  "The offset of ENTRY's METHOD URL line in SOURCE."
  (+ (entry-start entry)
     (first (find-if (lambda (line)
                       (let ((value (fourth line)))
                         (and (plusp (length value)) (not (find (char value 0) "#@")))))
                     (source-lines (subseq source (entry-start entry) (entry-end entry)))))))

(defun source-newline (source)
  (if (search (format nil "~C~%" #\Return) source) (format nil "~C~%" #\Return) (string #\Newline)))

(defun rename-entry (buffer index name)
  "Gives the INDEXth request the ### NAME line, editing only that line. A blank NAME
keeps an unnamed separator; the request is then named after its method and URL."
  (let* ((name (trim-line (or name "")))
         (source (xeon.core:buffer-text buffer))
         (entry (nth index (parse-collection source))))
    (unless entry (fail-request "Select an existing request"))
    (when (find-if (lambda (c) (member c '(#\Return #\Newline #\Null))) name)
      (fail-request "Request names are a single line"))
    (let* ((start (entry-start entry))
           (line-end (or (position #\Newline source :start start) (length source)))
           (content-end (if (and (> line-end start) (char= #\Return (char source (1- line-end))))
                            (1- line-end) line-end))
           (line (trim-line (subseq source start line-end)))
           (header-p (uiop:string-prefix-p "###" line))
           (newline (source-newline source)))
      (cond ((and header-p (string= name (trim-line (subseq line 3)))))
            ;; A blank name keeps the separator; only the name goes.
            (header-p (xeon.core:replace-range buffer start content-end
                                               (if (zerop (length name)) "###" (format nil "### ~A" name))))
            ((or (zerop (length name)) (string= name (entry-name entry))))
            (t (let ((at (entry-request-line source entry)))
                 (xeon.core:replace-range buffer at at (format nil "### ~A~A" name newline)))))))
  index)

(defun duplicate-entry (buffer index)
  "Copies the INDEXth request after itself, named \"<name> copy\". Returns the copy's index."
  (let* ((source (xeon.core:buffer-text buffer))
         (entry (nth index (parse-collection source))))
    (unless entry (fail-request "Select an existing request"))
    ;; The copy's request text is byte-for-byte the original's, body included.
    (let* ((newline (source-newline source))
           (end (entry-end entry))
           (copy (format nil "~:[~A~;~*~]### ~A copy~A~A"
                         (or (zerop end) (char= #\Newline (char source (1- end)))) newline
                         (entry-name entry) newline
                         (subseq source (entry-request-line source entry) end))))
      (xeon.core:replace-range buffer end end copy)
      (1+ index))))

(defun replace-builder-request (buffer index request variables &key expected-revision name)
  "Commit request, its NAME (unless NIL) and collection variables as one undoable,
revision-checked edit. Comments, other requests and body lines beginning with @ are retained."
  (when (and expected-revision (/= expected-revision (xeon.core:buffer-revision buffer)))
    (fail-request "Source changed while the builder was being edited; return to Pro mode"))
  (dolist (pair variables)
    (unless (and (http-token-p (car pair)) (stringp (cdr pair))
                 (notany (lambda (c) (find c '(#\Return #\Newline #\Null))) (cdr pair)))
      (fail-request "Variables need a valid name and a single-line value")))
  (when (/= (length variables) (length (remove-duplicates variables :key #'car :test #'string=)))
    (fail-request "Variable names must be unique"))
  (let* ((scratch (xeon.core:copy-buffer buffer))
         (old (nth-value 1 (parse-collection (xeon.core:buffer-text scratch)))))
    (replace-entry-request scratch index request)
    (when name (rename-entry scratch index name))
    (unless (equal variables old)
      (let* ((source (xeon.core:buffer-text scratch))
             (newline (if (search (format nil "~C~%" #\Return) source)
                          (format nil "~C~%" #\Return) (string #\Newline)))
             (request-p nil) (body-p nil)
             (text (with-output-to-string (out)
                     (dolist (pair variables)
                       (format out "@~A = ~A~A" (car pair) (cdr pair) newline))
                     (dolist (line (source-lines source))
                       (destructuring-bind (start end next value) line
                         (declare (ignore end))
                         (cond ((uiop:string-prefix-p "###" value) (setf request-p nil body-p nil))
                               (body-p nil)
                               ((zerop (length value)) (when request-p (setf body-p t)))
                               ((and (not request-p) (char= (char value 0) #\@)) nil)
                               ((char= (char value 0) #\#) nil)
                               (t (setf request-p t)))
                         (unless (and (not request-p) (not body-p)
                                      (plusp (length value)) (char= (char value 0) #\@))
                           (write-string source out :start start :end next)))))))
        (xeon.core:replace-range scratch 0 (xeon.core:buffer-length scratch) text)))
    (unless (string= (xeon.core:buffer-text buffer) (xeon.core:buffer-text scratch))
      (xeon.core:replace-buffer-from-buffer buffer scratch))
    request))
