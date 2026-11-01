(in-package #:xeon.http)

(defstruct entry name request start end)
(defun trim-line (line) (string-trim '(#\Space #\Tab #\Return) line))

(defun source-lines (text)
  "(start end next line) tuples with character offsets, preserving raw body bytes."
  (loop for start = 0 then next while (< start (length text))
        for end = (or (position #\Newline text :start start) (length text))
        for next = (min (length text) (1+ end))
        collect (list start end next (trim-line (subseq text start end)))))

(defun variable-definition (line)
  (let ((equal (position #\= line)))
    (unless (and equal (> equal 1) (char= (char line 0) #\@))
      (fail-request "Variables must use @name = value"))
    (let ((name (trim-line (subseq line 1 equal))))
      (unless (http-token-p name) (fail-request "Invalid variable name"))
      (cons name (trim-line (subseq line (1+ equal)))))))

(defun parse-variables (text)
  "Read explicit in-memory variables; never consult the process environment."
  (loop for line in (source-lines text) for value = (fourth line)
        unless (or (zerop (length value)) (char= (char value 0) #\#))
          collect (variable-definition value)))

(defun default-collection-source ()
  (format nil "@base = https://postman-echo.com~%~%# Public echo service: send synthetic data only.~%### Echo query parameters~%GET {{base}}/get?message=hello~%Accept: application/json~%~%### Echo a JSON body~%POST {{base}}/post~%Content-Type: application/json~%~%{\"message\": \"Hello from Xeon\"}~%"))

(defun parse-collection (text)
  "Return entries and file variables. Does not resolve placeholders or access disk/network.
Bound interactive collections to 2 MiB; large ordinary documents remain unaffected."
  (when (> (length text) (* 2 1024 1024)) (fail-request "HTTP collection exceeds 2 MiB"))
  (let ((lines (source-lines text)) (entries nil) (variables nil)
        (name nil) (start 0) (request nil) (headers nil) (body-start nil) (in-body nil))
    (labels ((finish (end)
               (when request
                 (setf (request-headers request) (nreverse headers)
                       (request-body request) (when body-start (subseq text body-start end)))
                 (push (make-entry :name (or name (format nil "~A ~A" (request-method request) (request-url request)))
                                   :request request :start start :end end) entries))
               (setf request nil headers nil body-start nil in-body nil)))
      (dolist (line lines)
        (destructuring-bind (begin end next value) line
          (declare (ignore end))
          (cond
            ((uiop:string-prefix-p "###" value)
             (finish begin) (setf start begin name (trim-line (subseq value 3))))
            (in-body nil)
            ((zerop (length value))
             (when request (setf in-body t body-start next)))
            ((and (not request) (char= (char value 0) #\@))
             (push (variable-definition value) variables))
            ((and (not request) (char= (char value 0) #\#)) nil)
            ((not request)
             (let* ((space (position #\Space value))
                    (method (and space (subseq value 0 space)))
                    (url (and space (trim-line (subseq value (1+ space))))))
               (unless (and (http-token-p method) (plusp (length url)))
                 (fail-request "Request line must use METHOD URL"))
               (when (uiop:string-suffix-p " HTTP/1.1" url)
                 (setf url (subseq url 0 (- (length url) 9))))
               (setf request (make-request :method method :url url))))
            (t
             (let ((colon (position #\: value)))
               (unless (and colon (http-token-p (subseq value 0 colon)))
                 (fail-request "Invalid request header; separate the body with a blank line"))
               (push (cons (subseq value 0 colon) (trim-line (subseq value (1+ colon)))) headers))))))
      (finish (length text)))
    (values (nreverse entries) variables)))

(defun entry-at-position (entries position)
  (or (find-if (lambda (entry) (<= (entry-start entry) position (1- (entry-end entry)))) entries)
      (when (and entries (<= 0 position) (< position (entry-start (first entries)))) (first entries))
      (when (and entries (= position (entry-end (car (last entries))))) (car (last entries)))))

(defun expand-template (text variables &optional stack)
  (with-output-to-string (out)
    (loop with start = 0
          for open = (search "{{" text :start2 start)
          do (unless open (write-string text out :start start) (return))
             (write-string text out :start start :end open)
             (let* ((close (search "}}" text :start2 (+ open 2)))
                    (name (and close (trim-line (subseq text (+ open 2) close))))
                    (value (and name (assoc name variables :test #'string=))))
               (unless value (fail-request "Undefined or unterminated request variable"))
               (when (or (member name stack :test #'string=) (> (length stack) 16))
                 (fail-request "Cyclic or excessively nested request variable"))
               (write-string (expand-template (cdr value) variables (cons name stack)) out)
               (setf start (+ close 2))))))

(defun resolve-entry (entry file-variables &key variables (timeout-ms 30000) ca-file)
  "Return a fresh validated request. Explicit private variables override file definitions."
  (let* ((request (copy-request (entry-request entry)))
         (all (append variables file-variables)))
    (flet ((expand (value) (expand-template value all)))
      (setf (request-url request) (expand (request-url request))
            (request-headers request) (mapcar (lambda (header) (cons (car header) (expand (cdr header))))
                                            (request-headers request))
            (request-body request) (when (request-body request) (expand (request-body request)))
            (request-timeout-ms request) timeout-ms (request-ca-file request) ca-file))
    (validate-request request)))

(defun delete-entry (buffer entry)
  "Delete only the request range, preserving all file variable definitions."
  ;; Variables can be inside a request's leading range. Move their exact lines
  ;; outside that range before removing it so deleting a request never deletes them.
  (let* ((text (xeon.core:buffer-substring buffer (entry-start entry) (entry-end entry)))
         (request-seen nil)
         (preserved (with-output-to-string (out)
                      (dolist (line (source-lines text))
                        (let ((value (fourth line)))
                          (when (and (not request-seen) (plusp (length value)))
                            (cond ((char= #\@ (char value 0))
                                   (write-string text out :start (first line) :end (third line)))
                                  ((not (char= #\# (char value 0))) (setf request-seen t)))))))))
    (xeon.core:replace-range buffer (entry-start entry) (entry-end entry) preserved)))

(defun append-request (buffer)
  (let ((start (xeon.core:buffer-length buffer)))
    (xeon.core:insert-text buffer (format nil "~%### New request~%GET https://example.org/~%~%") start)
    (xeon.core:set-selection buffer (1+ start))))
