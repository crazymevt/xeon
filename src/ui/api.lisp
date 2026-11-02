(in-package #:xeon.ui)

;;; API requests. An API tab edits one .http collection, in a visual builder or as
;;; source ("Pro"), and sends the selected request on a worker thread with the active
;;; environment's variables. The response is shown pretty-printed and highlighted,
;;; raw, and as a header table. No rendering callbacks, idle timers or network on the
;;; UI thread.

(defparameter *http-methods* '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS"))

(defparameter *body-types*
  '((:none "No body type" nil) (:json "JSON" "application/json")
    (:form "Form URL-encoded" "application/x-www-form-urlencoded")
    (:xml "XML" "application/xml") (:text "Plain text" "text/plain") (:other "Other" nil))
  "The Body tab's content type choices: (key label media-type).")

(defparameter *auth-types*
  '((:none "None or a custom header") (:bearer "Bearer token") (:basic "Basic"))
  "The Auth tab's choices. Each is stored as an ordinary Authorization header.")

(defclass api-page (document-page)
  ((workbench :initarg :workbench :reader api-workbench)
   (widgets :initform (make-hash-table) :reader api-widgets)
   (index :initform 0 :accessor api-index)
   (revision :initform nil :accessor api-revision)
   (dirty :initform nil :accessor api-dirty)
   (loading :initform nil :accessor api-loading)
   (job :initform nil :accessor api-job)
   (response :initform nil :accessor api-response)
   ;; The decoded response body, or NIL when it is not text.
   (response-text :initform nil :accessor api-response-text)
   ;; Where the Auth tab's Authorization header sat among the headers, or NIL.
   (auth-index :initform nil :accessor api-auth-index)
   (response-document :initform nil :accessor api-response-document)
   (response-view :initform nil :accessor api-response-view)
   (rows :initform (make-hash-table) :reader api-rows))
  (:default-initargs :orientation :vertical))

(defun api-buffer (page) (document-buffer (page-document page)))

(defun api-pages (workbench)
  (remove-if-not (lambda (page) (typep page 'api-page)) (children (workbench-tabs workbench))))

(defun active-api-page (workbench)
  (let ((page (selected-page (workbench-tabs workbench))))
    (and (typep page 'api-page) page)))

(defun api-message (page message)
  (setf (text (part page :message)) message))

(defun api-mark-dirty (page)
  (unless (api-loading page)
    (unless (api-dirty page)
      (setf (api-dirty page) t)
      (api-message page "Unapplied changes · Apply before sending, saving or switching modes.")
      (set-enabled (part page :apply) t)
      (set-enabled (part page :revert) t))))

(defun api-draft-ready (workbench document)
  (let ((page (find-document-page workbench document)))
    (if (and (typep page 'api-page) (api-dirty page))
        (progn
          (select-page (workbench-tabs workbench) page)
          (api-message page "Apply or Revert the builder changes first.")
          (report workbench "API builder has unapplied changes. Choose Apply or Revert.")
          nil)
        t)))

(defun api-pro-p (page)
  (eq (selected-page (part page :mode)) (page-editor page)))

;;; Key/value rows

(defun api-pairs (page key)
  (mapcar (lambda (row)
            (cons (string-trim " " (text (second row)))
                  (if (and (eq key :params) (not (checked (fourth row))))
                      nil (text (third row)))))
          (remove-if (lambda (row) (and (blank-p (text (second row))) (blank-p (text (third row)))))
                     (gethash key (api-rows page)))))

(defun api-add-row (page key &optional (name "") (value ""))
  (let* ((box (part page key))
         (row (make-instance 'hbox :spacing 6))
         (name-field (make-instance 'entry :text name :placeholder "Name" :width-chars 18 :control-size :small))
         (value-field (make-instance 'entry :text (or value "") :placeholder "Value" :hexpand t :control-size :small))
         (has-value (when (eq key :params)
                      (make-instance 'check-box :label "=" :checked (not (null value))
                                     :tooltip "Include an equals sign; uncheck for a valueless parameter")))
         (remove (make-instance 'button :style :icon :label "Remove" :tooltip "Remove this row"
                                        :image (icon :close :size 14) :control-size :small))
         (record (list row name-field value-field has-value)))
    (add row name-field) (add row value-field :expand t)
    (when has-value (add row has-value))
    (add row remove)
    (add box row)
    (setf (gethash key (api-rows page)) (append (gethash key (api-rows page)) (list record)))
    (dolist (field (list name-field value-field))
      (on field :changed (lambda (e) (declare (ignore e)) (api-mark-dirty page))))
    (when has-value
      (on has-value :toggled (lambda (e) (declare (ignore e)) (api-mark-dirty page))))
    (on remove :clicked
        (lambda (e)
          (declare (ignore e))
          (setf (gethash key (api-rows page)) (remove record (gethash key (api-rows page))))
          (remove-child box row) (destroy row) (api-mark-dirty page)))
    record))

(defun api-fill-pairs (page key pairs)
  (dolist (row (copy-list (children (part page key))))
    (remove-child (part page key) row) (destroy row))
  (setf (gethash key (api-rows page)) nil)
  (dolist (pair pairs) (api-add-row page key (car pair) (cdr pair))))

(defun api-header-row (page name)
  (find-if (lambda (row) (string-equal name (string-trim " " (text (second row)))))
           (gethash :headers (api-rows page))))

;;; Auth: an Authorization header shown as fields

(defun split-authorization (headers)
  "The Auth tab's view of HEADERS: (type . fields), the other headers, and where the
recognised Authorization header was. Unrecognised schemes stay ordinary headers."
  (let* ((index (position "Authorization" headers :key #'car :test #'string-equal))
         (value (and index (cdr (nth index headers)))))
    (flet ((without () (append (subseq headers 0 index) (nthcdr (1+ index) headers)))
           (scheme-p (scheme) (and (> (length value) (length scheme))
                                   (string-equal scheme value :end2 (length scheme)))))
      (cond ((null index) (values (list :none) headers nil))
            ((scheme-p "Bearer ") (values (list :bearer (subseq value 7)) (without) index))
            ((and (scheme-p "Basic ") (position #\: value :start 6))
             (let ((colon (position #\: value :start 6)))
               (values (list :basic (subseq value 6 colon) (subseq value (1+ colon))) (without) index)))
            (t (values (list :none) headers nil))))))

(defun api-auth-type (page)
  (first (selected-item (part page :auth-type))))

(defun api-show-auth (page type)
  (select-page (part page :auth-fields) (part page (ecase type (:none :auth-none) (:bearer :auth-bearer) (:basic :auth-basic)))))

(defun api-fill-auth (page auth)
  (with-handlers-blocked ((part page :auth-type) :changed)
    (setf (selected-index (part page :auth-type)) (position (first auth) *auth-types* :key #'first)))
  (setf (text (part page :bearer-token)) (if (eq (first auth) :bearer) (second auth) "")
        (text (part page :basic-user)) (if (eq (first auth) :basic) (second auth) "")
        (text (part page :basic-password)) (if (eq (first auth) :basic) (third auth) ""))
  (api-show-auth page (first auth)))

(defun api-authorization (page)
  "The Authorization value the Auth tab describes, or NIL."
  (ecase (api-auth-type page)
    (:none nil)
    (:bearer (format nil "Bearer ~A" (string-trim " " (text (part page :bearer-token)))))
    (:basic (let ((user (text (part page :basic-user))))
              (when (find #\: user) (error "Basic auth usernames cannot contain a colon."))
              (format nil "Basic ~A:~A" user (text (part page :basic-password)))))))

(defun api-request-headers (page)
  "The header rows with the Auth tab's header back where it was."
  (let ((headers (api-pairs page :headers))
        (authorization (api-authorization page)))
    (if authorization
        (let ((headers (remove "Authorization" headers :key #'car :test #'string-equal))
              (at (api-auth-index page)))
          (if (and at (<= at (length headers)))
              (append (subseq headers 0 at) (list (cons "Authorization" authorization)) (nthcdr at headers))
              (append headers (list (cons "Authorization" authorization)))))
        headers)))

;;; Body type: the Content-Type header

(defun body-type-for (content-type)
  (let ((media (xeon.http:media-type content-type)))
    (cond ((null content-type) :none)
          ((find media *body-types* :key #'third :test #'equal) (first (find media *body-types* :key #'third :test #'equal)))
          ((and media (uiop:string-suffix-p media "+json")) :json)
          (t :other))))

(defun api-refresh-body-type (page)
  (let* ((row (api-header-row page "Content-Type"))
         (type (body-type-for (and row (text (third row))))))
    (with-handlers-blocked ((part page :body-type) :changed)
      (setf (selected-index (part page :body-type)) (position type *body-types* :key #'first)))
    (set-enabled (part page :format-body) (eq type :json))))

(defun api-choose-body-type (page type)
  "Sets the Content-Type header row for TYPE; None removes it, Other leaves it."
  (let ((row (api-header-row page "Content-Type"))
        (media (third (find type *body-types* :key #'first))))
    (cond (media (if row
                     (setf (text (third row)) media)
                     (api-add-row page :headers "Content-Type" media)))
          ((and (eq type :none) row)
           (setf (gethash :headers (api-rows page)) (remove row (gethash :headers (api-rows page))))
           (remove-child (part page :headers) (first row))
           (destroy (first row))))
    (set-enabled (part page :format-body) (eq type :json))
    (api-mark-dirty page)))

(defun api-format-body (page)
  (let ((pretty (xeon.http:pretty-json (text (part page :body)))))
    (unless pretty (error "The body is not valid JSON."))
    (unless (string= pretty (text (part page :body)))
      (setf (text (part page :body)) pretty)
      (api-mark-dirty page))))

;;; Loading and applying

(defun api-default-name-p (entry)
  (let ((request (xeon.http:entry-request entry)))
    (string= (xeon.http:entry-name entry)
             (format nil "~A ~A" (xeon.http:request-method request) (xeon.http:request-url request)))))

(defun api-load-builder (page)
  ;; Parse and decode before changing widgets: malformed source remains intact in Pro.
  (multiple-value-bind (entries variables)
      (xeon.http:parse-collection (xeon.core:buffer-text (api-buffer page)))
    (let* ((index (min (api-index page) (max 0 (1- (length entries)))))
           (entry (nth index entries))
           (request (and entry (xeon.http:entry-request entry)))
           (pairs (and request (xeon.http:url-parameters (xeon.http:request-url request)))))
      (unless entry (error "No requests. Add a METHOD URL request in Pro mode, or choose New Request."))
      (when (some (lambda (list) (> (length list) 200))
                  (list entries pairs variables (xeon.http:request-headers request)))
        (error "This collection is too large for the visual builder; use Pro mode (200 rows per section)."))
      (multiple-value-bind (auth headers auth-index) (split-authorization (xeon.http:request-headers request))
        (setf (api-loading page) t)
        (unwind-protect
             (progn
               (setf (api-index page) index (api-auth-index page) auth-index)
               (with-handlers-blocked ((part page :requests) :changed)
                 (setf (items (part page :requests)) entries
                       (selected-index (part page :requests)) index))
               (setf (text (part page :name)) (if (api-default-name-p entry) "" (xeon.http:entry-name entry))
                     (placeholder (part page :name)) (format nil "~A ~A" (xeon.http:request-method request)
                                                             (xeon.http:request-url request))
                     (text (part page :method)) (xeon.http:request-method request)
                     (text (part page :url)) (xeon.http:with-url-parameters (xeon.http:request-url request) nil)
                     (text (part page :body)) (or (xeon.http:request-body request) ""))
               (api-fill-pairs page :params pairs)
               (api-fill-pairs page :headers headers)
               (api-fill-pairs page :variables variables)
               (api-fill-auth page auth)
               (api-refresh-body-type page)
               (setf (api-revision page) (xeon.core:buffer-revision (api-buffer page))
                     (api-dirty page) nil)
               (set-enabled (part page :apply) nil)
               (set-enabled (part page :revert) nil)
               (set-enabled (part page :delete) t)
               (set-enabled (part page :duplicate) t)
               (api-message page "Ready · Use {{name}} for collection or environment variables. Requests are sent only when you choose Send."))
          (setf (api-loading page) nil))))))

(defun api-apply (page)
  (when (api-dirty page)
    ;; The endpoint field accepts pasted full URLs. Explicit query text there
    ;; replaces the query rows; never silently discard a pasted query.
    (let* ((endpoint (string-trim " " (text (part page :url))))
           (fragment (position #\# endpoint))
           (question (position #\? endpoint :end (or fragment (length endpoint))))
           (pairs (if question (xeon.http:url-parameters endpoint) (api-pairs page :params)))
           (request (xeon.http:make-request
                     :method (string-upcase (string-trim " " (text (part page :method))))
                     :url (xeon.http:with-url-parameters endpoint pairs)
                     :headers (api-request-headers page) :body (text (part page :body)))))
      (xeon.http:replace-builder-request (api-buffer page) (api-index page) request
                                        (api-pairs page :variables)
                                        :expected-revision (api-revision page)
                                        :name (text (part page :name)))
      (api-load-builder page)
      (refresh-api-sidebar (api-workbench page))))
  t)

(defun api-set-mode (page pro-p)
  (unless (api-draft-ready (api-workbench page) (page-document page))
    (return-from api-set-mode nil))
  (let* ((source (xeon.core:buffer-text (api-buffer page)))
         (entries (if pro-p (ignore-errors (xeon.http:parse-collection source))
                      (xeon.http:parse-collection source)))
         (entry (if pro-p (nth (api-index page) entries)
                    (xeon.http:entry-at-position entries (cursor (page-editor page))))))
    (if pro-p
        (when entry (set-selection (page-editor page) (xeon.http:entry-start entry) (xeon.http:entry-start entry)))
        (progn
          (when entry (setf (api-index page) (position entry entries)))
          (api-load-builder page))))
  (select-page (part page :mode) (if pro-p (page-editor page) (part page :builder)))
  (set-enabled (part page :pro) (not pro-p))
  (set-enabled (part page :visual) pro-p)
  (dolist (key '(:requests :new-request :duplicate :delete))
    (set-enabled (part page key) (not pro-p)))
  (when pro-p (focus (page-editor page)))
  t)

(defun api-select-request (page index)
  "Shows the INDEXth request, in the builder or at the cursor in Pro mode."
  (cond ((api-dirty page) (api-message page "Apply or Revert before selecting another request.") nil)
        ((api-pro-p page)
         (let ((entry (nth index (xeon.http:parse-collection (xeon.core:buffer-text (api-buffer page))))))
           (when entry
             (setf (api-index page) index)
             (set-selection (page-editor page) (xeon.http:entry-start entry) (xeon.http:entry-start entry))
             (focus (page-editor page)))
           t))
        (t (setf (api-index page) index) (api-load-builder page) t)))

(defun api-new-request (page)
  (when (api-draft-ready (api-workbench page) (page-document page))
    (let ((buffer (api-buffer page)))
      (xeon.http:append-request buffer)
      (setf (api-index page) (1- (length (xeon.http:parse-collection (xeon.core:buffer-text buffer)))))
      (api-load-builder page)
      (refresh-api-sidebar (api-workbench page))
      (focus (part page :url)))))

(defun api-duplicate-request (page)
  (when (api-draft-ready (api-workbench page) (page-document page))
    (setf (api-index page) (xeon.http:duplicate-entry (api-buffer page) (api-index page)))
    (api-load-builder page)
    (refresh-api-sidebar (api-workbench page))))

(defun api-delete-request (page &key (confirm t))
  (when (api-draft-ready (api-workbench page) (page-document page))
    (let* ((buffer (api-buffer page))
           (entry (nth (api-index page) (xeon.http:parse-collection (xeon.core:buffer-text buffer)))))
      (when (and entry
                 (or (not confirm)
                     (confirm (format nil "Delete the request “~A”?" (xeon.http:entry-name entry))
                              :detail "Collection variables are kept. Undo restores the request."
                              :yes "Delete" :parent (workbench-window (api-workbench page)))))
        (xeon.http:delete-entry buffer entry)
        (setf (api-index page) (max 0 (1- (api-index page))))
        (refresh-api-sidebar (api-workbench page))
        (handler-case (api-load-builder page)
          (error (condition)
            (api-set-mode page t)
            (api-message page (princ-to-string condition))))))))

;;; Sending

(defun api-environment-bindings (workbench)
  (xeon.http:environment-bindings (current-api-environment workbench)))

(defun api-send (page)
  (when (api-job page) (return-from api-send nil))
  (unless (api-draft-ready (api-workbench page) (page-document page))
    (return-from api-send nil))
  (let ((workbench (api-workbench page)))
    (multiple-value-bind (entries variables)
        (xeon.http:parse-collection (xeon.core:buffer-text (api-buffer page)))
      (let* ((entry (if (api-pro-p page)
                        (xeon.http:entry-at-position entries (cursor (page-editor page)))
                        (nth (api-index page) entries)))
             (environment (current-api-environment workbench))
             (request (and entry
                           (handler-case
                               (xeon.http:resolve-entry entry variables
                                                        :variables (xeon.http:environment-bindings environment))
                             (error (condition)
                               (error "~A" (xeon.http:redact-secrets (princ-to-string condition) environment))))))
             (backend *backend*))
        (unless request (error "Select a request to send."))
        (setf (api-job page)
              (xeon.http:start-request request
                :on-done (lambda (job)
                           (let ((*backend* backend))
                             (call-in-ui-thread (lambda () (api-finished page job)) :owner page)))))
        (set-enabled (part page :send) nil)
        (set-enabled (part page :cancel) t)
        (setf (active (part page :spinner)) t
              (text-color (part page :summary)) :secondary-text
              (text (part page :summary))
              (format nil "Sending ~A ~A…~@[ · ~A~]" (xeon.http:request-method request)
                      (xeon.http:entry-name entry)
                      (and environment (xeon.http:environment-name environment))))
        t))))

(defun api-cancel (page)
  (when (api-job page)
    (xeon.http:cancel-request (api-job page))
    (set-enabled (part page :cancel) nil)
    (setf (text (part page :summary)) "Cancelling…")))

(defun release-api-page (page)
  (when (api-job page) (xeon.http:cancel-request (api-job page)) (setf (api-job page) nil))
  (let ((d (api-response-document page)) (view (api-response-view page)))
    (when d
      (release-editor-view view)
      (setf (document-views d) '())
      (release-document d)
      (setf (api-response-document page) nil))))

;;; The response

(defun byte-size-label (bytes)
  (cond ((< bytes 1024) (format nil "~:D B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024)))
        (t (format nil "~,1F MB" (/ bytes 1024 1024)))))

(defun response-status-line (response)
  "The reason phrase of RESPONSE's final status line, such as \"Not Found\", or NIL."
  (let ((reason nil))
    (dolist (line (uiop:split-string (xeon.http:response-headers-text response) :separator '(#\Newline)) reason)
      (let ((line (string-right-trim '(#\Return) line)))
        (when (uiop:string-prefix-p "HTTP/" line)
          (let* ((first (position #\Space line))
                 (second (and first (position #\Space line :start (1+ first)))))
            (setf reason (and second (string-trim " " (subseq line (1+ second)))))))))))

(defun decode-response (response)
  "RESPONSE's body as text in its declared charset (UTF-8 by default), or NIL for binary."
  (let* ((type (or (xeon.http:response-header-value response "Content-Type") ""))
         (charset (let ((at (search "charset=" type :test #'char-equal)))
                    (and at (string-trim " \"'" (subseq type (+ at 8) (or (position #\; type :start at) (length type)))))))
         (encoding (cond ((null charset) :utf-8)
                         ((member charset '("iso-8859-1" "latin1" "latin-1" "us-ascii" "windows-1252") :test #'string-equal)
                          :latin-1)
                         (t :utf-8)))
         (body (xeon.http:response-body response)))
    (unless (find 0 body)
      (handler-case (xeon.http:response-text response :encoding encoding)
        (error () nil)))))

(defun response-language (kind)
  (ecase kind (:json :json) ((:html :xml) :markup) (:text :plain)))

(defun api-show-pretty (page text language)
  (let* ((d (api-response-document page))
         (buffer (document-buffer d))
         (view (api-response-view page)))
    (xeon.syntax:set-buffer-language buffer language)
    (xeon.core:replace-range buffer 0 (xeon.core:buffer-length buffer) text)
    (restart-highlighting d)
    (set-selection view 0 0)))

(defun api-display-response (page)
  "Fills the Pretty, Raw and Headers views from the current response."
  (let* ((response (api-response page))
         (text (and response (decode-response response)))
         (kind (and text (xeon.http:content-kind (xeon.http:response-header-value response "Content-Type") text)))
         (pretty (and text (xeon.http:pretty-response-text text kind))))
    (setf (api-response-text page) text)
    (cond ((null response)
           (api-show-pretty page "" :plain)
           (setf (text (part page :output)) ""
                 (items (part page :response-headers)) #()))
          (t
           (let ((placeholder (format nil "Binary response · ~A. Choose Save… to write it to a file."
                                      (byte-size-label (xeon.http:response-size response)))))
             (api-show-pretty page (or pretty text placeholder) (if text (response-language kind) :plain))
             (setf (text (part page :output)) (or text placeholder)
                   (items (part page :response-headers)) (xeon.http:response-header-pairs response)))))
    (setf (text (part page :response-kind))
          (cond ((null response) "")
                ((null text) "Binary")
                (t (format nil "~A~:[ · shown as received~;~]"
                           (ecase kind (:json "JSON") (:html "HTML") (:xml "XML") (:text "Text"))
                           (or pretty (eq kind :text))))))
    (select-page (part page :response-body) (part page (if response :response-views :response-empty)))
    (set-enabled (part page :copy-response) response)
    (set-enabled (part page :save-response) response)))

(defun api-show-response (page headers-p)
  "Shows the response's headers, or with HEADERS-P NIL its pretty body."
  (select-page (part page :response-tabs) (part page (if headers-p :headers-page :pretty-page))))

(defun api-finished (page job)
  (when (eq job (api-job page))
    (multiple-value-bind (state response failure) (xeon.http:job-snapshot job)
      (setf (api-job page) nil)
      (set-enabled (part page :send) t)
      (set-enabled (part page :cancel) nil)
      (setf (active (part page :spinner)) nil)
      (when response (setf (api-response page) response))
      (setf (text-color (part page :summary))
            (cond ((null response) (if (eq state :cancelled) :secondary-text :system-red))
                  ((>= (xeon.http:response-status response) 400) :system-red)
                  ((>= (xeon.http:response-status response) 300) :system-orange)
                  (t :system-green)))
      (setf (text (part page :summary))
            (case state
              (:complete (format nil "~D~@[ ~A~]  ·  ~,0F ms  ·  ~A"
                                 (xeon.http:response-status response)
                                 (response-status-line response)
                                 (* 1000 (xeon.http:response-elapsed-seconds response))
                                 (byte-size-label (xeon.http:response-size response))))
              (:cancelled "Request cancelled")
              (t (format nil "Request failed · ~A"
                         (xeon.http:redact-secrets (princ-to-string failure)
                                                   (current-api-environment (api-workbench page)))))))
      (when response (api-display-response page)))))

(defun api-response-copy-text (page)
  (let ((tabs (part page :response-tabs)))
    (cond ((eq (selected-page tabs) (part page :headers-page))
           (format nil "~{~A~^~%~}" (map 'list (lambda (h) (format nil "~A: ~A" (car h) (cdr h)))
                                       (items (part page :response-headers)))))
          ((eq (selected-page tabs) (part page :raw-page)) (text (part page :output)))
          (t (xeon.core:buffer-text (document-buffer (api-response-document page)))))))

(defun api-save-response (page)
  (let* ((response (api-response page))
         (kind (and (api-response-text page)
                    (xeon.http:content-kind (xeon.http:response-header-value response "Content-Type")
                                            (api-response-text page))))
         (path (and response
                    (choose-file :mode :save :parent (workbench-window (api-workbench page))
                                 :name (format nil "response.~A" (case kind (:json "json") (:html "html") (:xml "xml")
                                                                   (:text "txt") (t "bin")))))))
    (when path
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (xeon.http:response-body response) out))
      (report (api-workbench page) "Saved the response to ~A." (file-namestring path)))))

;;; Building the tab

(defun make-api-section (page key title hint)
  (let ((rows (make-instance 'vbox :spacing 6))
        (add (small-button "Add" :image (icon :add :size 14) :tooltip (format nil "Add a ~A row" (string-downcase title)))))
    (setf (part page key) rows)
    (on add :clicked (lambda (e) (declare (ignore e))
                       (let ((record (api-add-row page key)))
                         (api-mark-dirty page)
                         (focus (second record)))))
    (let ((scroll (make-instance 'scrolled-window)))
      (add scroll (ui (vbox :spacing 8 :padding 12
                        (hbox :spacing 8 (:widget (hint-label hint) :expand t) (:widget add))
                        (:widget rows) (spacer))))
      scroll)))

(defun make-api-auth-page (page)
  (let* ((type (make-instance 'combo-box :items *auth-types* :label-function #'second :selected-index 0))
         (fields (make-instance 'stack))
         (token (make-instance 'entry :placeholder "{{token}}" :hexpand t))
         (user (make-instance 'entry :placeholder "{{username}}" :hexpand t))
         (password (make-instance 'entry :placeholder "{{password}}" :hexpand t))
         (none (ui (vbox (:widget (hint-label "No Authorization header from this tab. A header with another scheme stays in Headers.")) (spacer))))
         (bearer (ui (grid :columns 2 :column-spacing 10 :row-spacing 8
                       (label :text "Token" :halign :end :valign :center) (:widget token))))
         (basic (ui (grid :columns 2 :column-spacing 10 :row-spacing 8
                      (label :text "Username" :halign :end :valign :center) (:widget user)
                      (label :text "Password" :halign :end :valign :center) (:widget password)))))
    (flet ((keep (name widget) (setf (part page name) widget)))
      (keep :auth-type type) (keep :auth-fields fields) (keep :bearer-token token)
      (keep :basic-user user) (keep :basic-password password)
      (keep :auth-none none) (keep :auth-bearer bearer) (keep :auth-basic basic))
    (add-page fields none "None") (add-page fields bearer "Bearer") (add-page fields basic "Basic")
    (on type :changed (lambda (e) (api-show-auth page (first (event-value e))) (api-mark-dirty page)))
    (dolist (field (list token user password))
      (on field :changed (lambda (e) (declare (ignore e)) (api-mark-dirty page))))
    (ui (vbox :spacing 10 :padding 12
          (hbox :spacing 10 (label :text "Type" :valign :center) (:widget type))
          (:widget fields)
          (:widget (hint-label "Credentials are written into the collection file as typed. Put secrets in an environment and refer to them as {{name}}, so the file only holds the placeholder."))
          (spacer)))))

(defun make-api-body-page (page body)
  (let ((type (make-instance 'combo-box :items *body-types* :label-function #'second :selected-index 0
                                        :tooltip "Sets the Content-Type header"))
        (format-button (small-button "Format JSON" :enabled nil :tooltip "Re-indent the JSON body")))
    (setf (part page :body-type) type (part page :format-body) format-button)
    (on type :changed (lambda (e) (api-choose-body-type page (first (event-value e)))))
    (on format-button :clicked
        (lambda (e) (declare (ignore e))
          (handler-case (api-format-body page)
            (error (condition) (api-message page (princ-to-string condition))))))
    (ui (vbox :spacing 0
          (hbox :padding '(8 12 8 12) :spacing 8
            (label :text "Content type" :valign :center) (:widget type) (spacer) (:widget format-button))
          (separator)
          (:widget body :expand t)))))

(defun make-api-response-pane (page workbench)
  (let* ((themes (workbench-themes workbench))
         (buffer (xeon.core:make-buffer :name "response" :text "" :history-limit 0))
         (document (make-instance 'document :workbench workbench :buffer buffer))
         (pretty (make-instance 'editor-view :document document :buffer (document-text document)
                                             :editable nil :line-numbers-p t :tab-width 2
                                             :font (editor-font themes)
                                             :text-styles (syntax-text-styles (workbench-palette workbench))))
         (raw (make-instance 'text-view :editable nil :wrap t :font (editor-font themes)))
         (headers (make-instance 'table-view :headers t :sortable nil :selection-mode :multiple
                                             :columns (list (make-column "Name" #'car :width 200)
                                                            (make-column "Value" #'cdr))))
         (tabs (make-instance 'notebook))
         (body (make-instance 'stack))
         (spinner (make-instance 'spinner :active nil))
         (summary (make-instance 'label :text "" :font (make-font :size 12 :weight :bold) :valign :center))
         (kind (make-instance 'label :text "" :text-color :secondary-text :font (make-font :size 11) :valign :center))
         (copy (small-button "Copy" :enabled nil :tooltip "Copy the shown view: pretty body, raw body or headers"))
         (save (small-button "Save…" :enabled nil :tooltip "Write the response body, as received, to a file"))
         (empty-title (message-title))
         (empty-text (message-text))
         (empty (centered-message :api empty-title empty-text)))
    ;; The response view keeps no undo history: responses are replaced, never edited.
    (setf (history-limit (document-text document)) 0)
    (push pretty (document-views document))
    (setf (api-response-document page) document (api-response-view page) pretty)
    (setf (text empty-title) "No response yet"
          (text empty-text) (format nil "Choose Send, or press ~A, to send the selected request."
                                    (shortcut-label "Primary+Return")))
    (flet ((keep (name widget) (setf (part page name) widget)))
      (keep :output raw) (keep :response-headers headers) (keep :response-tabs tabs)
      (keep :response-body body) (keep :spinner spinner) (keep :summary summary)
      (keep :response-kind kind) (keep :copy-response copy) (keep :save-response save)
      (keep :pretty-page pretty) (keep :raw-page raw) (keep :headers-page headers)
      (keep :response-empty empty) (keep :response-views tabs))
    (add-page tabs pretty "Pretty") (add-page tabs raw "Raw") (add-page tabs headers "Headers")
    (add-page body empty "Empty") (add-page body tabs "Response")
    (on copy :clicked (action (workbench) (setf (clipboard-text) (api-response-copy-text page))))
    (on save :clicked (action (workbench) (api-save-response page)))
    (ui (vbox :spacing 0
          (hbox :padding '(8 14 8 14) :spacing 8
            (label :text "RESPONSE" :font (make-font :size 11 :weight :bold) :text-color :secondary-text :valign :center)
            (:widget spinner) (:widget summary) (:widget kind) (spacer) (:widget copy) (:widget save))
          (separator)
          (:widget body :expand t)))))

(defun environment-label (environment)
  (if (eq environment :none)
      "No environment"
      (format nil "~A~:[ · Project~;~]" (xeon.http:environment-name environment)
              (eq :system (xeon.http:environment-scope environment)))))

(defun make-environment-combo (workbench)
  (let ((combo (make-instance 'combo-box :items '(:none) :selected-index 0 :label-function #'environment-label
                                         :tooltip "Variables from the chosen environment fill {{name}} placeholders")))
    (on combo :changed
        (lambda (e) (with-reported-errors (workbench) (choose-api-environment workbench (event-value e)))))
    combo))

(defun open-api-workspace (workbench &key buffer)
  (let* ((opened buffer)
         (buffer (or buffer (xeon.core:new-buffer (workbench-manager workbench) :name "requests.http"
                                      :text (xeon.http:default-collection-source) :history-limit 0)))
         (document (make-instance 'document :workbench workbench :buffer buffer
                                  :on-change (lambda (d) (document-changed workbench d))))
         (editor (make-editor-view workbench document))
         (page (make-instance 'api-page :workbench workbench :editor editor))
         (root (make-instance 'vbox :spacing 0))
         (mode (make-instance 'stack))
         (builder (make-instance 'vbox :spacing 10 :padding '(12 14 8 14)))
         (sections (make-instance 'notebook))
         (requests (make-instance 'combo-box :label-function #'xeon.http:entry-name
                                             :tooltip "The requests in this collection"))
         (new-request (small-button "New" :image (icon :add :size 14) :tooltip "Add a request to this collection"))
         (duplicate (small-button "Duplicate" :tooltip "Copy this request below itself"))
         (delete (small-button "Delete" :tooltip "Delete this request from the collection"))
         (name (make-instance 'entry :placeholder "Request name" :hexpand t :control-size :small))
         (method (make-instance 'combo-entry :items *http-methods* :text "GET" :width 118
                                           :tooltip "The HTTP method; choose one or type another"))
         (url (make-instance 'entry :placeholder "https://api.example.com/resource or {{base}}/resource" :hexpand t))
         (body (make-instance 'text-view :wrap nil :font (editor-font (workbench-themes workbench))))
         (message (hint-label ""))
         (environment (make-environment-combo workbench))
         (environments (make-instance 'button :style :icon :label "Environments" :tooltip "Edit environments and secrets"
                                              :image (icon :key :size 16) :control-size :small))
         (visual (small-button "Builder" :enabled nil :tooltip "Edit the selected request in fields"))
         (pro (small-button "Source" :tooltip "Edit the collection as .http source"))
         (apply-button (small-button "Apply" :enabled nil :tooltip "Write the builder's changes into the collection"))
         (revert (small-button "Revert" :enabled nil :tooltip "Discard the builder's changes"))
         (send (make-instance 'button :label "Send" :image (icon :send :size 14)
                                      :tooltip (format nil "Send the request · ~A; in Source mode, the request at the cursor"
                                                       (shortcut-label "Primary+Return"))))
         (cancel (small-button "Cancel" :enabled nil :tooltip "Stop waiting for the response")))
    (loop for key in '(:mode :builder :requests :new-request :duplicate :delete :name :method :url :body
                       :message :environment :visual :pro :apply :revert :send :cancel)
          for widget in (list mode builder requests new-request duplicate delete name method url body
                              message environment visual pro apply-button revert send cancel)
          do (setf (part page key) widget))
    ;; Toolbar: environment and mode.
    (add root (ui (hbox :padding '(8 14 8 14) :spacing 8
                    (label :text "Environment" :text-color :secondary-text :font (make-font :size 11) :valign :center)
                    (:widget environment) (:widget environments)
                    (spacer) (:widget visual) (:widget pro))))
    (add root (make-instance 'separator))
    ;; Builder
    (add builder (ui (hbox :spacing 6 (:widget requests :expand t) (:widget new-request) (:widget duplicate) (:widget delete))))
    (add builder (ui (hbox :spacing 8
                       (label :text "Name" :text-color :secondary-text :valign :center) (:widget name :expand t))))
    (add builder (ui (hbox :spacing 8 (:widget method) (:widget url :expand t) (:widget cancel) (:widget send))))
    (add-page sections (make-api-section page :params "Query parameters"
                                         "Sent in the URL. Uncheck = for a parameter without a value.")
              "Params")
    (add-page sections (make-api-section page :headers "Headers" "Sent with the request. Body and Auth set some for you.")
              "Headers")
    (add-page sections (make-api-auth-page page) "Auth")
    (add-page sections (make-api-body-page page body) "Body")
    (add-page sections (make-api-section page :variables "Variables"
                                         "Collection variables (@name = value) for every request in this file. The environment's variables take precedence.")
              "Variables")
    (add builder sections :expand t)
    (add builder (ui (hbox :spacing 8 (:widget message :expand t) (:widget revert) (:widget apply-button))))
    (add-page mode builder "Builder") (add-page mode editor "Source")
    (add root mode :expand t)
    (add page root)
    (add page (make-api-response-pane page workbench))
    (dolist (field (list name url body))
      (on field :changed (lambda (e) (declare (ignore e)) (api-mark-dirty page))))
    (on method :changed (lambda (e) (declare (ignore e)) (api-mark-dirty page)))
    (on url :activate (lambda (e) (declare (ignore e))
                        (handler-case (progn (api-apply page) (api-send page))
                          (error (condition) (api-message page (princ-to-string condition))))))
    (flet ((wire (widget function)
             (on widget :clicked
                 (lambda (e) (declare (ignore e))
                   (handler-case (funcall function)
                     (error (condition) (api-message page (princ-to-string condition))))))))
      (wire new-request (lambda () (api-new-request page)))
      (wire duplicate (lambda () (api-duplicate-request page)))
      (wire delete (lambda () (api-delete-request page)))
      (wire visual (lambda () (api-set-mode page nil)))
      (wire pro (lambda () (api-set-mode page t)))
      (wire apply-button (lambda () (api-apply page)))
      (wire revert (lambda () (api-load-builder page)))
      ;; Send applies builder edits first, as the button promises to send what is shown.
      (wire send (lambda () (unless (api-pro-p page) (api-apply page)) (api-send page)))
      (wire cancel (lambda () (api-cancel page)))
      (wire environments (lambda () (open-environments workbench))))
    (on requests :changed
        (lambda (e)
          (declare (ignore e))
          (unless (api-loading page)
            (if (api-dirty page)
                (progn
                  (with-handlers-blocked (requests :changed) (setf (selected-index requests) (api-index page)))
                  (api-message page "Apply or Revert before selecting another request."))
                (handler-case
                    (progn (setf (api-index page) (or (selected-index requests) 0)) (api-load-builder page))
                  (error (condition) (api-message page (princ-to-string condition))))))))
    ;; A new collection replaces an empty untitled tab; OPEN-DOCUMENT does that for files.
    (let ((scratch (and (not opened) (disposable-document workbench))))
      (push document (workbench-documents workbench))
      (add (workbench-tabs workbench) page :tab-label (document-name document)
                                           :tab-icon (icon :api :size 14))
      (update-document-tab workbench document)
      (select-page (workbench-tabs workbench) page)
      (when scratch (close-document workbench scratch t)))
    (refresh-environment-choices workbench)
    (handler-case (api-load-builder page)
      (error (condition)
        (api-set-mode page t)
        (api-message page (princ-to-string condition))))
    (api-display-response page)
    (setf (divider-position page) 380)
    page))

(defun restyle-api-pages (workbench)
  "The theme or editor settings changed: response views follow."
  (let ((themes (workbench-themes workbench))
        (palette (workbench-palette workbench)))
    (dolist (page (api-pages workbench))
      (let ((view (api-response-view page)))
        (when view
          (setf (text-styles view) (syntax-text-styles palette)
                (font view) (editor-font themes))))
      (setf (font (part page :body)) (editor-font themes)
            (font (part page :output)) (editor-font themes)))))
