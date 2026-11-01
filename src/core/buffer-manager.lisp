(in-package #:xeon.core)

(define-condition manager-error (error)
  ((manager :initarg :manager :reader manager-error-manager)
   (buffer :initarg :buffer :initform nil :reader manager-error-buffer)))
(define-condition manager-busy (manager-error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Buffer manager is busy." s))))
(define-condition buffer-not-managed (manager-error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Buffer is not in this manager." s))))
(define-condition buffer-already-managed (manager-error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Buffer belongs to another manager." s))))
(define-condition buffer-file-already-open (manager-error)
  ((existing :initarg :existing :reader already-open-buffer))
  (:report (lambda (c s) (declare (ignore c)) (write-string "Another buffer already visits that file." s))))
(define-condition unsaved-buffer-close (manager-error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Closing would discard unsaved buffer contents." s))))

(defstruct (manager-change (:constructor %manager-change (kind buffer id previous-current current)))
  (kind nil :read-only t) (buffer nil :read-only t) (id nil :read-only t)
  (previous-current nil :read-only t) (current nil :read-only t))
(defstruct (buffer-save-result (:constructor %save-result (buffer id status condition)))
  (buffer nil :read-only t) (id nil :read-only t)
  (status nil :read-only t) (condition nil :read-only t))

(defclass buffer-manager ()
  ((entries :initform nil :accessor %entries)
   (next-id :initform 0 :accessor %next-id)
   (current :initform nil :reader current-buffer)
   (mru :initform nil :accessor %mru)
   (busy :initform nil :accessor %manager-busy)
   (listeners :initform nil :accessor %manager-listeners)))
(defun make-buffer-manager () (make-instance 'buffer-manager))

(defgeneric manager-buffers (manager))
(defmethod manager-buffers ((manager buffer-manager)) (mapcar #'cdr (%entries manager)))
(defgeneric manager-buffer-id (manager buffer))
(defmethod manager-buffer-id ((manager buffer-manager) buffer)
  (car (rassoc buffer (%entries manager) :test #'eq)))
(defgeneric find-managed-buffer (manager designator))
(defmethod find-managed-buffer ((manager buffer-manager) designator)
  (cond ((integerp designator) (cdr (assoc designator (%entries manager))))
        ((typep designator 'buffer) (and (manager-buffer-id manager designator) designator))
        (t nil)))
(defun require-managed (manager designator)
  (or (find-managed-buffer manager designator)
      (error 'buffer-not-managed :manager manager :buffer designator)))

(defun call-with-manager-operation (manager function)
  (when (%manager-busy manager) (error 'manager-busy :manager manager))
  (unwind-protect (progn (setf (%manager-busy manager) t) (funcall function))
    (setf (%manager-busy manager) nil)))

(defgeneric add-manager-listener (manager function))
(defmethod add-manager-listener ((manager buffer-manager) function)
  (check-type function function)
  (pushnew function (%manager-listeners manager) :test #'eq) function)
(defgeneric remove-manager-listener (manager function))
(defmethod remove-manager-listener ((manager buffer-manager) function)
  (setf (%manager-listeners manager) (remove function (%manager-listeners manager) :test #'eq)) function)
(defun notify-manager (manager kind buffer id previous)
  (dolist (function (copy-list (%manager-listeners manager)))
    (funcall function manager (%manager-change kind buffer id previous (current-buffer manager)))))

(defun file-buffer-in-manager (manager path &optional except)
  (when path
    (find-if (lambda (buffer)
               (and (not (eq buffer except)) (buffer-pathname buffer)
                    (funcall #+win32 #'equalp #-win32 #'equal path (file-path (buffer-pathname buffer)))))
             (manager-buffers manager))))
(defmethod validate-file-association ((manager buffer-manager) buffer pathname)
  (when pathname
    (let ((existing (file-buffer-in-manager manager (file-path pathname) buffer)))
      (when existing
        (error 'buffer-file-already-open :manager manager :buffer buffer :existing existing)))))

(defun select-managed (manager buffer)
  (setf (slot-value manager 'current) buffer
        (%mru manager) (cons buffer (remove buffer (%mru manager) :test #'eq))))
(defun register-buffer (manager buffer select)
  (check-idle buffer)
  (when (and (%manager buffer) (not (eq (%manager buffer) manager)))
    (error 'buffer-already-managed :manager manager :buffer buffer))
  (let ((id (manager-buffer-id manager buffer)) (previous (current-buffer manager)))
    (if id
        (when (and select (not (eq previous buffer)))
          (select-managed manager buffer) (notify-manager manager :switched buffer id previous))
        (progn
          (validate-file-association manager buffer (buffer-pathname buffer))
          (setf id (incf (%next-id manager))
                (%entries manager) (append (%entries manager) (list (cons id buffer)))
                (%manager buffer) manager)
          (when (or select (null previous)) (select-managed manager buffer))
          (notify-manager manager :added buffer id previous)))
    (values buffer id)))

(defgeneric add-buffer (manager buffer &key select))
(defmethod add-buffer ((manager buffer-manager) (buffer buffer) &key (select t))
  (call-with-manager-operation manager (lambda () (register-buffer manager buffer select))))
(defgeneric new-buffer (manager &key name text select history-limit))
(defmethod new-buffer ((manager buffer-manager) &key (name "untitled") (text "")
                                                    (select t) (history-limit 1000))
  (call-with-manager-operation manager
    (lambda () (register-buffer manager (make-buffer :name name :text text :history-limit history-limit)
                                select))))

(defgeneric open-managed-file (manager pathname &key select if-does-not-exist))
(defmethod open-managed-file ((manager buffer-manager) pathname
                             &key (select t) (if-does-not-exist :error))
  (unless (member if-does-not-exist '(:error :create)) (error "Invalid missing-file policy."))
  (call-with-manager-operation manager
    (lambda ()
      (let* ((path (file-path pathname)) (existing (file-buffer-in-manager manager path))
             (buffer (or existing (open-buffer-file path :if-does-not-exist if-does-not-exist))))
        (register-buffer manager buffer select)
        (values buffer (null existing))))))

(defgeneric switch-buffer (manager designator))
(defmethod switch-buffer ((manager buffer-manager) designator)
  (call-with-manager-operation manager
    (lambda ()
      (let ((buffer (require-managed manager designator)) (previous (current-buffer manager)))
        (unless (eq previous buffer)
          (select-managed manager buffer)
          (notify-manager manager :switched buffer (manager-buffer-id manager buffer) previous))
        buffer))))

(defgeneric buffer-needs-saving-p (buffer))
(defun disposable-untitled-buffer-p (buffer)
  "True for an empty, unchanged untitled document, never a file or generated view."
  (and buffer
       (null (buffer-pathname buffer))
       (not (buffer-read-only-p buffer))
       (not (buffer-modified-p buffer))
       (zerop (buffer-length buffer))
       (let ((name (buffer-name buffer)))
         (or (string= name "untitled")
             (and (> (length name) 9)
                  (string= name "untitled-" :end1 9)
                  (every #'digit-char-p (subseq name 9)))))))

(defmethod buffer-needs-saving-p ((buffer buffer))
  (or (buffer-modified-p buffer)
      (and (%disk-state buffer) (eq :missing (disk-state-bytes (%disk-state buffer))))
      ;; Initial scratch text has never reached a file even if its edit baseline
      ;; was initialized to that same text. Read-only generated views (such as
      ;; Git patches) are disposable, but actual edits still need protection.
      (and (not (buffer-read-only-p buffer))
           (null (%disk-state buffer)) (plusp (buffer-length buffer)))))

(defgeneric save-all-buffers (manager &key trim-whitespace))
(defmethod save-all-buffers ((manager buffer-manager) &key trim-whitespace)
  (call-with-manager-operation manager
    (lambda ()
      (loop for (id . buffer) in (copy-list (%entries manager)) collect
        (handler-case
            (progn
              (check-idle buffer)
              (cond ((not (buffer-needs-saving-p buffer)) (%save-result buffer id :unchanged nil))
                    ((null (buffer-pathname buffer)) (%save-result buffer id :needs-pathname nil))
                    (t (save-buffer buffer :trim-whitespace trim-whitespace) (%save-result buffer id :saved nil))))
          (error (condition) (%save-result buffer id :failed condition)))))))

(defgeneric close-buffer (manager designator &key if-modified))
(defmethod close-buffer ((manager buffer-manager) designator &key (if-modified :error))
  (unless (member if-modified '(:error :save :discard :cancel)) (error "Invalid close policy."))
  (call-with-manager-operation manager
    (lambda ()
      (let* ((buffer (require-managed manager designator))
             (id (manager-buffer-id manager buffer)) (previous (current-buffer manager)))
        (check-idle buffer)
        (when (eq if-modified :cancel) (return-from close-buffer (values nil :cancelled)))
        (when (buffer-needs-saving-p buffer)
          (case if-modified
            (:error (error 'unsaved-buffer-close :manager manager :buffer buffer))
            (:save (save-buffer buffer)
                   (when (buffer-needs-saving-p buffer)
                     (error 'unsaved-buffer-close :manager manager :buffer buffer)))))
        (setf (%entries manager) (remove id (%entries manager) :key #'car)
              (%mru manager) (remove buffer (%mru manager) :test #'eq)
              (%manager buffer) nil)
        (when (eq previous buffer)
          (setf (slot-value manager 'current)
                (or (first (%mru manager)) (cdar (%entries manager)))))
        (notify-manager manager :closed buffer id previous)
        (values buffer :closed)))))

(defun call-with-file-retargets (manager associations operation)
  "Validate (BUFFER . NEW-PATH) associations, run OPERATION, then adopt paths.
No text, saved contents, revision, selection or undo state changes. OPERATION
must perform only the filesystem move and signal on failure. Notifications see
all updated paths, including when several documents move together."
  (call-with-manager-operation manager
    (lambda ()
      (let ((seen nil)
            (prepared
              (mapcar (lambda (association)
                        (let* ((buffer (require-managed manager (car association)))
                               (path (file-path (cdr association))))
                          (check-idle buffer)
                          (unless (buffer-pathname buffer) (error "Cannot retarget an unsaved document."))
                          (validate-file-association manager buffer path)
                          (list buffer path (file-namestring path)
                                (when (%disk-state buffer)
                                  (%make-disk-state path (disk-state-bytes (%disk-state buffer)))))))
                      associations)))
        (dolist (entry prepared)
          (when (or (member (first entry) seen :key #'first :test #'eq)
                    (member (second entry) seen :key #'second :test #+win32 #'equalp #-win32 #'equal))
            (error "Duplicate buffer or destination in file relocation."))
          (push entry seen))
        (let ((result (funcall operation)))
          (dolist (entry prepared)
            (destructuring-bind (buffer path name disk-state) entry
              ;; All checks and allocations precede the move. Commit metadata without
              ;; invoking setters that would inspect the newly changed filesystem.
              (setf (slot-value buffer 'pathname) path (slot-value buffer 'name) name (%disk-state buffer) disk-state)))
          (dolist (entry prepared)
            (notify-manager manager :file-renamed (first entry) (manager-buffer-id manager (first entry)) (current-buffer manager)))
          result)))))

(defun call-with-buffer-removals (manager buffers operation)
  "Run OPERATION before discarding and unregistering BUFFERS as one batch.
The caller must obtain consent for unsaved changes first. A failing operation
leaves every buffer registered. Notifications observe the completed batch."
  (call-with-manager-operation manager
    (lambda ()
      (let* ((buffers (remove-duplicates buffers :test #'eq))
             (previous (current-buffer manager))
             (ids (mapcar (lambda (buffer) (check-idle (require-managed manager buffer))
                            (cons buffer (manager-buffer-id manager buffer))) buffers))
             (entries (remove-if (lambda (entry) (member (cdr entry) buffers :test #'eq)) (%entries manager)))
             (mru (remove-if (lambda (buffer) (member buffer buffers :test #'eq)) (%mru manager))))
        (let ((result (funcall operation)))
          (setf (%entries manager) entries (%mru manager) mru)
          (dolist (buffer buffers) (setf (%manager buffer) nil))
          (when (member previous buffers :test #'eq)
            (setf (slot-value manager 'current) (or (first mru) (cdar entries))))
          (dolist (entry ids) (notify-manager manager :closed (car entry) (cdr entry) previous))
          result)))))
