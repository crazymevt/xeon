(in-package #:xeon.core)

(define-condition buffer-file-error (buffer-error)
  ((pathname :initarg :pathname :reader buffer-file-error-pathname)
   (operation :initarg :operation :reader buffer-file-error-operation))
  (:report (lambda (condition stream)
             (format stream "Cannot ~A buffer file ~S."
                     (buffer-file-error-operation condition) (buffer-file-error-pathname condition)))))
(define-condition file-conflict (buffer-file-error)
  ((status :initarg :status :reader file-conflict-status))
  (:report (lambda (condition stream)
             (format stream "Refusing to ~A ~S: disk status is ~S."
                     (buffer-file-error-operation condition) (buffer-file-error-pathname condition)
                     (file-conflict-status condition)))))
(define-condition unsaved-buffer-changes (buffer-file-error) ()
  (:report (lambda (condition stream)
             (format stream "Reload would discard unsaved edits in ~S."
                     (buffer-file-error-pathname condition)))))

(defstruct (disk-state (:constructor %make-disk-state (pathname bytes))) pathname bytes)

(defun utf8-encode (text &key (start 0) (end (length text)) into)
  (let ((bytes (or into (make-array 0 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0))))
    (flet ((emit (byte) (vector-push-extend byte bytes)))
      (loop for index from start below end for code = (char-code (char text index)) do
        (cond
          ((< code #x80) (emit code))
          ((< code #x800)
           (emit (logior #xc0 (ash code -6)))
           (emit (logior #x80 (logand code #x3f))))
          ((or (<= #xd800 code #xdfff) (> code #x10ffff))
           (error "Character U+~X cannot be encoded as UTF-8." code))
          ((< code #x10000)
           (emit (logior #xe0 (ash code -12)))
           (emit (logior #x80 (logand (ash code -6) #x3f)))
           (emit (logior #x80 (logand code #x3f))))
          (t
           (emit (logior #xf0 (ash code -18)))
           (emit (logior #x80 (logand (ash code -12) #x3f)))
           (emit (logior #x80 (logand (ash code -6) #x3f)))
           (emit (logior #x80 (logand code #x3f)))))))
    bytes))

(defun utf8-encode-root (root)
  (let ((bytes (make-array (text-size root) :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
    (map-text-chunks (lambda (string start end)
                       (utf8-encode string :start start :end end :into bytes)) root)
    bytes))

(defun map-utf8-characters (function bytes)
  (progn
    (loop with i = 0 while (< i (length bytes)) do
      (let* ((first (aref bytes i))
             (count (cond ((< first #x80) 1) ((<= #xc2 first #xdf) 2)
                          ((<= #xe0 first #xef) 3) ((<= #xf0 first #xf4) 4)
                          (t (error "Invalid UTF-8 at byte ~D." i))))
             (code (logand first (case count (1 #x7f) (2 #x1f) (3 #x0f) (4 #x07)))))
        (when (> (+ i count) (length bytes)) (error "Truncated UTF-8 at byte ~D." i))
        (loop for j from 1 below count for next = (aref bytes (+ i j)) do
          (unless (<= #x80 next #xbf) (error "Invalid UTF-8 at byte ~D." (+ i j)))
          (setf code (logior (ash code 6) (logand next #x3f))))
        (unless (and (>= code (case count (1 0) (2 #x80) (3 #x800) (4 #x10000)))
                     (<= code #x10ffff) (not (<= #xd800 code #xdfff)) (code-char code))
          (error "Invalid UTF-8 code point at byte ~D." i))
        (funcall function (code-char code))
        (incf i count)))))

(defun utf8-decode (bytes)
  (with-output-to-string (out)
    (map-utf8-characters (lambda (char) (write-char char out)) bytes)))

(defun utf8-decode-root (bytes)
  ;; Keep ordinary ASCII files in compact base-character chunks. Upgrade only a
  ;; chunk containing characters outside BASE-CHAR, without a file-sized string.
  (let ((root nil) (chunk (make-string 4096 :element-type 'base-char)) (used 0))
    (map-utf8-characters
     (lambda (char)
       (unless (typep char (array-element-type chunk))
         (let ((wide (make-string 4096 :element-type 'character)))
           (replace wide chunk :end1 used :end2 used)
           (setf chunk wide)))
       (setf (char chunk used) char)
       (incf used)
       (when (= used 4096)
         (setf root (text-join root (text-leaf chunk 0 used))
               chunk (make-string 4096 :element-type 'base-char) used 0))) bytes)
    (when (plusp used) (setf root (text-join root (text-leaf chunk 0 used))))
    root))

(defun canonical-file-parent (directory)
  "Resolve the nearest existing ancestor, retaining missing directory names."
  (or (probe-file directory)
      (let ((parts (pathname-directory directory)))
        (if (> (length parts) 1)
            (merge-pathnames (make-pathname :directory (list :relative (car (last parts))))
                             (canonical-file-parent (make-pathname :directory (butlast parts) :defaults directory)))
            directory))))

(defun file-path (path)
  "Resolve existing symlinks, retaining a lexical path when parents are absent."
  (let ((absolute (merge-pathnames (pathname path))))
    (when (or (wild-pathname-p absolute) (uiop:directory-pathname-p absolute)
              (uiop:directory-exists-p absolute))
      (error "Expected a non-wild file pathname, got ~S." path))
    (or (probe-file absolute)
        (let ((parent (canonical-file-parent (uiop:pathname-directory-pathname absolute))))
          (make-pathname :directory (pathname-directory parent) :host (pathname-host parent)
                         :device (pathname-device parent) :defaults absolute)))))

(defun read-file-bytes (path)
  ;; OPEN with NIL distinguishes absence from permission/IO errors. Do not treat
  ;; all FILE-ERROR conditions as deletion.
  (let ((stream (open path :direction :input :element-type '(unsigned-byte 8)
                           :if-does-not-exist nil)))
    (if (null stream) :missing
        (unwind-protect
             (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0))
                   (chunk (make-array 8192 :element-type '(unsigned-byte 8))))
               (loop for count = (read-sequence chunk stream) while (plusp count)
                     do (loop for i below count do (vector-push-extend (aref chunk i) bytes)))
               bytes)
          (close stream)))))

(defun same-file-bytes-p (left right)
  (if (or (eq left :missing) (eq right :missing)) (eq left right) (equalp left right)))

(defun association-matches-p (buffer path)
  (and (%disk-state buffer)
       (equal path (disk-state-pathname (%disk-state buffer)))))

(defun disk-status (buffer path bytes)
  (cond ((not (association-matches-p buffer path)) :untracked)
        ((same-file-bytes-p bytes (disk-state-bytes (%disk-state buffer))) :unchanged)
        ((eq bytes :missing) :deleted)
        ((eq (disk-state-bytes (%disk-state buffer)) :missing) :created)
        (t :modified)))

(defgeneric open-buffer-file (pathname &key if-does-not-exist))
(defmethod open-buffer-file (pathname &key (if-does-not-exist :error))
  "Open UTF-8 text as a new buffer; :CREATE allows a missing file without writing it."
  (unless (member if-does-not-exist '(:error :create))
    (error "IF-DOES-NOT-EXIST must be :ERROR or :CREATE."))
  (let* ((path (file-path pathname)) (bytes (read-file-bytes path)))
    (when (and (eq bytes :missing) (eq if-does-not-exist :error))
      (error 'buffer-file-error :buffer nil :pathname path :operation :open))
    (let ((buffer (make-buffer :name (file-namestring path) :pathname path))
          (root (unless (eq bytes :missing) (utf8-decode-root bytes))))
      (setf (%root buffer) root (%saved-root buffer) root
            (%disk-state buffer) (%make-disk-state path bytes))
      buffer)))

(defgeneric check-buffer-file (buffer))
(defmethod check-buffer-file ((buffer buffer))
  "Read-only content comparison; no timestamp shortcut and no buffer mutation."
  (if (null (buffer-pathname buffer)) :unassociated
      (let ((path (file-path (buffer-pathname buffer))))
        (disk-status buffer path (read-file-bytes path)))))

(defun write-file-atomically (buffer path bytes expected)
  (let ((temporary nil) (stream nil)
        (mode (unless (eq expected :missing) (existing-file-mode path))))
    (unwind-protect
         (progn
           (loop repeat 100
                 for candidate = (merge-pathnames
                                  (format nil ".xeon-~36R-~36R.tmp"
                                          (get-universal-time) (random (expt 2 60)))
                                  (uiop:pathname-directory-pathname path))
                 do (setf stream (open candidate :direction :output
                                                 :element-type '(unsigned-byte 8)
                                                 :if-exists nil :if-does-not-exist :create))
                 when stream do (setf temporary candidate) (return)
                 finally (error "Unable to allocate a temporary file beside ~S." path))
           ;; Start private, then preserve ordinary permissions of an existing file.
           (set-file-mode temporary #o600)
           (write-sequence bytes stream)
           (finish-output stream)
           (close stream)
           (setf stream nil)
           (when mode (set-file-mode temporary mode))
           (unless (same-file-bytes-p expected (read-file-bytes path))
             (error 'file-conflict :buffer buffer :pathname path :operation :save
                                   :status :changed-during-save))
           (replace-file-on-disk temporary path (not (eq expected :missing)))
           (setf temporary nil))
      (when stream (close stream :abort t))
      (when (and temporary (probe-file temporary)) (delete-file temporary)))))

(defun save-to-path (buffer path overwrite-p same-target-p trim-whitespace)
  (check-idle buffer)
  (check-writable buffer)
  (validate-file-association (%manager buffer) buffer path)
  (let* ((observed (read-file-bytes path))
         (status (disk-status buffer path observed)))
    (unless (or overwrite-p
                (if same-target-p (eq status :unchanged) (eq observed :missing)))
      (error 'file-conflict :buffer buffer :pathname path :operation :save
                            :status (if same-target-p status :target-exists)))
    (when trim-whitespace (trim-trailing-whitespace buffer))
    (let* ((root (%root buffer)) (bytes (utf8-encode-root root)))
      (write-file-atomically buffer path bytes observed)
      ;; Adopt destination only after the write/close/recheck/rename all succeed.
      (setf (buffer-pathname buffer) path
            (buffer-name buffer) (file-namestring path)
            (%saved-root buffer) root
            (%disk-state buffer) (%make-disk-state path bytes))
      buffer)))

(defgeneric save-buffer (buffer &key overwrite-external trim-whitespace))
(defmethod save-buffer ((buffer buffer) &key overwrite-external trim-whitespace)
  (check-idle buffer)
  (unless (buffer-pathname buffer)
    (error 'buffer-file-error :buffer buffer :pathname nil :operation :save))
  (save-to-path buffer (file-path (buffer-pathname buffer)) overwrite-external t trim-whitespace))

(defgeneric save-buffer-as (buffer pathname &key if-exists trim-whitespace))
(defmethod save-buffer-as ((buffer buffer) pathname &key (if-exists :error) trim-whitespace)
  (check-idle buffer)
  (unless (member if-exists '(:error :overwrite))
    (error "IF-EXISTS must be :ERROR or :OVERWRITE."))
  (let ((path (file-path pathname)))
    (save-to-path buffer path (eq if-exists :overwrite) (association-matches-p buffer path) trim-whitespace)))

(defgeneric reload-buffer (buffer &key discard-changes))
(defmethod reload-buffer ((buffer buffer) &key discard-changes)
  (check-idle buffer)
  (unless (buffer-pathname buffer)
    (error 'buffer-file-error :buffer buffer :pathname nil :operation :reload))
  (when (and (buffer-modified-p buffer) (not discard-changes))
    (error 'unsaved-buffer-changes :buffer buffer :pathname (buffer-pathname buffer)
                                  :operation :reload))
  (let* ((path (file-path (buffer-pathname buffer))) (bytes (read-file-bytes path)))
    (when (eq bytes :missing)
      (error 'buffer-file-error :buffer buffer :pathname path :operation :reload))
    (validate-file-association (%manager buffer) buffer path)
    (let* ((root (utf8-decode-root bytes)) (before (snapshot buffer))
           (changed (not (text-equal-p root (%root buffer)))))
      ;; Commit all state before notifying. Reload is allowed for read-only views
      ;; and remains undoable (undo itself still respects user read-only policy).
      (setf (%root buffer) root
            (%point buffer) (min (%point buffer) (text-size root))
            (%anchor buffer) (when (%anchor buffer) (min (%anchor buffer) (text-size root)))
            (%saved-root buffer) root
            (buffer-pathname buffer) path
            (%disk-state buffer) (%make-disk-state path bytes))
      (when changed
        (record-history buffer before)
        (notify-change buffer :reload before)))
    buffer))

(defgeneric refresh-buffer-file (buffer))
(defmethod refresh-buffer-file ((buffer buffer))
  "Poll disk: reload clean changed buffers, preserve dirty buffers and report conflict."
  (check-idle buffer)
  (let ((status (check-buffer-file buffer)))
    (case status
      ((:modified :created)
       (if (buffer-modified-p buffer) :conflict
           (progn (reload-buffer buffer) :reloaded)))
      (otherwise status))))
