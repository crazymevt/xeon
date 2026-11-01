(in-package #:xeon.folders)

(defun entry-leaf (path)
  "Name of an entry, without resolving the final symbolic link."
  (if (uiop:directory-pathname-p path) (car (last (pathname-directory path))) (uiop:native-namestring (make-pathname :directory nil :host nil :device nil :defaults path))))

(defun entry-parent (path)
  (if (uiop:directory-pathname-p path)
      (make-pathname :directory (butlast (pathname-directory path)) :name nil :type nil :defaults path)
      (uiop:pathname-directory-pathname path)))

(defun checked-parent (project path)
  (when (wild-pathname-p path) (error "Wildcard paths are unsupported."))
  (let ((parent (truename (entry-parent path))))
    (unless (path-within-root-p parent (project-root project)) (error "Entry is outside the opened folder."))
    (when (or (folder-path-equal-p path (project-root project))
              (and (not (symbolic-entry-p path))
                   (folder-path-equal-p (truename path) (project-root project)))) (error "The opened root cannot be renamed or deleted."))
    parent))

(defun checked-entry-name (name)
  (unless (and (stringp name) (<= 1 (length name) 240)
               (not (member name '("." "..") :test #'string=))
               (not (find (char name (1- (length name))) '(#\Space #\.)))
               (notany (lambda (c) (or (< (char-code c) 32) (find c "<>:\"/\\|?*"))) name)
               (not (member (string-upcase (first (uiop:split-string name :separator ".")))
                            '("CON" "PRN" "AUX" "NUL" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8" "COM9"
                              "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9") :test #'string=)))
    (error "Enter a valid file or folder name (one name, without path separators)."))
  name)

(defun raw-entry-path (path)
  (merge-pathnames (uiop:parse-native-namestring (entry-leaf path)) (entry-parent path)))

#+(and sbcl darwin)
(sb-alien:define-alien-routine ("renamex_np" %rename-exclusive) sb-alien:int
  (old sb-alien:c-string) (new sb-alien:c-string) (flags sb-alien:unsigned-int))
#+(and sbcl linux)
(sb-alien:define-alien-routine ("renameat2" %rename-exclusive-linux) sb-alien:int
  (old-fd sb-alien:int) (old sb-alien:c-string) (new-fd sb-alien:int) (new sb-alien:c-string) (flags sb-alien:unsigned-int))

(defun rename-entry-exclusive (old new)
  ;; No delete-then-rename fallback: a racing destination must never be overwritten.
  #+(and sbcl darwin)
  (unless (zerop (%rename-exclusive (uiop:native-namestring old) (uiop:native-namestring new) 4))
    (error "Cannot rename ~A to ~A (destination exists or filesystem denied the operation)." old new))
  #+(and sbcl linux)
  (unless (zerop (%rename-exclusive-linux -100 (uiop:native-namestring old) -100 (uiop:native-namestring new) 1))
    (error "Cannot rename ~A to ~A." old new))
  #+(and sbcl win32)
  (let ((source (xeon.core::windows-path-units old)) (destination (xeon.core::windows-path-units new)))
    (sb-sys:with-pinned-objects (source destination)
      (when (zerop (xeon.core::%move-file-ex-wide
                     (sb-alien:sap-alien (sb-sys:vector-sap source) (* sb-alien:unsigned-short))
                     (sb-alien:sap-alien (sb-sys:vector-sap destination) (* sb-alien:unsigned-short)) 8))
        (error "Cannot rename ~A to ~A." old new))))
  #-(and sbcl (or darwin linux win32))
  (error "Exclusive rename is not implemented for this platform.")
  new)

#+(and sbcl win32)
(sb-alien:define-alien-routine ("CreateDirectoryW" %create-directory-wide) sb-alien:int
  (path (* sb-alien:unsigned-short)) (security sb-alien:system-area-pointer))

(defun create-folder-entry (project directory name &key directory-p)
  (checked-entry-name name)
  (when (symbolic-entry-p directory) (error "Cannot create entries inside a symbolic folder."))
  (let* ((parent (truename (uiop:ensure-directory-pathname directory)))
         (path (merge-pathnames (uiop:parse-native-namestring name) parent)))
    (unless (path-within-root-p parent (project-root project)) (error "Folder is outside the opened root."))
    (if directory-p
        (progn
          ;; ENSURE-DIRECTORIES-EXIST would silently accept an existing directory.
          #+(and sbcl (not win32)) (sb-posix:mkdir (uiop:native-namestring path) #o777)
          #+(and sbcl win32)
          (let ((units (xeon.core::windows-path-units path)))
            (sb-sys:with-pinned-objects (units)
              (when (zerop (%create-directory-wide (sb-alien:sap-alien (sb-sys:vector-sap units) (* sb-alien:unsigned-short)) (sb-sys:int-sap 0)))
                (error "Cannot create directory ~A." path))))
          #-sbcl (error "Exclusive directory creation requires SBCL.")
          (uiop:ensure-directory-pathname path))
        (progn (with-open-file (stream path :direction :output :if-exists :error :if-does-not-exist :create)
                 (declare (ignorable stream))) path))))

(defun relocate-folder-entry (path destination manager)
  (let* ((directory-p (uiop:directory-pathname-p path))
         (new-path (if directory-p (uiop:ensure-directory-pathname destination) destination))
         ;; A symbolic entry is renamed as a link. Its target documents do not move.
         (old-path (unless (symbolic-entry-p path) (truename path)))
         (associations
           (when (and manager old-path)
             (loop for buffer in (xeon.core:manager-buffers manager)
                   for file = (xeon.core:buffer-pathname buffer)
                   when (and file (if directory-p (path-within-root-p file old-path)
                                      (folder-path-equal-p file old-path)))
                     collect (cons buffer (if directory-p
                                              (merge-pathnames (uiop:enough-pathname file old-path) new-path)
                                              new-path))))))
    (flet ((move-entry () (rename-entry-exclusive (raw-entry-path path) destination) new-path))
      (if manager (xeon.core:call-with-file-retargets manager associations #'move-entry) (move-entry)))))


(defun rename-folder-entry (project path name &key manager)
  (let* ((parent (checked-parent project path))
         (destination (merge-pathnames (uiop:parse-native-namestring (checked-entry-name name)) parent)))
    (relocate-folder-entry path destination manager)))

(defun move-folder-entry (project path directory &key manager)
  "Move an entry into an existing project folder without overwriting anything."
  (let* ((parent (checked-parent project path))
         (requested (uiop:ensure-directory-pathname directory)))
    (when (or (wild-pathname-p requested) (symbolic-entry-p requested))
      (error "Drop destination must be an ordinary folder."))
    (let* ((target (truename requested))
           (source (unless (symbolic-entry-p path) (truename path)))
           (destination (merge-pathnames (uiop:parse-native-namestring (entry-leaf path)) target)))
      (unless (and (uiop:directory-exists-p target) (path-within-root-p target (project-root project)))
        (error "Drop destination is outside the opened folder."))
      (when (and source (uiop:directory-pathname-p path) (path-within-root-p target source))
        (error "A folder cannot be moved into itself or one of its descendants."))
      (if (folder-path-equal-p parent target) path
          (relocate-folder-entry path destination manager)))))

(defun %delete-folder-entry (project path)
  "Permanently delete an entry. Directory links are leaves, never traversed."
  (checked-parent project path)
  (cond ((symbolic-entry-p path)
         #+(and sbcl (not win32)) (sb-posix:unlink (uiop:native-namestring (raw-entry-path path)))
         #+win32 (if (uiop:directory-pathname-p path) (uiop:delete-empty-directory path) (delete-file path))
         #-(or sbcl win32) (delete-file path))
        ((uiop:directory-pathname-p path)
         (dolist (child (directory-entries path)) (%delete-folder-entry project child))
         (uiop:delete-empty-directory path))
        (t (delete-file path)))
  t)

(defun entry-open-buffers (project path manager)
  "Open documents whose filesystem entries will be removed. Link targets stay open."
  (checked-parent project path)
  (unless (symbolic-entry-p path)
    (let ((canonical (truename path)) (directory-p (uiop:directory-pathname-p path)))
      (remove-if-not
       (lambda (buffer)
         (let ((file (xeon.core:buffer-pathname buffer)))
           (and file (if directory-p (path-within-root-p file canonical) (folder-path-equal-p file canonical)))))
       (xeon.core:manager-buffers manager)))))

(defun delete-folder-entry (project path &key manager)
  "Delete an entry, then unregister affected documents. Caller obtains discard consent.
On filesystem failure all documents remain open, including on partial deletion."
  (if manager
      (xeon.core:call-with-buffer-removals manager (entry-open-buffers project path manager)
                                                (lambda () (%delete-folder-entry project path)))
      (%delete-folder-entry project path)))

(define-condition folder-batch-error (error)
  ((completed :initarg :completed :reader batch-completed)
   (cause :initarg :cause :reader batch-cause))
  (:report (lambda (condition stream)
             (format stream "Operation stopped after ~D completed entries: ~A"
                     (length (batch-completed condition)) (batch-cause condition)))))

(defun normalize-folder-entries (project paths)
  "Canonical entry parents; remove duplicates and children of selected folders."
  (let ((count (ignore-errors (list-length paths))))
    (unless (and count (<= 1 count 4096)) (error "Select1..4096 entries.")))
  (let ((unique nil))
    (dolist (path paths)
      (let* ((parent (checked-parent project path))
             (canonical (merge-pathnames (uiop:enough-pathname path (entry-parent path)) parent)))
        (unless (member canonical unique :test #'folder-path-equal-p) (push canonical unique))))
    (setf unique (nreverse unique))
    (let ((directories (remove-if-not (lambda (path) (and (uiop:directory-pathname-p path)
                                                         (not (symbolic-entry-p path)))) unique)))
      (remove-if (lambda (path)
                   (some (lambda (ancestor) (and (not (folder-path-equal-p path ancestor))
                                                (path-within-root-p path ancestor))) directories))
                 unique))))

(defun move-folder-entries (project paths directory &key manager)
  "Preflight the entire selection; then move entries, keeping tabs accurate per move.
On a later IO failure BATCH-COMPLETED records successful (OLD . NEW) moves."
  (let* ((sources (normalize-folder-entries project paths))
         (requested (uiop:ensure-directory-pathname directory))
         (target (progn
                   (when (or (wild-pathname-p requested) (symbolic-entry-p requested))
                     (error "Drop destination must be an ordinary folder."))
                   (truename requested)))
         (destinations nil) (plan nil) (completed nil))
    (unless (and (uiop:directory-exists-p target) (path-within-root-p target (project-root project)))
      (error "Drop destination is outside the opened folder."))
    (dolist (source sources)
      (when (and (uiop:directory-pathname-p source) (not (symbolic-entry-p source))
                 (path-within-root-p target (truename source)))
        (error "A folder cannot be moved into itself or its descendants."))
      (let* ((destination (merge-pathnames (uiop:parse-native-namestring (entry-leaf source)) target))
             (same-parent (folder-path-equal-p (entry-parent source) target)))
        (when (member destination destinations :test #'folder-path-equal-p)
          (error "Selected entries would have the same destination name: ~A" destination))
        (push destination destinations)
        (unless same-parent
          (when (or (probe-file destination) (ignore-errors (symbolic-entry-p destination)))
            (error "Destination already exists: ~A" destination))
          (push (cons source (if (uiop:directory-pathname-p source) (uiop:ensure-directory-pathname destination) destination)) plan))))
    ;; No editor state or disk writes occur during preflight. This also catches
    ;; open documents visiting a destination that does not yet exist on disk.
    (when manager
      (dolist (pair plan)
        (dolist (buffer (entry-open-buffers project (car pair) manager))
          (xeon.core::check-idle buffer)
          (xeon.core::validate-file-association
           manager buffer (if (uiop:directory-pathname-p (car pair))
                              (merge-pathnames (uiop:enough-pathname (xeon.core:buffer-pathname buffer) (car pair)) (cdr pair))
                              (cdr pair))))))
    (handler-case
        (progn (dolist (pair (nreverse plan))
                 (relocate-folder-entry (car pair) (raw-entry-path (cdr pair)) manager)
                 (push pair completed))
               (nreverse completed))
      (error (condition) (error 'folder-batch-error :completed (nreverse completed) :cause condition)))))

(defun delete-folder-entries (project paths &key manager)
  "Delete selected top-level entries, closing documents only for completed roots."
  (let ((sources (normalize-folder-entries project paths)) (completed nil))
    (when manager
      (dolist (path sources)
        (dolist (buffer (entry-open-buffers project path manager)) (xeon.core::check-idle buffer))))
    (handler-case
        (progn (dolist (path sources) (delete-folder-entry project path :manager manager) (push path completed))
               (nreverse completed))
      (error (condition) (error 'folder-batch-error :completed (nreverse completed) :cause condition)))))

(defun copy-file-entry-p (path)
  #+(and sbcl (not win32))
  (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring path))))
  #-(and sbcl (not win32)) (and (probe-file path) (not (uiop:directory-pathname-p path))))

(defun copy-entry-snapshot (project path &key cancelled-p progress)
  "Snapshot regular files/directories before creating anything. Never follow links."
  (let ((entries nil) (count 0))
    (labels ((visit (entry)
               (when (and cancelled-p (funcall cancelled-p)) (error "Copy cancelled."))
               (when progress (funcall progress 0 nil entry))
               (checked-parent project entry)
               (when (> (incf count) 100000) (error "A copy is limited to100000 entries."))
               (when (symbolic-entry-p entry) (error "Copying symbolic links is not supported: ~A" entry))
               (unless (or (uiop:directory-pathname-p entry) (copy-file-entry-p entry))
                 (error "Cannot copy a special filesystem entry: ~A" entry))
               (push entry entries)
               (when (uiop:directory-pathname-p entry) (dolist (child (directory-entries entry)) (visit child)))))
      (visit path))
    (nreverse entries)))

(defun unused-copy-destination (source directory reserved)
  (let* ((directory-p (uiop:directory-pathname-p source))
         (leaf (entry-leaf source)) (parsed (uiop:parse-native-namestring leaf))
         (stem (if directory-p leaf (pathname-name parsed)))
         (extension (unless directory-p (pathname-type parsed))))
    (loop for index from 0 below 10000
          for name = (if (zerop index) leaf
                         (format nil "~A copy~A~A" stem (if (= index 1) "" (format nil " ~D" index))
                                 (if extension (format nil ".~A" extension) "")))
          for candidate = (merge-pathnames (uiop:parse-native-namestring (checked-entry-name name)) directory)
          unless (or (member candidate reserved :test #'folder-path-equal-p) (probe-file candidate)
                     (ignore-errors (symbolic-entry-p candidate)))
            return (if directory-p (uiop:ensure-directory-pathname candidate) candidate)
          finally (error "Cannot find an unused copy name for ~A." source))))

(defun copy-one-entry (project source destination snapshot &key cancelled-p progress)
  (let ((created nil) (complete nil))
    (unwind-protect
         (progn
           (dolist (entry snapshot)
             (when (and cancelled-p (funcall cancelled-p)) (error "Copy cancelled."))
             ;; Recheck source type and containment after preflight as directory
             ;; contents can change while copying. Destination opens are exclusive.
             (checked-parent project entry)
             (when (symbolic-entry-p entry) (error "Source changed to a symbolic link: ~A" entry))
             (let ((output (if (folder-path-equal-p entry source) destination
                               (merge-pathnames (uiop:enough-pathname entry source) destination))))
               (if (uiop:directory-pathname-p entry)
                   (progn (create-folder-entry project (entry-parent output) (entry-leaf output) :directory-p t)
                          (when (folder-path-equal-p entry source) (setf created output)))
                   (progn
                     (unless (copy-file-entry-p entry) (error "Source is no longer a regular file: ~A" entry))
                     (unless (path-within-root-p (truename (entry-parent output)) (project-root project))
                       (error "Copy destination escaped the opened folder."))
                     (with-open-file (input entry :direction :input :element-type '(unsigned-byte 8))
                       (with-open-file (out output :direction :output :element-type '(unsigned-byte 8)
                                                   :if-exists :error :if-does-not-exist :create)
                         (when (folder-path-equal-p entry source) (setf created output))
                         (let ((chunk (make-array 65536 :element-type '(unsigned-byte 8))))
                           (loop
                             (when (and cancelled-p (funcall cancelled-p)) (error "Copy cancelled."))
                             (let ((size (read-sequence chunk input)))
                               (when (zerop size) (return))
                               (write-sequence chunk out :end size)
                               (when progress (funcall progress size entry)))))))))))
           (setf complete t)
           destination)
      (when (and created (not complete) (or (probe-file created) (ignore-errors (symbolic-entry-p created))))
        ;; Only remove a destination successfully created by this operation;
        ;; an existing/racing destination is never considered ours to clean up.
        (%delete-folder-entry project created)))))

(defun copy-folder-entries (project paths directory &key progress cancelled-p)
  "Duplicate on-disk entries with collision-free names. Never retarget source buffers."
  (let* ((sources (normalize-folder-entries project paths))
         (requested (uiop:ensure-directory-pathname directory))
         (target (progn
                   (when (or (wild-pathname-p requested) (symbolic-entry-p requested))
                     (error "Paste destination must be an ordinary folder."))
                   (truename requested)))
         (reserved nil) (plan nil) (completed nil) (total 0) (copied 0) (entry-count 0))
    (unless (and (uiop:directory-exists-p target) (path-within-root-p target (project-root project)))
      (error "Paste destination is outside the opened folder."))
    (dolist (source sources)
      (when (and (uiop:directory-pathname-p source) (not (symbolic-entry-p source))
                 (path-within-root-p target (truename source)))
        (error "A folder cannot be copied into itself or its descendants."))
      (let ((destination (unused-copy-destination source target reserved)))
        (push (raw-entry-path destination) reserved)
        (let ((snapshot (copy-entry-snapshot project source :cancelled-p cancelled-p :progress progress)))
          (when (> (incf entry-count (length snapshot)) 100000) (error "A copy batch is limited to100000 entries."))
          (dolist (entry snapshot)
            (when (and cancelled-p (funcall cancelled-p)) (error "Copy cancelled."))
            (unless (uiop:directory-pathname-p entry)
              (with-open-file (input entry :element-type '(unsigned-byte 8)) (incf total (file-length input)))))
          (push (list source destination snapshot) plan))))
    (when progress (funcall progress 0 total nil))
    (handler-case
        (progn (dolist (entry (nreverse plan))
                 (copy-one-entry project (first entry) (second entry) (third entry)
                                 :cancelled-p cancelled-p
                                 :progress (when progress (lambda (bytes path) (incf copied bytes) (funcall progress copied total path))))
                 (push (cons (first entry) (second entry)) completed))
               (nreverse completed))
      (error (condition) (error 'folder-batch-error :completed (nreverse completed) :cause condition)))))
