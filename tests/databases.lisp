;;; Standalone headless SQLite backend regression; run with `make database-test`.
(in-package #:cl-user)

(defun database-test-path (name)
  (merge-pathnames (format nil "xeon-db-~A-~A" (symbol-name (gensym)) name) (uiop:temporary-directory)))

(defun await-database (predicate &optional (seconds 10))
  (loop repeat (* seconds 100) until (funcall predicate) do (sleep 0.01))
  (funcall predicate))

;;; Driver
(let* ((path (database-test-path "driver.sqlite"))
       (db nil))
  (unwind-protect
       (progn
         (setf db (xeon.databases:connect path :create t))
         (assert (probe-file path))
         (xeon.databases:query db "CREATE TABLE sample (id INTEGER PRIMARY KEY, name TEXT, n INTEGER)")
         (xeon.databases:query db "INSERT INTO sample VALUES (1, 'O''Brien', NULL)")
         (assert (equal '("sample") (xeon.databases:tables db)))
         (multiple-value-bind (names rows) (xeon.databases:query db "SELECT * FROM sample")
           (assert (equal '((1 "O'Brien" :null)) rows))
           (xeon.databases:query db "BEGIN IMMEDIATE")
           (xeon.databases:save-row db "sample" names (first rows) '(1 "updated" 42))
           (xeon.databases:query db "COMMIT")
           (assert (equal '((1 "updated" 42)) (nth-value 1 (xeon.databases:query db "SELECT * FROM sample")))))
         (assert (handler-case (progn (xeon.databases:query db "SELECT 1; DELETE FROM sample") nil) (error () t)))
         (multiple-value-bind (names rows more) (xeon.databases:query db "SELECT 1 UNION ALL SELECT 2" :limit 1)
           (declare (ignore names)) (assert (= 1 (length rows))) (assert more))
         ;; Scripts: every statement runs; the last one with rows is shown.
         (let ((result (xeon.databases:execute db (format nil "-- seed~%INSERT INTO sample VALUES (2, 'two', 2.5);~%INSERT INTO sample VALUES (3, 'three', 3);~%SELECT id, n FROM sample ORDER BY id;~%UPDATE sample SET n = n + 1 WHERE id > 1;~%/* done */"))))
           (assert (equal '("id" "n") (xeon.databases:result-columns result)))
           (assert (equalp #(#(1 42) #(2 2.5d0) #(3 3)) (xeon.databases:result-rows result)))
           (assert (= 4 (xeon.databases:result-statements result)))
           (assert (= 4 (xeon.databases:result-changes result)))
           (assert (not (xeon.databases:result-truncated-p result)))
           (assert (realp (xeon.databases:result-seconds result))))
         (let ((result (xeon.databases:execute db "DELETE FROM sample WHERE id = 3")))
           (assert (null (xeon.databases:result-columns result)))
           (assert (= 1 (xeon.databases:result-changes result))))
         (assert (xeon.databases:result-truncated-p
                  (xeon.databases:execute db "SELECT * FROM sample" :limit 1)))
         (let ((message (handler-case (progn (xeon.databases:execute db "SELECT 1; SELECT * FROM missing") nil)
                          (error (e) (princ-to-string e)))))
           (assert (search "statement 2" message))
           (assert (search "missing" message)))
         (assert (handler-case (progn (xeon.databases:execute db "  -- nothing  ") nil) (error () t)))
         ;; Tables and views.
         (xeon.databases:query db "CREATE VIEW \"Big \"\"names\"\"\" AS SELECT * FROM sample WHERE n > 2")
         (assert (equal '(("Big \"names\"" . :view) ("sample" . :table)) (xeon.databases:schema-objects db)))
         (assert (equal "SELECT * FROM \"Big \"\"names\"\"\"" (xeon.databases:table-select-sql "Big \"names\"")))
         (assert (= 2 (length (xeon.databases:result-rows
                               (xeon.databases:execute db (xeon.databases:table-select-sql "Big \"names\""))))))
         (xeon.databases:disconnect db)
         (assert (probe-file path))
         (assert (not (xeon.databases:connection-open-p db)))
         (setf db (xeon.databases:connect path))
         (assert (equal '("sample") (xeon.databases:tables db))))
    (when db (xeon.databases:disconnect db))
    (when (probe-file path) (delete-file path))))

;;; Workers
(let* ((path (database-test-path "worker.sqlite"))
       (worker nil))
  (xeon.databases:disconnect (xeon.databases:connect path :create t))
  (unwind-protect
       (let ((results '()) (lock (sb-thread:make-mutex)))
         (flet ((collect (value error)
                  (sb-thread:with-mutex (lock) (push (list value error) results))))
           (setf worker (xeon.databases:make-database-worker path))
           (xeon.databases:submit-database-job
            worker (lambda (db) (xeon.databases:execute db "CREATE TABLE t (x); INSERT INTO t VALUES (1), (2)")) #'collect)
           (xeon.databases:submit-database-job
            worker (lambda (db) (xeon.databases:result-rows (xeon.databases:execute db "SELECT x FROM t"))) #'collect)
           (xeon.databases:submit-database-job
            worker (lambda (db) (xeon.databases:execute db "SELECT nope")) #'collect)
           (assert (await-database (lambda () (= 3 (length results)))))
           (assert (not (xeon.databases:database-worker-busy-p worker)))
           (destructuring-bind (failed rows created) results
             (assert (and (first created) (null (second created))))
             (assert (equalp #(#(1) #(2)) (first rows)))
             (assert (and (null (first failed)) (search "nope" (second failed)))))
           ;; A runaway query is interrupted.
           (setf results '())
           (xeon.databases:submit-database-job
            worker (lambda (db) (xeon.databases:execute db "WITH RECURSIVE c(n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM c) SELECT count(*) FROM c"))
            #'collect)
           (assert (await-database (lambda () (xeon.databases:database-worker-busy-p worker))))
           (sleep 0.1)
           (xeon.databases:interrupt-database-worker worker)
           (assert (await-database (lambda () results)))
           (assert (search "interrupt" (second (first results))))
           (xeon.databases:stop-database-worker worker)
           (assert (xeon.databases:database-worker-stopped-p worker))
           (assert (handler-case (progn (xeon.databases:submit-database-job worker #'identity) nil) (error () t)))))
    (when worker (xeon.databases:stop-database-worker worker))
    (sleep 0.1)
    (when (probe-file path) (delete-file path))))

;;; Stored connections
(let ((file (database-test-path "connections.sexp")))
  (unwind-protect
       (let* ((store (xeon.databases:load-connection-store file)))
         (assert (null (xeon.databases:connection-store-profiles store)))
         (assert (null (xeon.databases:connection-store-warning store)))
         (let ((a (xeon.databases:add-connection-profile store "/tmp/app.sqlite"))
               (b (xeon.databases:add-connection-profile store "/tmp/other.db" :name "Other" :read-only t)))
           (assert (equal "app.sqlite" (xeon.databases:profile-name a)))
           (assert (eq :sqlite (xeon.databases:profile-kind a)))
           (assert (xeon.databases:profile-read-only-p b))
           (multiple-value-bind (again existing) (xeon.databases:add-connection-profile store "/tmp/app.sqlite")
             (assert (and (eq again a) existing)))
           (xeon.databases:rename-connection-profile store a "  App  ")
           (assert (equal "App" (xeon.databases:profile-name a)))
           (assert (handler-case (progn (xeon.databases:rename-connection-profile store a "   ") nil) (error () t)))
           (let ((loaded (xeon.databases:load-connection-store file)))
             (assert (equal '("App" "Other")
                            (mapcar #'xeon.databases:profile-name (xeon.databases:connection-store-profiles loaded))))
             (assert (equal (xeon.databases:profile-id b)
                            (xeon.databases:profile-id (second (xeon.databases:connection-store-profiles loaded)))))
             (assert (xeon.databases:find-connection-profile loaded (xeon.databases:profile-id a))))
           (xeon.databases:remove-connection-profile store a)
           (assert (equal '("Other") (mapcar #'xeon.databases:profile-name
                                             (xeon.databases:connection-store-profiles
                                              (xeon.databases:load-connection-store file)))))))
    (when (probe-file file) (delete-file file)))
  ;; Damaged or hostile files are ignored, with a warning.
  (dolist (text '("(:version 1 :connections ((:id \"x\" :name \"n\" :kind :postgres :path \"/p\" :read-only nil)))"
                  "(:version 1 :connections #.(error \"no\"))"
                  "(:version 2 :connections ())"
                  "not data ("))
    (with-open-file (out file :direction :output :if-exists :supersede) (write-string text out))
    (let ((store (xeon.databases:load-connection-store file)))
      (assert (null (xeon.databases:connection-store-profiles store)))
      (assert (xeon.databases:connection-store-warning store))))
  (delete-file file))

(format t "SQLite backend tests passed.~%")
