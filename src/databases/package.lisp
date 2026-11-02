;;;; Headless database access: the native SQLite driver, a worker thread per open
;;;; connection, and the user's stored connections. No UI code.

(defpackage #:xeon.databases
  (:use #:cl)
  (:export
   ;; Driver
   #:connection-path #:connection-open-p #:connect #:disconnect #:query #:execute #:interrupt
   #:tables #:schema-objects #:table-select-sql #:quote-name #:sql-value #:save-row
   #:result #:result-columns #:result-rows #:result-truncated-p #:result-changes
   #:result-statements #:result-seconds
   ;; Workers
   #:database-worker #:make-database-worker #:database-worker-path #:database-worker-busy-p
   #:database-worker-stopped-p #:submit-database-job #:interrupt-database-worker
   #:stop-database-worker
   ;; Stored connections
   #:connection-profile #:profile-id #:profile-name #:profile-kind #:profile-path
   #:profile-read-only-p #:profile-pathname
   #:connection-store #:load-connection-store #:connection-store-path
   #:connection-store-profiles #:connection-store-warning #:find-connection-profile
   #:add-connection-profile #:rename-connection-profile #:remove-connection-profile))
