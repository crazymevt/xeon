(defpackage #:xeon.core
  (:use #:cl)
  (:export #:buffer-cursor #:make-buffer-cursor #:cursor-point #:cursor-anchor
   #:capture-buffer-cursor #:restore-buffer-cursor #:rebase-buffer-cursor
   #:hyperspec-target #:hyperspec-buffer #:documentation-docsets #:documentation-records
   #:visit-documentation-entry #:navigate-documentation #:documentation-page
   #:reference-docset #:reference-entry
   #:buffer #:make-buffer #:copy-buffer #:replace-buffer-from-buffer #:buffer-name #:buffer-pathname #:buffer-language-override
   #:buffer-text #:buffer-length #:buffer-substring #:buffer-revision
   #:buffer-history-limit
   #:buffer-modified-p #:mark-buffer-saved #:buffer-read-only-p
   #:buffer-point #:buffer-anchor #:set-selection #:selection-range
   #:replace-range #:insert-text #:delete-range #:replace-selection #:paired-delimiter #:wrap-selection
   #:trim-trailing-whitespace #:join-lines
   #:delete-backward #:delete-forward #:move-point #:smart-home
   #:position-line-column #:line-column-position
   #:undo-edit #:redo-edit #:can-undo-p #:can-redo-p
   #:call-with-edit-group #:with-edit-group
   #:add-change-listener #:remove-change-listener
   #:buffer-change #:buffer-change-kind #:buffer-change-start
   #:buffer-change-removed #:buffer-change-inserted #:buffer-change-revision
   #:open-buffer-file #:save-buffer #:save-buffer-as #:reload-buffer
   #:check-buffer-file #:refresh-buffer-file
   #:buffer-file-error #:buffer-file-error-pathname #:buffer-file-error-operation
   #:file-conflict #:file-conflict-status #:unsaved-buffer-changes
   #:buffer-manager #:make-buffer-manager #:manager-buffers #:current-buffer
   #:manager-buffer-id #:find-managed-buffer #:add-buffer #:new-buffer
   #:open-managed-file #:switch-buffer #:close-buffer #:save-all-buffers
   #:call-with-buffer-removals #:call-with-file-retargets #:buffer-needs-saving-p
   #:disposable-untitled-buffer-p
   #:manager-error #:manager-error-manager #:manager-error-buffer
   #:manager-busy #:buffer-not-managed #:buffer-already-managed
   #:buffer-file-already-open #:already-open-buffer #:unsaved-buffer-close
   #:buffer-save-result #:buffer-save-result-buffer #:buffer-save-result-id
   #:buffer-save-result-status #:buffer-save-result-condition
   #:add-manager-listener #:remove-manager-listener #:manager-change
   #:manager-change-kind #:manager-change-buffer #:manager-change-id
   #:manager-change-previous-current #:manager-change-current
   #:search-query #:make-search-query #:search-session #:make-search-session #:search-session-current-p
   #:search-match #:search-match-start #:search-match-end #:search-match-revision
   #:search-match-buffer #:search-match-text #:search-match-group-range #:search-word-character-p
   #:map-search-matches #:find-search-match #:search-buffer #:search-all
   #:select-next-search-match
   #:replace-search-match #:replace-all
   #:search-error #:search-error-detail #:invalid-search-pattern #:invalid-replacement
   #:stale-search-match #:search-cancelled
   #:buffer-error #:invalid-buffer-range #:read-only-buffer #:buffer-busy))
