(defpackage #:xeon.folders
  (:use #:cl)
  (:export #:folder-files)
  (:export #:project-run-command #:launch-project #:project-run #:project-run-process #:project-run-log)
  (:export #:git-status #:git-status-branch #:git-status-changes #:git-status-error #:read-git-status
           #:git-change #:git-change-path #:git-change-original #:git-change-index #:git-change-worktree
           #:git-change-conflict-p #:git-change-badge #:git-path-badge #:perform-git-action #:git-all-paths #:read-git-diff
           #:git-status-head #:git-status-upstream #:git-status-ahead #:git-status-behind #:git-status-gone-p
           #:git-status-detached-p #:git-status-unborn-p #:read-git-head-commit #:read-git-branches #:read-git-remotes
           #:git-failed #:git-failed-action #:git-failed-output #:valid-git-branch-name-p
           #:project-git-p #:file-icon-kind
           #:discover-folder-systems #:discovered-system-dialect #:discovered-system-name #:discovered-system-file #:system-action-source
           #:search-workspace #:start-workspace-search #:cancel-workspace-search #:workspace-search-status
           #:workspace-hit-path #:workspace-hit-buffer #:workspace-hit-revision #:workspace-hit-start #:workspace-hit-end
           #:workspace-hit-line #:workspace-hit-column #:workspace-hit-signature #:workspace-hit-preview
           #:workspace-report-hits #:workspace-report-skipped #:workspace-report-truncated-p
           #:start-file-copy #:file-copy-status #:cancel-file-copy #:copy-folder-entries #:normalize-folder-entries #:move-folder-entries #:delete-folder-entries
           #:folder-batch-error #:batch-completed #:batch-cause #:entry-open-buffers #:create-folder-entry #:move-folder-entry #:rename-folder-entry #:delete-folder-entry #:entry-parent #:entry-leaf
           #:folder-project #:open-folder-project #:project-root #:project-warning
           #:folder-entry #:entry-path #:entry-name #:entry-directory-p #:entry-link-p #:entry-icon
           #:list-folder-children #:make-folder-worker #:request-folder-work #:drain-folder-results
           #:stop-folder-worker #:folder-result-id #:folder-result-value #:folder-result-error))
