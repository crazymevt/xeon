(defpackage #:xeon.themes
  (:use #:cl)
  (:export #:make-workspace-history #:load-workspace-history #:workspace-history-files #:workspace-history-folders
           #:workspace-history-last-folder #:workspace-history-warning #:rewrite-workspace-entries #:rewrite-workspace-paths #:remember-workspace-path #:theme-error #:make-theme-manager #:available-themes #:selected-theme-id
           #:select-theme #:reload-theme-files #:import-theme-file #:resolve-theme #:theme-colour #:theme-rgb
           #:load-sidebar-state #:save-sidebar-state #:load-chat-composer-height #:save-chat-composer-height #:theme-signature #:default-configuration-directory #:theme-directory
           #:application-font #:editor-font #:trim-trailing-whitespace-on-save #:editor-tab-size #:configure-settings #:preferences-path #:preferences-version #:write-theme-template))
