(in-package #:xeon.themes)

(defun configuration-directory (platform getenv home)
  "The sole OS-specific configuration-location adapter. No directories are created."
  (labels ((environment-directory (name)
             (let ((value (funcall getenv name)))
               (when (and value (plusp (length value)))
                 (uiop:ensure-directory-pathname (uiop:parse-native-namestring value)))))
           (below (base &rest components)
             (merge-pathnames (make-pathname :directory (cons :relative components)) base)))
    (or (environment-directory "XEON_CONFIG_HOME")
        (ecase platform
          (:windows (below (or (environment-directory "APPDATA")
                               (below home "AppData" "Roaming")) "Xeon"))
          (:macos (below home "Library" "Application Support" "Xeon"))
          (:unix (below (or (environment-directory "XDG_CONFIG_HOME") (below home ".config")) "xeon"))))))

(defun default-configuration-directory ()
  (configuration-directory #+win32 :windows #+(and (not win32) darwin) :macos
                           #-(or win32 darwin) :unix #'uiop:getenv (user-homedir-pathname)))
