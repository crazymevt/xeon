;;; Local development setup; production systems do not change ASDF configuration.
(require :asdf)
(let* ((root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname *load-truename*)))
       ;; Mapped and shared drives can fail when ASDF opens compiled files.
       (cache #+win32
              (merge-pathnames "Xeon/cache/"
                               (let ((local (uiop:getenv "LOCALAPPDATA")))
                                 (if (and local (plusp (length local)))
                                     (uiop:ensure-directory-pathname (uiop:parse-native-namestring local))
                                     (uiop:temporary-directory))))
              #-win32 (merge-pathnames "build/cache/" root))
       (translations
         `(:output-translations (t (,(namestring cache) :implementation :**/ :*.*.*))
                                :ignore-inherited-configuration)))
  (asdf:initialize-output-translations translations)
  ;; Fresh SBCL processes in integration fixtures use the same cache.
  (setf (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS") (prin1-to-string translations))
  (let ((dependency (or (uiop:getenv "XEON_CL_PPCRE_ASD")
                        (probe-file (merge-pathnames "build/deps/cl-ppcre/cl-ppcre.asd" root)))))
    (when dependency (asdf:load-asd (truename dependency))))
  (asdf:load-asd (merge-pathnames "xeon.asd" root)))
