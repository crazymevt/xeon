;;; Local development setup; production systems do not change ASDF configuration.
(require :asdf)
(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:initialize-output-translations
   `(:output-translations (t (,(merge-pathnames "build/cache/" root)
                              :implementation :**/ :*.*.*))
                          :ignore-inherited-configuration))
  ;; Fresh SBCL processes in integration fixtures also compile local systems.
  (setf (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")
        (format nil "(:output-translations (t (~S :implementation :**/ :*.*.*)) :ignore-inherited-configuration)"
                (namestring (merge-pathnames "build/cache/" root))))
  (let ((dependency (or (uiop:getenv "XEON_CL_PPCRE_ASD")
                        (probe-file (merge-pathnames "build/deps/cl-ppcre/cl-ppcre.asd" root)))))
    (when dependency (asdf:load-asd (truename dependency))))
  (asdf:load-asd (merge-pathnames "xeon.asd" root)))
