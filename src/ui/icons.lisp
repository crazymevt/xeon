(in-package #:xeon.ui)

;;; Material Icons, bundled with its license under assets/fonts/material-icons/.
;;; Explicit codepoints, never ligatures or a system font fallback.

(defparameter *glyphs*
  '((:folder . #xe2c7) (:folder-open . #xe2c8) (:file . #xe873) (:explorer . #xe2c7)
    (:code . #xe86f) (:data . #xead3) (:image . #xe3f4) (:archive . #xe149)
    (:text . #xef42) (:settings . #xe8b8) (:git . #xe97a) (:search . #xe8b6)
    (:refresh . #xe5d5) (:bug . #xe868) (:inspect . #xe8f4) (:problems . #xe002)
    (:repl . #xe86f) (:terminal . #xeb8e) (:chat . #xe0cb) (:close . #xe5cd)
    (:outline . #xe8ef) (:add . #xe145) (:play . #xe037) (:stop . #xe047)
    (:circle . #xe061) (:error . #xe000) (:warning . #xe002) (:info . #xe88e)
    (:projects . #xe1a1)
    (:lisp . #xe86f) (:clojure . #xe86f) (:racket . #xe86f)))

(defparameter *icon-font* "Material Icons")

(defvar *icon-font-backend* nil
  "The backend the bundled icon font was registered with.")

(defun icon-font-path ()
  (asdf:system-relative-pathname "xeon" "assets/fonts/material-icons/MaterialIcons-Regular.ttf"))

(defun ensure-icon-font ()
  (let ((backend (current-backend)))
    (unless (eq backend *icon-font-backend*)
      (register-font-file (icon-font-path))
      (setf *icon-font-backend* backend))))

(defun glyph (name)
  (code-char (or (cdr (assoc name *glyphs*)) (error "Unknown icon ~S." name))))

(defun icon (name &key (size 18) color)
  "An image of the icon NAME. Without COLOR it is a template that follows the native
appearance and selection; with one it keeps that color."
  (ensure-icon-font)
  (if color
      (glyph-image (glyph name) :font *icon-font* :size size :color color)
      (glyph-image (glyph name) :font *icon-font* :size size)))

;;; File-type icons take their colors from the theme's syntax roles, as in the
;;; original Xeon explorer.

(defparameter *file-icon-roles*
  '((:folder :folder :syntax-function) (:folder-open :folder-open :syntax-function)
    (:file :file :sidebar-foreground) (:lisp :code :syntax-keyword)
    (:clojure :code :syntax-string) (:racket :code :syntax-parameter)
    (:code :code :syntax-number) (:data :data :syntax-symbol)
    (:image :image :syntax-string) (:archive :archive :syntax-function)
    (:text :text :syntax-symbol) (:git :git :syntax-number)))

(defparameter *native-file-icon-colors*
  '((:folder . :system-blue) (:folder-open . :system-blue) (:file . :system-gray)
    (:lisp . :system-purple) (:clojure . :system-green) (:racket . :system-red)
    (:code . :system-orange) (:data . :system-teal) (:image . :system-pink)
    (:archive . :system-brown) (:text . :system-gray) (:git . :system-orange))
  "File icon colors in the System theme, from the platform palette; folders are blue as
in the Finder.")

(defun file-icon (palette kind)
  (destructuring-bind (glyph role)
      (rest (or (assoc kind *file-icon-roles*) (assoc :file *file-icon-roles*)))
    (icon glyph :size 16
                :color (if (native-palette-p palette)
                           (or (cdr (assoc kind *native-file-icon-colors*)) :system-gray)
                           (palette-color palette role)))))
