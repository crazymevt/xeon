(in-package #:xeon.ui)

;;; Xeon's palettes come from xeon.themes. Native controls keep their own look in
;;; the palette's light or dark appearance; the palette colors syntax and icons.

(defun rgb-hex (value)
  (format nil "#~6,'0X" value))

(defparameter *native-colors*
  '((:syntax-keyword . :system-pink) (:syntax-string . :system-red)
    (:syntax-comment . :secondary-text) (:syntax-number . :system-orange)
    (:syntax-symbol . :system-teal) (:syntax-function . :system-purple)
    (:syntax-parameter . :system-indigo) (:inserted . :system-green) (:deleted . :system-red)
    (:sidebar-foreground . :secondary-text) (:accent . :accent))
  "The System theme's colors: the platform's own adaptable colors, which follow light and
dark mode, increased contrast and the user's accent color.")

(defun native-palette-p (palette) (eq palette :native))

(defun palette-color (palette role)
  "The color for ROLE: a system color keyword for the System theme, otherwise the
custom theme's color as a hex string."
  (if (native-palette-p palette)
      (or (cdr (assoc role *native-colors*)) :text)
      (case role
        (:inserted "#50B97D")
        (:deleted "#EB646E")
        (t (rgb-hex (xeon.themes:theme-colour palette role))))))

(defun palette-dark-p (palette)
  (if (native-palette-p palette)
      (dark-mode-p)
      (destructuring-bind (r g b) (xeon.themes:theme-rgb palette :editor-background)
        (< (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 128))))

(defparameter *syntax-styles*
  #(nil :keyword :string :comment :number :symbol :function :parameter :inserted :deleted)
  "Style names for the backend lexer's per-character style codes.")

(defun syntax-text-styles (palette)
  (flet ((role (name) (palette-color palette name)))
    (list :keyword (list :color (role :syntax-keyword))
          :string (list :color (role :syntax-string))
          :comment (list :color (role :syntax-comment) :italic t)
          :number (list :color (role :syntax-number))
          :symbol (list :color (role :syntax-symbol))
          :function (list :color (role :syntax-function))
          :parameter (list :color (role :syntax-parameter))
          :inserted (list :color (role :inserted))
          :deleted (list :color (role :deleted)))))

(defun make-theme-manager ()
  "The user's theme and editor preferences, or the built-in defaults if they cannot be read."
  (let ((directory (xeon.themes:default-configuration-directory)))
    (handler-case
        (xeon.themes:make-theme-manager
         :directory (merge-pathnames "themes/" directory)
         :preferences-path (merge-pathnames "preferences.sexp" directory))
      (error (condition)
        (warn "Using default theme settings: ~A" condition)
        (xeon.themes:make-theme-manager :load-p nil)))))

(defun resolve-palette (manager)
  "The selected palette: :NATIVE for the System theme, which uses the platform's own
colors, otherwise the custom theme's resolved palette."
  (when (equal "system" (xeon.themes:selected-theme-id manager))
    (return-from resolve-palette :native))
  (let ((variant (if (dark-mode-p) "dark" "light")))
    (handler-case (xeon.themes:resolve-theme manager (xeon.themes:selected-theme-id manager)
                                             :system-variant variant)
      (error ()
        (xeon.themes:resolve-theme manager "system" :system-variant variant)))))

(defun theme-appearance (manager palette)
  (declare (ignore manager))
  (cond ((native-palette-p palette) :system)
        ((palette-dark-p palette) :dark)
        (t :light)))

(defun editor-font (manager)
  (let ((setting (xeon.themes:editor-font manager)))
    (if setting
        (make-font :family (first setting) :size (second setting))
        (make-font :family :monospace :size 13))))
