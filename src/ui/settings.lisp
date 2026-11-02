(in-package #:xeon.ui)

;;; Settings: a popover from the rail's gear (or Xeon › Settings…). Changes apply
;;; at once and are saved in Xeon's configuration directory.

(defun settings-anchor (workbench)
  (find :settings (rail-buttons workbench) :key #'user-data))

(defun save-settings (workbench &rest settings)
  (with-reported-errors (workbench)
    (apply #'xeon.themes:configure-settings (workbench-themes workbench) :persist t settings)
    (apply-theme workbench)))

(defun make-settings-content (workbench)
  (let* ((themes (workbench-themes workbench))
         (choices (xeon.themes:available-themes themes))
         (theme (make-instance 'combo-box :items choices :label-function #'second
                                          :selected-index (or (position (xeon.themes:selected-theme-id themes)
                                                                        choices :key #'first :test #'equal)
                                                              0)))
         (font-setting (xeon.themes:editor-font themes))
         (size (make-instance 'spin-button :min 8 :max 36 :value (or (second font-setting) 13) :digits 0))
         (tab (make-instance 'spin-button :min 1 :max 16 :value (xeon.themes:editor-tab-size themes) :digits 0))
         (trim (make-instance 'check-box :label "Trim trailing whitespace on save"
                                         :checked (xeon.themes:trim-trailing-whitespace-on-save themes))))
    (on theme :changed
        (lambda (e) (when (event-value e) (save-settings workbench :theme (first (event-value e))))))
    (on size :changed
        (lambda (e)
          (save-settings workbench :editor-font
                         (list (or (first (xeon.themes:editor-font themes)) "Menlo")
                               (round (event-value e))))))
    (on tab :changed (lambda (e) (save-settings workbench :tab-size (round (event-value e)))))
    (on trim :toggled (lambda (e) (save-settings workbench :trim-trailing-whitespace-on-save (and (event-value e) t))))
    (ui (vbox :padding 16 :spacing 12 :width 320
          (label :text "Settings" :font (make-font :size 15 :weight :bold))
          (grid :columns 2 :column-spacing 12 :row-spacing 10
            (label :text "Theme" :halign :end :valign :center) (:widget theme :hexpand t)
            (label :text "Editor font size" :halign :end :valign :center) (:widget size)
            (label :text "Tab width" :halign :end :valign :center) (:widget tab))
          (:widget trim)
          (label :text "Settings are saved automatically." :text-color :secondary-text
                 :font (make-font :size 11))))))

(defun show-settings (workbench)
  (let ((anchor (settings-anchor workbench)))
    (when (workbench-settings workbench)
      (ignore-errors (destroy (workbench-settings workbench))))
    (let ((popover (make-instance 'popover :behavior :transient :preferred-edge :right
                                           :content (make-settings-content workbench))))
      (setf (workbench-settings workbench) popover)
      (show-popover popover anchor))))
