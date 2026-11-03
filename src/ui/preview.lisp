(in-package #:xeon.ui)

;;; Markdown preview: a native web view beside the editor, in the same tab. The
;;; backend renders Markdown as escaped HTML (never raw HTML from the document); the
;;; page is loaded once and its content replaced as the text changes, so the preview
;;; keeps its scroll position. Links open in the default browser.

(defparameter *preview-delay* 0.25
  "Seconds after the last edit before the preview is rendered again.")

(defun markdown-document-p (d)
  ;; Detected from the buffer, so a language change counts before it is restyled.
  (and d (eq :markdown (xeon.syntax:detect-language (document-buffer d)))))

(defun preview-colors (palette)
  "CSS custom properties for the preview: the platform's own colors for the System
theme, which follow light and dark mode, otherwise the custom theme's colors."
  (flet ((css (role native)
           (if (native-palette-p palette) (native-css native) (palette-color palette role))))
    (format nil "--fg:~A;--muted:~A;--accent:~A;--rule:~A;--code:~A;--kw:~A;--str:~A;--comment:~A;--number:~A;--symbol:~A;--function:~A;--parameter:~A"
            (css :editor-foreground :text)
            (css :syntax-comment :secondary-text)
            (css :accent :accent)
            (css :border :separator)
            (if (native-palette-p palette)
                "light-dark(rgba(0,0,0,.045),rgba(255,255,255,.06))"
                (palette-color palette :sidebar-background))
            (css :syntax-keyword :system-pink)
            (css :syntax-string :system-red)
            (css :syntax-comment :secondary-text)
            (css :syntax-number :system-orange)
            (css :syntax-symbol :system-teal)
            (css :syntax-function :system-purple)
            (css :syntax-parameter :system-indigo))))

(defun preview-background (palette)
  (if (native-palette-p palette) "transparent" (palette-color palette :editor-background)))

(defun preview-shell (palette)
  "The preview page, without content: styles, and a script that shows content the
editor sends and turns link clicks into messages."
  (format nil "<!doctype html><html><head><meta charset='utf-8'>
<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'\">
<style id='colors'>:root{~A}html{background:~A}</style>
<style>
html{color-scheme:light dark;color:var(--fg)}
body{margin:0;font:15px/1.65 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;overflow-wrap:anywhere;-webkit-font-smoothing:antialiased}
main{max-width:820px;margin:0 auto;padding:28px 36px 60px}
main>:first-child{margin-top:0}
h1,h2,h3,h4,h5,h6{line-height:1.3;margin:1.6em 0 .6em;font-weight:650}
h1{font-size:2em;padding-bottom:.3em;border-bottom:1px solid var(--rule)}
h2{font-size:1.5em;padding-bottom:.25em;border-bottom:1px solid var(--rule)}
h3{font-size:1.25em}h4{font-size:1.05em}
p{margin:0 0 1em}
a{color:var(--accent);text-decoration:none;cursor:pointer}a:hover{text-decoration:underline}
ul,ol{padding-left:1.6em;margin:0 0 1em}li{margin:.25em 0}
blockquote{margin:0 0 1em;padding:.2em 1em;border-left:3px solid var(--rule);color:var(--muted)}
hr{border:0;border-top:1px solid var(--rule);margin:2em 0}
code{font:.88em/1.6 ui-monospace,SFMono-Regular,Menlo,monospace;background:var(--code);border-radius:5px;padding:.15em .4em}
.code-block{margin:0 0 1.2em;background:var(--code);border-radius:8px;overflow:hidden}
.code-block header{padding:6px 14px 0;font-size:11px;color:var(--muted)}
.code-block .copy-code{display:none}
pre{margin:0;padding:10px 14px 14px;overflow-x:auto;tab-size:4;line-height:1.45}
pre code{background:none;padding:0;font-size:.85em;line-height:inherit}
.tok-1{color:var(--kw)}.tok-2{color:var(--str)}.tok-3{color:var(--comment);font-style:italic}.tok-4{color:var(--number)}
.tok-5{color:var(--symbol)}.tok-6{color:var(--function)}.tok-7{color:var(--parameter)}
.table-wrap{overflow-x:auto;margin:0 0 1em}
table{border-collapse:collapse;font-size:.92em}
th,td{border:1px solid var(--rule);padding:6px 12px;text-align:left}th{background:var(--code);font-weight:600}
.empty{color:var(--muted);text-align:center;margin-top:30vh}
</style></head><body><main id='doc'></main>
<script>
window.xeonShow=function(html){document.getElementById('doc').innerHTML=html};
window.xeonColors=function(css,background){document.getElementById('colors').textContent=':root{'+css+'}html{background:'+background+'}'};
document.addEventListener('click',function(e){var a=e.target.closest('a');if(!a)return;e.preventDefault();var url=a.getAttribute('data-url');if(url)window.lgt.postMessage(url)});
</script></body></html>"
          (preview-colors palette) (preview-background palette)))

(defun preview-content (d)
  (let ((text (xeon.core:buffer-text (document-buffer d))))
    (if (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
        "<p class='empty'>Nothing to preview yet.</p>"
        (xeon.chat-render:render-markdown text (lambda (index code) (declare (ignore code))
                                                 (princ-to-string index))))))

(defun render-preview (page)
  (let ((view (page-preview page)))
    (when (and view (not (destroyed-p view)) (web-view-ready-p view))
      (run-script view (format nil "xeonShow(~A)" (lgt::javascript-string (preview-content (page-document page))))
                  :callback (lambda (result error) (declare (ignore result error)))))))

(defun schedule-preview (workbench page)
  "Renders PAGE's preview once editing pauses."
  (declare (ignore workbench))
  (when (page-preview page)
    (when (page-preview-timer page) (stop-timer (page-preview-timer page)))
    (setf (page-preview-timer page)
          (after *preview-delay* (lambda ()
                                   (setf (page-preview-timer page) nil)
                                   (render-preview page))))))

(defun make-preview-view (workbench page)
  (let ((view (make-instance 'web-view :html (preview-shell (workbench-palette workbench))
                                       :local-messages-p t)))
    (on view :loaded (lambda (e) (declare (ignore e)) (render-preview page)))
    ;; Only the page itself loads here; links go to the browser through messages.
    (on view :navigation-request
        (lambda (e)
          (when (or (event-new-window-p e) (member (event-navigation-kind e) '(:link :form)))
            (veto e))))
    (on view :message
        (lambda (e)
          (let ((url (event-value e)))
            (when (xeon.chat-render::safe-link-p url) (open-url url)))))
    view))

(defun show-preview (workbench page)
  (unless (page-preview page)
    (let ((view (make-preview-view workbench page)))
      (setf (page-preview page) view)
      (add page view)
      (flet ((split-evenly ()
               (let ((width (widget-width page)))
                 (when (and (plusp width) (eq view (page-preview page)))
                   (setf (divider-position page) (floor width 2))))))
        (split-evenly)
        ;; The split view lays its new pane out on the next turn; split it evenly then.
        (after 0 #'split-evenly)))
    (refresh-preview-button workbench)))

(defun release-preview (page)
  (when (page-preview-timer page)
    (stop-timer (page-preview-timer page))
    (setf (page-preview-timer page) nil)))

(defun hide-preview (workbench page)
  (let ((view (page-preview page)))
    (when view
      (release-preview page)
      (setf (page-preview page) nil)
      (remove-child page view)
      (destroy view)
      (focus (page-editor page)))
    (refresh-preview-button workbench)))

(defun toggle-markdown-preview (workbench &optional (page (active-page workbench)))
  "Shows or hides the Markdown preview beside the editor."
  (cond ((null page) nil)
        ((page-preview page) (hide-preview workbench page) nil)
        ((markdown-document-p (page-document page)) (show-preview workbench page) t)
        (t (report workbench "Preview is available for Markdown documents.") nil)))

(defun restyle-previews (workbench)
  "Gives every open preview the current theme's colors."
  (let ((palette (workbench-palette workbench)))
    (dolist (page (all-tab-pages workbench))
      (let ((view (and (typep page 'document-page) (page-preview page))))
        (when (and view (web-view-ready-p view))
          (run-script view (format nil "xeonColors(~A,~A)"
                                   (lgt::javascript-string (preview-colors palette))
                                   (lgt::javascript-string (preview-background palette)))
                      :callback (lambda (result error) (declare (ignore result error)))))))))

;;; The Preview button at the end of the tab bar, shown for Markdown documents.

(defun preview-button (workbench)
  (find :preview (children (tab-bar-end (tab-bar (workbench-tabs workbench)))) :key #'user-data))

(defun make-preview-button (workbench)
  (let ((button (make-instance 'toggle-button :label "Preview" :control-size :small :visible nil
                                              :tooltip (format nil "Markdown preview · ~A" (shortcut-label "Primary+Shift+V")))))
    (setf (user-data button) :preview)
    (on button :toggled (lambda (e) (declare (ignore e))
                          (toggle-markdown-preview workbench)
                          (refresh-preview-button workbench)))
    button))

(defun refresh-preview-button (workbench)
  (let ((button (preview-button workbench))
        (page (active-page workbench)))
    (when button
      ;; A document that stopped being Markdown loses its preview.
      (when (and page (page-preview page) (not (markdown-document-p (page-document page))))
        (hide-preview workbench page)
        (return-from refresh-preview-button))
      (let ((shown (and page (markdown-document-p (page-document page)) t))
            (on (and page (page-preview page) t)))
        (unless (eq shown (visible button)) (setf (visible button) shown))
        (unless (eq on (checked button))
          (with-handlers-blocked (button :toggled) (setf (checked button) on)))))))
