(in-package #:xeon.ui)

;;; The runtime tools pane: REPL, Problems, Debugger and Inspector, over one
;;; backend REPL session. The session reports changes from its own thread; the
;;; pane coalesces them into one UI update and rebuilds only what changed.

(defparameter *tool-pages*
  '((:repl "REPL" :repl) (:problems "Problems" :problems)
    (:debugger "Debugger" :bug) (:inspector "Inspector" :inspect)))

(defparameter *dialects*
  '((:common-lisp "Common Lisp (Swank)" "Start SBCL" "sbcl")
    (:clojure "Clojure (nREPL)" "Start Clojure" "clojure")
    (:racket "Racket (Cadre)" "Start Racket" "racket")))

(defclass repl-tools ()
  ((workbench :initarg :workbench :reader tools-workbench)
   (session :accessor tools-session)
   (last :initform nil :accessor tools-last)
   (queued :initform nil :accessor tools-queued)
   (lock :initform (sb-thread:make-mutex :name "Xeon REPL delivery") :reader tools-lock)
   (history-index :initform -1 :accessor tools-history-index)
   (history-draft :initform "" :accessor tools-history-draft)
   (stack :accessor tools-stack)
   (switches :initform '() :accessor tools-switches)
   (widgets :initform (make-hash-table) :reader tools-widgets)))

(defun tool (tools name) (gethash name (tools-widgets tools)))
(defun (setf tool) (widget tools name) (setf (gethash name (tools-widgets tools)) widget))

(defun repl-snapshot (tools) (xeon.repl:repl-snapshot (tools-session tools)))

(defun repl-status (tools) (getf (repl-snapshot tools) :status))

(defun repl-ready-p (tools) (member (repl-status tools) '(:ready :input)))

(defun selected-dialect (tools)
  (first (nth (or (selected-index (tool tools :dialect)) 0) *dialects*)))

;;; Inputs

(defclass repl-input (code-view)
  ((tools :initarg :tools :reader input-tools))
  (:default-initargs :line-numbers-p nil :wrap t :vexpand nil))

(defmethod measure ((view repl-input)) (values 200 64))

(defmethod handle-event ((view repl-input) (event key-event))
  (when (and (eq (event-signal event) :key-down) (not (event-consumed-p event)))
    (let ((tools (input-tools view)))
      (case (event-key event)
        (:return
         (when (null (event-modifiers event))
           (submit-repl-input tools)
           (consume event)))
        ((:up :down)
         ;; History only from the first or last line, so multi-line input still edits.
         (when (and (null (event-modifiers event))
                    (let* ((buffer (buffer view)) (line (position-line buffer (cursor view))))
                      (if (eq (event-key event) :up)
                          (zerop line)
                          (= line (1- (buffer-line-count buffer))))))
           (browse-repl-history tools (event-key event))
           (consume event))))))
  (call-next-method))

(defclass repl-transcript (code-view) ()
  (:default-initargs :line-numbers-p nil :wrap t :editable nil))

(defmethod measure ((view repl-transcript)) (values 200 120))

(defun submit-repl-input (tools)
  (let* ((input (tool tools :input)) (source (text input)))
    (with-reported-errors ((tools-workbench tools))
      (unless (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) source)
        (unless (repl-ready-p tools) (error "Connect a ready REPL before evaluating."))
        (xeon.repl:repl-submit (tools-session tools) source)
        (setf (text input) "" (tools-history-index tools) -1 (tools-history-draft tools) "")))))

(defun browse-repl-history (tools direction)
  (let ((history (xeon.repl:repl-history (tools-session tools)))
        (input (tool tools :input)))
    (when history
      (when (= (tools-history-index tools) -1)
        (setf (tools-history-draft tools) (text input)))
      (setf (tools-history-index tools)
            (max -1 (min (1- (length history))
                         (+ (tools-history-index tools) (if (eq direction :up) 1 -1)))))
      (setf (text input) (if (= (tools-history-index tools) -1)
                             (tools-history-draft tools)
                             (nth (tools-history-index tools) history)))
      (setf (cursor input) (text-length input)))))

;;; Commands

(defun start-repl (tools)
  (let* ((dialect (selected-dialect tools))
         (executable (fourth (assoc dialect *dialects*)))
         (folder (or (explorer-folder (tools-workbench tools)) (uiop:getcwd))))
    (xeon.repl:connect-repl (tools-session tools) dialect "127.0.0.1" (repl-port tools)
                            :launch t :executable executable :directory folder)))

(defun repl-port (tools)
  (let ((port (ignore-errors (parse-integer (string-trim " " (text (tool tools :port)))))))
    (unless (typep port '(integer 1 65535)) (error "Enter a port number from 1 to 65535."))
    port))

(defun connect-repl (tools)
  (xeon.repl:connect-repl (tools-session tools) (selected-dialect tools)
                          (string-trim " " (text (tool tools :host))) (repl-port tools)))

(defun inspect-latest-result (tools)
  (let ((result (first (getf (repl-snapshot tools) :results))))
    (unless result (error "There is no retained result to inspect."))
    (xeon.repl:repl-inspect-result (tools-session tools) (getf result :id) (getf result :generation))
    (select-tool (tools-workbench tools) :inspector)))

(defun send-editor-source (workbench scope compile-p)
  "Evaluates or compiles the current form or the whole document in the REPL."
  (let* ((tools (workbench-repl workbench))
         (view (active-editor workbench))
         (d (and view (view-document view))))
    (unless d (error "Select a source document first."))
    (unless (lisp-document-p d) (error "Select a Lisp source document first."))
    (unless (eq (document-language d) (getf (repl-snapshot tools) :dialect))
      (error "Connect a REPL matching the document's language first."))
    (let ((buffer (document-buffer d)))
      (multiple-value-bind (start end)
          (ecase scope
            (:buffer (values 0 (xeon.core:buffer-length buffer)))
            (:form (xeon.syntax:current-form-range (document-syntax d) (cursor view))))
        (unless start (error "There is no form at the caret."))
        (let* ((id (xeon.core:manager-buffer-id (workbench-manager workbench) buffer))
               (source (xeon.core:buffer-substring buffer start end))
               (path (xeon.core:buffer-pathname buffer))
               (context (list :source-id id :buffer-id id :position start
                              :revision (xeon.core:buffer-revision buffer)
                              :buffer-name (xeon.core:buffer-name buffer)
                              :file (and path (namestring path)))))
          (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
            (error "There is no source to send."))
          (if compile-p
              (multiple-value-bind (line column) (xeon.core:position-line-column buffer start)
                (xeon.repl:repl-compile (tools-session tools) source
                                        :source-context context :line (1+ line) :column (1+ column)))
              (xeon.repl:repl-submit (tools-session tools) source :source-context context))
          (select-tool workbench :repl))))))

(defun visit-problem (workbench issue)
  (let* ((manager (workbench-manager workbench))
         (buffer (xeon.core:find-managed-buffer manager (getf issue :buffer-id)))
         (file (getf issue :file)))
    (when (and buffer file
               (not (equal (ignore-errors (truename file))
                           (and (xeon.core:buffer-pathname buffer)
                                (ignore-errors (truename (xeon.core:buffer-pathname buffer)))))))
      (setf buffer nil))
    (cond (buffer
           (unless (eql (getf issue :revision) (xeon.core:buffer-revision buffer))
             (error "This diagnostic is out of date; compile the document again."))
           (show-document workbench (find buffer (workbench-documents workbench) :key #'document-buffer)))
          (file (open-document workbench file))
          (t (error "This diagnostic has no source location.")))
    (let* ((view (active-editor workbench))
           (text (document-text (view-document view)))
           (position (or (getf issue :position)
                         (when (getf issue :line)
                           (let ((line (min (1- (buffer-line-count text)) (max 0 (1- (getf issue :line))))))
                             (+ (line-start text line) (max 0 (1- (or (getf issue :column) 1))))))
                         0)))
      (setf (cursor view) (min (text-length view) position))
      (scroll-to-position view (cursor view))
      (focus view))))

;;; Refreshing from snapshots

(defun notify-tools (tools)
  "Called on the REPL thread. Queues at most one UI update."
  (when (sb-thread:with-mutex ((tools-lock tools))
          (unless (tools-queued tools) (setf (tools-queued tools) t)))
    (call-in-ui-thread
     (lambda ()
       (sb-thread:with-mutex ((tools-lock tools)) (setf (tools-queued tools) nil))
       (refresh-tools tools))
     :owner (tool tools :root))))

(defun set-enabled (widget value)
  (let ((value (and value t)))
    (unless (eq value (enabled widget)) (setf (enabled widget) value))))

(defun status-text (snapshot)
  (case (getf snapshot :status)
    (:disconnected "Disconnected")
    (:connecting "Connecting…")
    (:ready (format nil "Connected · ~A" (getf snapshot :prompt)))
    (:busy "Evaluating… · Interrupt to pause")
    (:input "Waiting for input · Submit a response below")
    (:debugging "Paused in the debugger")
    (:error "Connection failed")
    (t (format nil "REPL · ~(~A~)" (getf snapshot :status)))))

(defun transcript-runs (snapshot)
  (loop for (from to kind) in (getf snapshot :spans)
        for style = (case kind (:prompt :keyword) (:result :function))
        when (and style (< from to)) collect (list from to :style style)))

(defun refresh-tools (tools)
  (let* ((snapshot (repl-snapshot tools))
         (last (tools-last tools))
         (state (getf snapshot :status))
         (capabilities (getf snapshot :capabilities))
         (changed (lambda (key) (or (null last) (not (equal (getf snapshot key) (getf last key)))))))
    (dolist (name '(:dialect :host :port :connect :start))
      (set-enabled (tool tools name) (member state '(:disconnected :error))))
    (set-enabled (tool tools :stop) (not (eq state :disconnected)))
    (set-enabled (tool tools :evaluate) (member state '(:ready :input)))
    (set-enabled (tool tools :compile) (and (eq state :ready) (member :compile capabilities)))
    (set-enabled (tool tools :interrupt)
                 (and (member :interrupt capabilities) (member state '(:busy :input :debugging))))
    (set-enabled (tool tools :inspect-value)
                 (and (member :inspector capabilities) (getf snapshot :results)))
    (when (or (funcall changed :status) (funcall changed :prompt))
      (setf (text (tool tools :status)) (status-text snapshot)))
    (when (funcall changed :text)
      (let* ((buffer (buffer (tool tools :transcript)))
             (old (buffer-text buffer)) (new (getf snapshot :text))
             (prefix (or (mismatch old new) (length old))))
        (replace-buffer-range buffer prefix (length old) (subseq new prefix))
        (setf (text-runs (tool tools :transcript)) (transcript-runs snapshot))
        (setf (cursor (tool tools :transcript)) (length new))
        (scroll-to-position (tool tools :transcript) (length new))))
    (when (funcall changed :problems)
      (refresh-problems tools (getf snapshot :problems)))
    (when (funcall changed :debug)
      (when (and (getf snapshot :debug) (not (getf last :debug)))
        (select-tool (tools-workbench tools) :debugger))
      (refresh-debugger tools (getf snapshot :debug)))
    (when (funcall changed :inspection)
      (when (getf snapshot :inspection) (select-tool (tools-workbench tools) :inspector))
      (refresh-inspector tools (getf snapshot :inspection) capabilities))
    (refresh-language-status (tools-workbench tools))
    (setf (tools-last tools) snapshot)))

(defun refresh-problems (tools problems)
  (setf (text (tool tools :problems-summary))
        (if problems
            (format nil "~D diagnostic~:P · double-click to open the source" (length problems))
            "No problems reported. Compile a document to check it."))
  (setf (items (tool tools :problems)) problems)
  (refresh-switch-badges tools))

(defun refresh-debugger (tools debug)
  (setf (text (tool tools :condition))
        (or (getf debug :condition)
            "No paused evaluation. Conditions, restarts, stack frames and local values appear here when execution stops."))
  (set-enabled (tool tools :abort) debug)
  (setf (items (tool tools :restarts))
        (unless (getf debug :post-mortem)
          (loop for restart in (getf debug :restarts) for i from 0 collect (cons i restart)))
        (items (tool tools :frames)) (getf debug :frames)
        (items (tool tools :locals)) (getf debug :locals))
  (setf (user-data (tool tools :condition)) debug))

(defun refresh-inspector (tools inspection capabilities)
  (setf (text (tool tools :inspection-title))
        (or (getf inspection :title)
            "Inspect a live value by expression, or use Inspect value in the REPL. Double-click a field to explore it; Back returns to its parent."))
  (setf (items (tool tools :parts)) (first (getf inspection :content)))
  (setf (user-data (tool tools :parts)) inspection)
  (set-enabled (tool tools :inspect) (member :inspector capabilities))
  (set-enabled (tool tools :inspect-back) (and (member :inspector capabilities) inspection))
  (set-enabled (tool tools :inspect-refresh) (and (member :inspector capabilities) inspection)))

(defun refresh-switch-badges (tools)
  (let ((count (length (items (tool tools :problems))))
        (switch (second (assoc :problems (tools-switches tools)))))
    (setf (tooltip switch) (if (plusp count) (format nil "Problems (~D)" count) "Problems"))))

;;; Selecting a page

(defun select-tool (workbench page &key (show t))
  "Shows the tools pane's PAGE: :REPL, :PROBLEMS, :DEBUGGER or :INSPECTOR."
  (let* ((tools (workbench-repl workbench))
         (entry (assoc page (tools-switches tools))))
    (when entry
      (when show (show-tools workbench))
      (select-page (tools-stack tools) (third entry))
      (loop for (name switch) in (tools-switches tools)
            for glyph = (third (assoc name *tool-pages*))
            do (with-handlers-blocked (switch :toggled)
                 (setf (checked switch) (eq name page)
                       ;; The accent-colored icon marks the shown page.
                       (image switch) (if (eq name page)
                                          (icon glyph :size 18 :color (palette-color (workbench-palette workbench) :accent))
                                          (icon glyph :size 18))))))))

;;; Building

(defun section-label (text)
  (make-instance 'label :text text :font (make-font :size 13 :weight :bold)))

(defun hint-label (text)
  (make-instance 'label :text text :text-color :secondary-text :wrap t
                        :font (make-font :size 11)))

(defun small-button (label &rest initargs)
  (apply #'make-instance 'button :label label :control-size :small initargs))

(defun make-repl-page (tools)
  (let* ((w (tools-widgets tools))
         (status (hint-label "Disconnected"))
         (dialect (make-instance 'combo-box :items (mapcar #'second *dialects*) :selected-index 0
                                            :tooltip "REPL protocol · choose before connecting"))
         (host (make-instance 'entry :text "127.0.0.1" :width-chars 10 :control-size :small
                                     :tooltip "Existing REPL server host"))
         (port (make-instance 'entry :text "4005" :width-chars 5 :control-size :small
                                     :tooltip "Existing REPL server port"))
         (connect (small-button "Connect" :tooltip "Connect to a running server"))
         (start (small-button "Start SBCL" :tooltip "Launch the runtime in the open folder; it must be on PATH"))
         (stop (small-button "Stop" :tooltip "Disconnect, stopping a runtime Xeon started"))
         (transcript (make-instance 'repl-transcript :font (editor-font (workbench-themes (tools-workbench tools)))
                                                     :text-styles (syntax-text-styles (workbench-palette (tools-workbench tools)))))
         (inspect-value (small-button "Inspect value" :tooltip "Inspect the latest result"))
         (input (make-instance 'repl-input :tools tools
                                           :font (editor-font (workbench-themes (tools-workbench tools)))
                                           :tooltip "Return evaluates · Shift+Return newline · Up/Down history"))
         (evaluate (small-button "Evaluate" :default nil))
         (compile (small-button "Compile" :tooltip "Compile and install the active document without saving"))
         (interrupt (small-button "Interrupt"))
         (clear (small-button "Clear")))
    (setf (gethash :status w) status (gethash :dialect w) dialect (gethash :host w) host
          (gethash :port w) port (gethash :connect w) connect (gethash :start w) start
          (gethash :stop w) stop (gethash :transcript w) transcript (gethash :input w) input
          (gethash :inspect-value w) inspect-value (gethash :evaluate w) evaluate
          (gethash :compile w) compile (gethash :interrupt w) interrupt (gethash :clear w) clear)
    (let ((workbench (tools-workbench tools)))
      (macrolet ((clicked (button &body body)
                   `(on ,button :clicked (lambda (event) (declare (ignore event))
                                           (with-reported-errors (workbench) ,@body)))))
        (clicked connect (connect-repl tools))
        (clicked start (start-repl tools))
        (clicked stop (xeon.repl:disconnect-repl (tools-session tools)))
        (clicked evaluate (submit-repl-input tools))
        (clicked compile (send-editor-source workbench :buffer t))
        (clicked interrupt (xeon.repl:repl-interrupt (tools-session tools)))
        (clicked clear (xeon.repl:repl-clear (tools-session tools)))
        (clicked inspect-value (inspect-latest-result tools))))
    (on dialect :changed
        (lambda (event)
          (setf (label start) (third (nth (or (event-index event) 0) *dialects*)))))
    (ui (vbox :padding '(10 12 12 12) :spacing 8
          (:widget (section-label "REPL"))
          (:widget status)
          (:widget dialect)
          (hbox :spacing 6
            (:widget host) (:widget port) (spacer) (:widget connect) (:widget start) (:widget stop))
          (:widget transcript :expand t)
          (hbox (:widget inspect-value) (spacer))
          (:widget (hint-label "Return evaluates · Shift+Return inserts a newline"))
          (:widget input)
          (hbox :spacing 6
            (:widget evaluate) (:widget compile) (:widget interrupt) (:widget clear) (spacer))))))

(defun problem-label (issue)
  (format nil "~:(~A~) · ~A — ~A~@[:~A~]"
          (or (getf issue :severity) "note") (getf issue :message)
          (let ((file (getf issue :file))) (if file (file-namestring file) "REPL"))
          (getf issue :line)))

(defun problem-icon (issue)
  (case (getf issue :severity)
    ((:error :read-error) (icon :error :size 16 :color :system-red))
    ((:warning :style-warning) (icon :warning :size 16 :color :system-yellow))
    (t (icon :info :size 16 :color :system-blue))))

(defun make-problems-page (tools)
  (let* ((workbench (tools-workbench tools))
         (summary (hint-label "No problems reported. Compile a document to check it."))
         (list (make-instance 'list-box :label-function #'problem-label :icon-function #'problem-icon)))
    (setf (tool tools :problems-summary) summary (tool tools :problems) list)
    (on list :row-activated
        (lambda (event) (with-reported-errors (workbench) (visit-problem workbench (event-item event)))))
    (ui (vbox :padding '(10 12 12 12) :spacing 8
          (:widget (section-label "Problems"))
          (:widget summary)
          (:widget list :expand t)))))

(defun make-debugger-page (tools)
  (let* ((workbench (tools-workbench tools))
         (condition (make-instance 'label :text "" :wrap t :selectable t))
         (abort (small-button "Abort / Dismiss" :enabled nil))
         (restarts (make-instance 'list-box :label-function
                                  (lambda (entry) (format nil "~A · ~A" (second entry) (third entry)))))
         (frames (make-instance 'list-box :label-function
                                (lambda (frame) (format nil "~A · ~A" (first frame) (second frame)))))
         (locals (make-instance 'list-box :label-function
                                (lambda (local) (format nil "~A = ~A" (getf local :name) (getf local :value))))))
    (setf (tool tools :condition) condition (tool tools :abort) abort (tool tools :restarts) restarts
          (tool tools :frames) frames (tool tools :locals) locals)
    (flet ((debug () (user-data condition))
           (session () (tools-session tools)))
      (on abort :clicked (lambda (e) (declare (ignore e))
                           (with-reported-errors (workbench) (xeon.repl:repl-debug-abort (session)))))
      (on restarts :row-activated
          (lambda (e) (with-reported-errors (workbench)
                        (xeon.repl:repl-debug-restart (session) (first (event-item e))))))
      (on frames :row-activated
          (lambda (e) (with-reported-errors (workbench)
                        (unless (getf (debug) :post-mortem)
                          (xeon.repl:repl-debug-frame (session) (first (event-item e)))))))
      (on locals :row-activated
          (lambda (e) (with-reported-errors (workbench)
                        (xeon.repl:repl-debug-inspect-local (session) (getf (debug) :selected-frame)
                                                            (getf (event-item e) :id))
                        (select-tool workbench :inspector)))))
    (refresh-debugger tools nil)
    (ui (vbox :padding '(10 12 12 12) :spacing 8
          (:widget (section-label "Debugger"))
          (:widget condition)
          (hbox (:widget abort) (spacer))
          (label :text "Restarts" :text-color :secondary-text :font (make-font :size 11 :weight :bold))
          (:widget restarts :expand t)
          (label :text "Stack frames" :text-color :secondary-text :font (make-font :size 11 :weight :bold))
          (:widget frames :expand t)
          (label :text "Local values · double-click to inspect" :text-color :secondary-text
                 :font (make-font :size 11 :weight :bold))
          (:widget locals :expand t)))))

(defun make-inspector-page (tools)
  (let* ((workbench (tools-workbench tools))
         (expression (make-instance 'entry :text "*" :placeholder "Expression" :control-size :small))
         (inspect (small-button "Inspect"))
         (back (small-button "Back"))
         (refresh (small-button "Refresh"))
         (title (make-instance 'label :text "" :wrap t :selectable t))
         (parts (make-instance 'list-box :label-function
                               (lambda (part) (if (consp part) (format nil "~A" (second part)) (format nil "~A" part))))))
    (setf (tool tools :expression) expression (tool tools :inspect) inspect
          (tool tools :inspect-back) back (tool tools :inspect-refresh) refresh
          (tool tools :inspection-title) title (tool tools :parts) parts)
    (flet ((session () (tools-session tools)))
      (flet ((inspect-expression (e) (declare (ignore e))
               (with-reported-errors (workbench) (xeon.repl:repl-inspect (session) (text expression)))))
        (on inspect :clicked #'inspect-expression)
        (on expression :activate #'inspect-expression))
      (on back :clicked (lambda (e) (declare (ignore e))
                          (with-reported-errors (workbench) (xeon.repl:repl-inspector-action (session) :back))))
      (on refresh :clicked (lambda (e) (declare (ignore e))
                             (with-reported-errors (workbench) (xeon.repl:repl-inspector-action (session) :refresh))))
      (on parts :row-activated
          (lambda (e)
            (let ((part (event-item e)))
              (when (and (consp part) (eq (first part) :value))
                (with-reported-errors (workbench)
                  (xeon.repl:repl-inspector-action (session) :part (third part) (user-data parts))))))))
    (refresh-inspector tools nil nil)
    (ui (vbox :padding '(10 12 12 12) :spacing 8
          (:widget (section-label "Inspector"))
          (hbox :spacing 6 (:widget expression :expand t) (:widget inspect) (:widget back) (:widget refresh))
          (:widget title)
          (:widget parts :expand t)))))

(defun make-tools-panel (workbench)
  (let* ((tools (make-instance 'repl-tools :workbench workbench))
         (stack (make-instance 'stack))
         (switch-row (make-instance 'hbox :spacing 4 :padding '(8 10 4 10) :homogeneous t)))
    (setf (workbench-repl workbench) tools (tools-stack tools) stack)
    (let ((pages (list (make-repl-page tools) (make-problems-page tools)
                       (make-debugger-page tools) (make-inspector-page tools))))
      (loop for (name title glyph) in *tool-pages*
            for page in pages
            do (let ((switch (make-instance 'toggle-button :style :icon :label title :tooltip title
                                                           :image (icon glyph :size 18) :hexpand t))
                     (name name))
                 (add-page stack page title)
                 (add switch-row switch)
                 (push (list name switch page) (tools-switches tools))
                 (on switch :toggled (lambda (e) (declare (ignore e)) (select-tool workbench name))))))
    (setf (tools-switches tools) (nreverse (tools-switches tools)))
    (let ((root (ui (vbox :spacing 0
                      (:widget switch-row)
                      (separator)
                      (:widget stack :expand t)))))
      (setf (tool tools :root) root)
      (let ((backend *backend*))
        (setf (tools-session tools)
              (xeon.repl:make-repl-session
               :on-change (lambda () (let ((*backend* backend)) (notify-tools tools))))))
      (refresh-tools tools)
      root)))

(defun stop-tools (workbench)
  (let ((tools (workbench-repl workbench)))
    (when tools
      (ignore-errors (xeon.repl:stop-repl-session (tools-session tools))))))
