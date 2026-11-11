;;; ui/settings-page.el  -*- lexical-binding: t; -*-

;; Settings: one page for this config's own options

;; Every option of the config that's worth changing, grouped like VS
;; Code's settings: a title, a line saying what it does, and a control
;; on the right (a switch, a pick-one, a − n + stepper or a folder).
;; Changes apply at once and are remembered in ~/.emacs.d/settings.eld
;; (only what differs from the default).  A bar on the left marks what
;; you've changed; r puts one back.
;;
;; Adding a setting is one `my/defsetting' form (bottom of this file).
;;
;; Keys
;; ---------------------------------------------------------------------
;;   SPC t s  open Settings (also the gear icon, and "settings" in search)
;; On the page:
;;   j / k  next / previous     RET or SPC  toggle, pick, or type a value
;;   h / l  or  - / +  step a number down / up
;;   /  filter (empty shows all)          r  reset to default    q  close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function my/tab--mix "tabs")
(declare-function my/tab--face-color "tabs")
(declare-function my/menu "tabs")

(defvar my/settings-file (locate-user-emacs-file "settings.eld")
  "Where changed settings are kept.")

;;; the registry
;; ---------------------------------------------------------------------

(defvar my/settings nil "Every setting, in page order (plists).")

(defun my/defsetting (id &rest props)
  "Declare setting ID.  PROPS:
:section :title :doc        where it shows and what it says
:type     toggle, choice, number, text or directory
:var      the variable it reads and sets (unless :get / :set are given)
:get :set functions to read it and to apply a new value
:choices  a list (or a function returning one) for choice;
          entries are values or (LABEL . VALUE)
:min :max :step   for numbers
:default  the default, when it isn't simply the value at load time"
  (let ((setting (append (list :id id) props)))
    (unless (plist-member setting :default)
      (setq setting (plist-put setting :default (my/settings--get setting))))
    (setq my/settings (append (seq-remove (lambda (s) (eq (plist-get s :id) id)) my/settings)
                              (list setting)))))

(defun my/settings--find (id)
  (seq-find (lambda (s) (eq (plist-get s :id) id)) my/settings))

(defun my/settings--get (setting)
  (cond ((plist-get setting :get) (ignore-errors (funcall (plist-get setting :get))))
        ((plist-get setting :var) (and (boundp (plist-get setting :var))
                                       (symbol-value (plist-get setting :var))))))

(defun my/settings--apply (setting value)
  "Make VALUE live for SETTING."
  (if (plist-get setting :set)
      (funcall (plist-get setting :set) value)
    (set-default (plist-get setting :var) value)))

(defun my/settings--choices (setting)
  "SETTING's choices as (LABEL . VALUE)."
  (let ((choices (plist-get setting :choices)))
    (mapcar (lambda (c) (if (consp c) c (cons (format "%s" c) c)))
            (if (functionp choices) (funcall choices) choices))))

(defun my/settings--label (setting value)
  (pcase (plist-get setting :type)
    ('choice (or (car (rassoc value (my/settings--choices setting))) (format "%s" value)))
    ('directory (abbreviate-file-name (or value "")))
    ('number (if (floatp value) (format "%.2f" value) (format "%s" value)))
    (_ (format "%s" value))))

;;; remembering
;; ---------------------------------------------------------------------

(defun my/settings--saved ()
  (when (file-exists-p my/settings-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my/settings-file)
        (read (current-buffer))))))

(defun my/settings--save (id value)
  "Remember VALUE for ID (forget it when it's the default again)."
  (let* ((setting (my/settings--find id))
         (saved (assq-delete-all id (copy-alist (my/settings--saved)))))
    (unless (equal value (plist-get setting :default))
      (setq saved (append saved (list (cons id value)))))
    (with-temp-file my/settings-file
      (insert ";; changed settings (SPC t s); only what differs from the defaults\n")
      (let ((print-length nil) (print-level nil))
        (pp saved (current-buffer))))))

(defun my/settings-set (id value)
  "Set ID to VALUE now and from now on."
  (let ((setting (my/settings--find id)))
    (my/settings--apply setting value)
    (my/settings--save id value)
    (my/settings--redraw)))

(defun my/settings-apply-saved ()
  "Put back every remembered setting (at startup)."
  (dolist (entry (my/settings--saved))
    (when-let ((setting (my/settings--find (car entry))))
      (condition-case err
          (my/settings--apply setting (cdr entry))
        (error (message "Settings: couldn't apply %s: %s" (car entry) (error-message-string err)))))))

;;; looks
;; ---------------------------------------------------------------------

(defface my/settings-section '((t :weight semibold)) "APPEARANCE, EDITOR…")
(defface my/settings-title '((t)) "A setting's name.")
(defface my/settings-doc '((t)) "What it does.")
(defface my/settings-control '((t)) "The value on the right.")
(defface my/settings-on '((t)) "A switch that's on.")
(defface my/settings-changed '((t)) "The bar beside a changed setting.")
(defface my/settings-hover '((t)) "Under the mouse.")
(defface my/settings-current '((t)) "The setting you're on.")

(defun my/settings-apply-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
           (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
           (accent (my/tab--face-color 'link :foreground "#56a8f5")))
      (set-face-attribute 'my/settings-section nil :foreground (my/tab--mix fg bg 0.55) :height 0.85)
      (set-face-attribute 'my/settings-title nil :foreground (my/tab--mix fg bg 0.92))
      (set-face-attribute 'my/settings-doc nil :foreground (my/tab--mix fg bg 0.45) :height 0.9)
      (set-face-attribute 'my/settings-control nil :foreground fg
                          :background (my/tab--mix fg bg 0.08)
                          :box (list :line-width '(6 . 2) :color (my/tab--mix fg bg 0.08)))
      (set-face-attribute 'my/settings-on nil :foreground (my/tab--mix accent bg 0.9))
      (set-face-attribute 'my/settings-changed nil :background (my/tab--mix accent bg 0.8))
      (set-face-attribute 'my/settings-hover nil :background (my/tab--mix fg bg 0.14))
      (set-face-attribute 'my/settings-current nil :background (my/tab--mix fg bg 0.05) :extend t))))

(defun my/settings--icon (name fallback &optional face)
  (or (ignore-errors
        (let ((icon (nerd-icons-mdicon name)))
          (propertize (substring-no-properties icon)
                      'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                              :height 1.3 ,@(when face (list :inherit face))))))
      fallback))

;;; drawing
;; ---------------------------------------------------------------------

(defvar-local my/settings--filter nil "Only settings matching this show.")

(defvar my/settings-page-width 100 "Widest the page gets, in columns.")

(defvar-local my/settings--margin 0 "Columns left and right of the centred page.")

(defun my/settings--right (string)
  "STRING against the right edge of the page."
  (concat (propertize " " 'display `(space :align-to (- right (,(+ (string-pixel-width string)
                                                                   (* 2 (frame-char-width)))))))
          string))

(defun my/settings--button (text action help)
  (propertize text 'my/settings-click action 'mouse-face 'my/settings-hover 'help-echo help))

(defun my/settings--control (setting value)
  "The control on the right for SETTING showing VALUE."
  (let ((id (plist-get setting :id)))
    (pcase (plist-get setting :type)
      ('toggle
       (my/settings--button
        (if value
            (concat (propertize "on " 'face 'my/settings-doc)
                    (my/settings--icon "nf-md-toggle_switch" "[●]" 'my/settings-on))
          (concat (propertize "off " 'face 'my/settings-doc)
                  (my/settings--icon "nf-md-toggle_switch_off_outline" "[○]" 'my/settings-doc)))
        (lambda () (my/settings-set id (not value)))
        "Click to switch"))
      ('number
       (concat (my/settings--button (propertize " − " 'face 'my/settings-control)
                                    (lambda () (my/settings-step id -1)) "Less")
               " "
               (my/settings--button (propertize (concat " " (my/settings--label setting value) " ")
                                                'face 'my/settings-control)
                                    (lambda () (my/settings-edit id)) "Click to type a value")
               " "
               (my/settings--button (propertize " + " 'face 'my/settings-control)
                                    (lambda () (my/settings-step id 1)) "More")))
      ('choice
       (my/settings--button (propertize (concat " " (my/settings--label setting value) "  ▾ ")
                                        'face 'my/settings-control)
                            (lambda () (my/settings-edit id)) "Click to choose"))
      (_
       (my/settings--button (propertize (concat " " (truncate-string-to-width
                                                     (my/settings--label setting value) 40 nil nil "…")
                                                "  ✎ ")
                                        'face 'my/settings-control)
                            (lambda () (my/settings-edit id)) "Click to change")))))

(defun my/settings--matches (setting)
  (or (null my/settings--filter)
      (string-match-p (regexp-quote (downcase my/settings--filter))
                      (downcase (concat (plist-get setting :section) " " (plist-get setting :title)
                                        " " (or (plist-get setting :doc) ""))))))

(defun my/settings--render ()
  (let* ((inhibit-read-only t)
         (id (get-text-property (point) 'my/settings-id))
         (section nil)
         (window (get-buffer-window (current-buffer)))
         ;; the whole width (margins included), so setting them doesn't feed back
         (width (if window (- (window-total-width window) 2) my/settings-page-width)))
    (setq my/settings--margin (max 0 (/ (- width my/settings-page-width) 2)))
    (setq-local left-margin-width my/settings--margin
                right-margin-width my/settings--margin)
    (when window
      (set-window-margins window my/settings--margin my/settings--margin))
    (erase-buffer)
    (insert "\n  " (propertize "Settings" 'face '(:height 1.6 :weight light)) "\n")
    (insert "  " (propertize (if my/settings--filter
                                 (format "showing \"%s\"   / to change, / then RET to show all" my/settings--filter)
                               "j/k move · RET change · h/l step · / filter · r reset · q close")
                             'face 'my/settings-doc)
            "\n")
    (dolist (setting (seq-filter #'my/settings--matches my/settings))
      (let* ((value (my/settings--get setting))
             (changed (not (equal value (plist-get setting :default))))
             (bar (if changed (propertize " " 'face 'my/settings-changed
                                          'display '(space :width (3)))
                    (propertize " " 'display '(space :width (3))))))
        (unless (equal section (plist-get setting :section))
          (setq section (plist-get setting :section))
          (insert "\n\n  " (propertize (upcase section) 'face 'my/settings-section) "\n"))
        (let ((start (point)))
          (insert "\n" bar " "
                  (propertize (plist-get setting :title) 'face 'my/settings-title)
                  (my/settings--right (my/settings--control setting value))
                  "\n" bar " "
                  (propertize (or (plist-get setting :doc) "") 'face 'my/settings-doc)
                  (if changed
                      (concat "  " (my/settings--button
                                    (propertize "reset" 'face '(:inherit my/settings-doc :underline t))
                                    (let ((sid (plist-get setting :id))) (lambda () (my/settings-reset sid)))
                                    "Back to the default"))
                    "")
                  "\n")
          (put-text-property start (point) 'my/settings-id (plist-get setting :id)))))
    (insert "\n")
    (goto-char (point-min))
    (if-let ((pos (and id (text-property-any (point-min) (point-max) 'my/settings-id id))))
        (progn (goto-char pos) (forward-line 1))
      (my/settings-next))
    (my/settings--highlight)))

(defvar-local my/settings--overlay nil)

(defun my/settings--highlight ()
  "Tint the setting point is on."
  (let ((id (get-text-property (point) 'my/settings-id)))
    (unless my/settings--overlay
      (setq my/settings--overlay (make-overlay 1 1))
      (overlay-put my/settings--overlay 'face 'my/settings-current))
    (if (not id)
        (move-overlay my/settings--overlay 1 1)
      (let ((start (or (previous-single-property-change (1+ (point)) 'my/settings-id) (point-min)))
            (end (or (next-single-property-change (point) 'my/settings-id) (point-max))))
        (move-overlay my/settings--overlay start end)))))

(defun my/settings--redraw ()
  (when-let ((buffer (get-buffer "*Settings*")))
    (with-current-buffer buffer
      (let ((start (and (get-buffer-window buffer) (window-start (get-buffer-window buffer)))))
        (my/settings--render)
        (when start (set-window-start (get-buffer-window buffer) start t))))))

;;; changing things
;; ---------------------------------------------------------------------

(defun my/settings--here ()
  (or (my/settings--find (get-text-property (point) 'my/settings-id))
      (user-error "Not on a setting")))

(defun my/settings--read (setting)
  "Ask for a new value for SETTING."
  (let ((value (my/settings--get setting))
        (title (plist-get setting :title)))
    (pcase (plist-get setting :type)
      ('toggle (not value))
      ('choice
       (let* ((choices (my/settings--choices setting))
              (pick (completing-read (format "%s: " title) (mapcar #'car choices) nil t nil nil
                                     (my/settings--label setting value))))
         (cdr (assoc pick choices))))
      ('number
       (let ((n (read-number (format "%s (%s–%s): " title (plist-get setting :min) (plist-get setting :max))
                             value)))
         (my/settings--clamp setting n)))
      ('directory (file-name-as-directory (read-directory-name (format "%s: " title) value nil t)))
      (_ (read-string (format "%s: " title) (format "%s" (or value "")))))))

(defun my/settings--clamp (setting n)
  (let ((min (plist-get setting :min)) (max (plist-get setting :max)))
    (when min (setq n (max min n)))
    (when max (setq n (min max n)))
    n))

(defun my/settings-edit (&optional id)
  "Change the setting at point (or ID)."
  (interactive)
  (let* ((setting (if id (my/settings--find id) (my/settings--here)))
         (read (lambda () (my/settings--read setting))))
    (my/settings-set (plist-get setting :id)
                     (if (and (fboundp 'my/with-vertico) (memq (plist-get setting :type) '(choice)))
                         (let (result)
                           (my/with-vertico (lambda () (interactive) (setq result (funcall read))))
                           result)
                       (funcall read)))))

(defun my/settings-step (&optional id direction)
  "Step the number at point (or ID) by its :step, DIRECTION 1 or -1."
  (interactive)
  (let* ((setting (if id (my/settings--find id) (my/settings--here)))
         (direction (or direction 1)))
    (unless (eq (plist-get setting :type) 'number) (user-error "Not a number"))
    (let* ((step (or (plist-get setting :step) 1))
           (raw (+ (my/settings--get setting) (* direction step)))
           ;; whole-number steps snap to the step (113 -> 120, not 123)
           (snapped (if (integerp step) (* step (round raw step)) raw))
           (new (my/settings--clamp setting snapped)))
      (my/settings-set (plist-get setting :id)
                       (if (floatp step) (/ (round (* new 100)) 100.0) new)))))

(defun my/settings-up () (interactive) (my/settings-step nil 1))
(defun my/settings-down () (interactive) (my/settings-step nil -1))

(defun my/settings-activate ()
  "Toggle a switch, or change the value at point."
  (interactive)
  (let ((setting (my/settings--here)))
    (if (eq (plist-get setting :type) 'toggle)
        (my/settings-set (plist-get setting :id) (not (my/settings--get setting)))
      (my/settings-edit))))

(defun my/settings-reset (&optional id)
  "Put the setting at point (or ID) back to its default."
  (interactive)
  (let ((setting (if id (my/settings--find id) (my/settings--here))))
    (my/settings-set (plist-get setting :id) (plist-get setting :default))))

(defun my/settings-filter (text)
  "Show only settings matching TEXT (empty shows all)."
  (interactive (list (read-string "Filter settings: " my/settings--filter)))
  (setq my/settings--filter (unless (string-empty-p (string-trim text)) (string-trim text)))
  (my/settings--render))

(defun my/settings-next ()
  (interactive)
  (let ((id (get-text-property (point) 'my/settings-id)))
    (let ((next (next-single-property-change (point) 'my/settings-id)))
      (while (and next (or (null (get-text-property next 'my/settings-id))
                           (eq (get-text-property next 'my/settings-id) id)))
        (setq next (next-single-property-change next 'my/settings-id)))
      (when next (goto-char next) (forward-line 1))))
  (my/settings--highlight))

(defun my/settings-previous ()
  (interactive)
  (let ((id (get-text-property (point) 'my/settings-id))
        (pos (point)))
    ;; back to this setting's start, then to the previous one's
    (setq pos (or (previous-single-property-change pos 'my/settings-id) (point-min)))
    (while (and (> pos (point-min))
                (or (null (get-text-property (1- pos) 'my/settings-id))
                    (eq (get-text-property (1- pos) 'my/settings-id) id)))
      (setq pos (or (previous-single-property-change pos 'my/settings-id) (point-min))))
    (when (> pos (point-min))
      (goto-char (1- pos))
      (goto-char (or (previous-single-property-change (point) 'my/settings-id) (point-min)))
      (forward-line 1)))
  (my/settings--highlight))

(defun my/settings-click (event)
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (select-window (posn-window (event-start event)))
    (when pos
      (goto-char pos)
      (my/settings--highlight)
      (when-let ((action (get-text-property pos 'my/settings-click)))
        (funcall action)))))

(defvar my/settings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'my/settings-click)
    map))

(define-derived-mode my/settings-mode special-mode "Settings"
  "This config's settings."
  (setq-local truncate-lines t
              cursor-type nil
              evil-normal-state-cursor '(nil)
              line-spacing 0.2)
  (display-line-numbers-mode -1)
  (add-hook 'post-command-hook #'my/settings--highlight nil t)
  (add-hook 'window-size-change-functions
            (lambda (w) (with-current-buffer (window-buffer w)
                          (when (derived-mode-p 'my/settings-mode) (my/settings--redraw))))
            nil t))

(defun my/settings-open ()
  "Open Settings in a tab of its own."
  (interactive)
  (let ((index (seq-position (mapcar (lambda (tab) (alist-get 'name tab)) (funcall tab-bar-tabs-function))
                             "Settings")))
    (if index
        (tab-bar-select-tab (1+ index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Settings"))
    (when (fboundp 'my/session--leave-side-window) (my/session--leave-side-window))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*Settings*"))
    (unless (derived-mode-p 'my/settings-mode) (my/settings-mode))
    (my/settings--render)))

(defun my/settings-quit ()
  (interactive)
  (if (> (length (funcall tab-bar-tabs-function)) 1) (tab-bar-close-tab) (quit-window)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/settings-mode 'normal)
  (evil-define-key 'normal my/settings-mode-map
    "j" #'my/settings-next
    "k" #'my/settings-previous
    (kbd "RET") #'my/settings-activate
    (kbd "<return>") #'my/settings-activate
    (kbd "SPC") #'my/settings-activate
    "l" #'my/settings-up
    "+" #'my/settings-up
    "=" #'my/settings-up
    "h" #'my/settings-down
    "-" #'my/settings-down
    "/" #'my/settings-filter
    "r" #'my/settings-reset
    "q" #'my/settings-quit
    [down-mouse-1] #'ignore
    [mouse-1] #'my/settings-click)
  (evil-define-key 'normal 'global
    (kbd "<leader>ts") #'my/settings-open))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC t s" "settings"))

;;; the settings
;; ---------------------------------------------------------------------

(defun my/settings--mono-fonts ()
  "Installed fixed-width font families, the usual coding ones first."
  (let ((families (delete-dups (seq-filter (lambda (f) (string-match-p "Mono\\|Code\\|Consol\\|Courier\\|Hack\\|Iosevka\\|Menlo" f))
                                           (font-family-list)))))
    (sort families #'string<)))

;; appearance

(my/defsetting 'theme
  :section "Appearance" :title "Theme" :type 'choice
  :doc "The colours of everything."
  :choices (lambda () (and (boundp 'my/themes) my/themes))
  :get (lambda () (car custom-enabled-themes))
  :set (lambda (theme) (when theme (my/load-theme theme))))

(my/defsetting 'font
  :section "Appearance" :title "Font" :type 'choice
  :doc "The font for code and everything else."
  :choices #'my/settings--mono-fonts
  :get (lambda () (face-attribute 'default :family))
  :set (lambda (family) (set-face-attribute 'default nil :family family)))

(my/defsetting 'font-size
  :section "Appearance" :title "Font size" :type 'number :min 70 :max 240 :step 10
  :doc "Text size (in tenths of a point: 110 is 11pt)."
  :get (lambda () (face-attribute 'default :height))
  :set (lambda (height) (set-face-attribute 'default nil :height height)))

(my/defsetting 'line-spacing
  :section "Appearance" :title "Line spacing" :type 'number :min 0.0 :max 0.6 :step 0.05
  :doc "Extra space between lines, as a share of a line."
  :get (lambda () (or (default-value 'line-spacing) 0.0))
  :set (lambda (v) (setq-default line-spacing (if (zerop v) nil v))))

(my/defsetting 'line-numbers
  :section "Appearance" :title "Line numbers" :type 'toggle
  :doc "Numbers down the side of code."
  :get (lambda () (bound-and-true-p global-display-line-numbers-mode))
  :set (lambda (on) (global-display-line-numbers-mode (if on 1 -1))))

(my/defsetting 'relative-line-numbers
  :section "Appearance" :title "Relative line numbers" :type 'toggle
  :doc "Count lines from the cursor (handy for 5j, 3dd…)."
  :get (lambda () (eq (default-value 'display-line-numbers-type) 'relative))
  :set (lambda (on)
         (setq-default display-line-numbers-type (if on 'relative t))
         (dolist (b (buffer-list))
           (with-current-buffer b
             (when (bound-and-true-p display-line-numbers-mode)
               (setq display-line-numbers (if on 'relative t)))))))

(my/defsetting 'activity-bar
  :section "Appearance" :title "Activity bar" :type 'toggle :default t
  :doc "The column of icons on the left."
  :get (lambda () (bound-and-true-p my/activity-bar-mode))
  :set (lambda (on) (when (fboundp 'my/activity-bar-mode) (my/activity-bar-mode (if on 1 -1)))))

;; one switch per activity bar icon
(when (boundp 'my/activity-items)
  (dolist (item (seq-filter #'consp my/activity-items))
    (let ((id (car item)) (label (nth 1 item)))
      (unless (eq id 'menu)
        (my/defsetting (intern (format "activity-icon-%s" id))
          :section "Appearance" :title (format "Bar icon: %s" label) :type 'toggle
          :doc (format "Show %s on the activity bar." label)
          :get (lambda () (not (memq id (bound-and-true-p my/activity-hidden))))
          :set (lambda (on)
                 (unless (eq on (not (memq id my/activity-hidden)))
                   (my/activity-toggle-icon id))))))))

;; editor

(my/defsetting 'tab-width
  :section "Editor" :title "Indent width" :type 'number :min 2 :max 8 :step 1
  :doc "Spaces per indent level (also what > and < shift by)."
  :get (lambda () (default-value 'tab-width))
  :set (lambda (n) (setq-default tab-width n evil-shift-width n)))

(my/defsetting 'wrap-lines
  :section "Editor" :title "Wrap long lines" :type 'toggle
  :doc "Long lines wrap onto the next line instead of running off the edge."
  :get (lambda () (not (default-value 'truncate-lines)))
  :set (lambda (on) (setq-default truncate-lines (not on))))

(my/defsetting 'code-prefetch
  :section "Editor" :title "Ask for code actions ahead" :type 'toggle
  :doc "Fetch fixes where the cursor or mouse rests, so right-click opens at once."
  :get (lambda () (numberp (bound-and-true-p my/code-prefetch-delay)))
  :set (lambda (on) (setq my/code-prefetch-delay (and on 0.2))))

(my/defsetting 'hover-docs
  :section "Editor" :title "Hover docs" :type 'toggle :default t
  :doc "Rest the mouse on a symbol to see its signature and docs."
  :get (lambda () (if (boundp 'lsp-ui-doc-show-with-mouse) lsp-ui-doc-show-with-mouse t))
  :set (lambda (on) (when (fboundp 'my/hover-set-mouse) (my/hover-set-mouse on))))

(my/defsetting 'hover-delay
  :section "Editor" :title "Hover delay (seconds)" :type 'number :min 0.1 :max 2.0 :step 0.1 :default 0.5
  :doc "How long the mouse rests before the docs show."
  :get (lambda () (if (boundp 'lsp-ui-doc-delay) lsp-ui-doc-delay 0.5))
  :set (lambda (v) (setq lsp-ui-doc-delay v)))

(my/defsetting 'code-menu-actions
  :section "Editor" :title "Actions in the right-click menu" :type 'number :min 3 :max 30 :step 1
  :doc "How many code actions show before the rest go under More."
  :var 'my/code-menu-max-actions)

;; source control

(my/defsetting 'git-interval
  :section "Source Control" :title "Refresh every (seconds)" :type 'number :min 2 :max 60 :step 1
  :doc "How often an open Source Control panel checks for changes."
  :var 'my/git-interval)

(my/defsetting 'git-recent
  :section "Source Control" :title "Recent commits shown" :type 'number :min 0 :max 50 :step 1
  :doc "Commits listed at the bottom of the panel."
  :var 'my/git-recent-count)

;; sessions and drives

(my/defsetting 'session-picker
  :section "Sessions" :title "Ask which session to open at startup" :type 'toggle
  :doc "Off: start straight on the Welcome page."
  :var 'my/session-startup-picker)

;; containers

(my/defsetting 'container-engine
  :section "Containers" :title "Engine" :type 'choice
  :doc "What runs containers. Automatic uses docker when its daemon is up, else podman."
  :choices '(("Automatic" . nil) ("podman" . "podman") ("docker" . "docker"))
  :get (lambda () (bound-and-true-p my/containers-engine))
  :set (lambda (engine)
         (setq my/containers-engine engine)
         (when (boundp 'my/containers--engine) (setq my/containers--engine nil))))

(my/defsetting 'container-panel-width
  :section "Containers" :title "Panel width" :type 'number :min 30 :max 90 :step 2
  :doc "Columns the containers panel takes beside your code."
  :var 'my/containers-panel-width)

;; music

(my/defsetting 'music-folder
  :section "Music" :title "Music folder" :type 'directory
  :doc "Where your library lives (on the pub drive by default)."
  :var 'my/music-directory)

(my/defsetting 'volume-step
  :section "Music" :title "Volume step" :type 'number :min 1 :max 25 :step 1
  :doc "How much louder or quieter one press makes it."
  :var 'my/music-volume-step)

(my/defsetting 'lyrics-width
  :section "Music" :title "Lyrics width" :type 'number :min 0.2 :max 0.7 :step 0.05
  :doc "Share of the window the docked lyrics take."
  :var 'my/music-lyrics-width)

;; startup: put back what was changed
(my/settings-apply-saved)

(my/settings-apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/settings-apply-theme))

(provide 'settings-page)

;;; settings-page.el ends here
