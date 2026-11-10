;;; ui/activity-bar.el  -*- lexical-binding: t; -*-

;; the activity bar: a strip of icons down the left, like VS Code's

;; Every tab gets a thin column of VS Code's own icons (codicons, from the
;; Symbols Nerd Font) on its far left, and every pane is boxed in by a
;; light 1px line.  Click one to
;; open or close what it stands for; the icon of whatever is open lights
;; up, with a small accent bar beside it.  Hovering says what it is and
;; its key.
;;
;;   top:     File tree · Search · Git (changed files) · Run · Containers
;;            · Terminal
;;   bottom:  Music · Menu (everything, same as clicking NORMAL)
;;
;; The tree and the Git view share the spot beside the bar, like VS
;; Code's sidebar: opening one closes the other.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC t a  hide / show the bar
;;   right-click the bar: tick / untick which icons it shows (remembered)
;;   SPC g s  the Git view (changed files)
;;   SPC t m  move the panel you're in to its own tab, and back again
;;            (also the ⤢ / ⤡ buttons on a panel's title bar)

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabs)
(require 'nerd-icons nil t)

(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function neo-global--window-exists-p "neotree")
(declare-function evil-define-key "evil-core")

(defvar neo-window-width)
(defvar neo-display-action)

(defvar my/activity-buffer " *activity bar*")

(defvar my/activity-width 6
  "Width of the bar, in columns (about VS Code's 48 pixels).")

(defvar my/activity-row 44
  "Height of each icon's row, in pixels.")

(defvar my/activity-icon-height 1.45
  "Icon size relative to the default font.")

;;; layout: side panels run full height, the tree sits beside the bar
;; ---------------------------------------------------------------------

;; left/right panels (this bar, containers) take the whole height and the
;; bottom ones (Run, terminal) sit under the editor area, like VS Code
(setq window-sides-vertical t)

(defun my/activity--sidebar-display (buffer _alist)
  "Show BUFFER in a sidebar window: the far left of the editor area."
  ;; the tree and the code become one permanent block, so closing a
  ;; bottom panel can't dissolve it (the panel would come back under
  ;; the tree alone)
  ;; the bar first, so the tree lands to its right
  (my/activity--now)
  (let ((window (let ((window-combination-limit t))
                  (split-window (window-main-window) (- (or (bound-and-true-p neo-window-width) 35))
                                'left))))
    (set-window-buffer window buffer)
    (window-preserve-size window t t)
    window))

;; NeoTree opens beside the bar rather than as a side window (side
;; windows on the same side would stack top to bottom)
(with-eval-after-load 'neotree
  (setq neo-display-action '(my/activity--sidebar-display)))

(defun my/activity--sidebar-p (window)
  "Non-nil if WINDOW is the tree or the Git view beside the bar."
  (and (window-live-p window)
       (or (window-parameter window 'my-activity-git)
           (eq (buffer-local-value 'major-mode (window-buffer window)) 'neotree-mode))))

(defun my/activity--editor-window ()
  "The code window to use instead of the sidebar."
  (or (let ((right (window-in-direction 'right (selected-window))))
        (and right (not (window-parameter right 'window-side)) right))
      (seq-find (lambda (w) (not (or (window-parameter w 'window-side)
                                     (my/activity--sidebar-p w))))
                (window-list nil 'nomini))))

(defun my/activity--not-in-sidebar (fn &rest args)
  "Run FN (`display-buffer') from the code area when the sidebar has focus,
so nothing opens by splitting the tree."
  (let ((editor (and (my/activity--sidebar-p (selected-window))
                     (my/activity--editor-window))))
    (if editor
        (with-selected-window editor (apply fn args))
      (apply fn args))))

(advice-add 'display-buffer :around #'my/activity--not-in-sidebar)

;;; what each icon does
;; ---------------------------------------------------------------------

(defun my/activity--window-with (param)
  (seq-find (lambda (w) (window-parameter w param)) (window-list nil 'nomini)))

;; the git view

(defun my/activity-git-p ()
  (my/activity--window-with 'my-activity-git))

(defun my/activity-git ()
  "Show or hide the changed files of the project (vc-dir), beside the bar."
  (interactive)
  (if-let ((window (my/activity-git-p)))
      (delete-window window)
    (let ((root (or (and (fboundp 'my/project-root) (my/project-root))
                    (and (fboundp 'vc-root-dir) (vc-root-dir))
                    (user-error "Not in a project or repository"))))
      (when (my/activity-explorer-p) (my/activity-explorer))
      (let* ((buffer (save-window-excursion (vc-dir root) (current-buffer)))
             (window (my/activity--sidebar-display buffer nil)))
        (set-window-parameter window 'my-activity-git t)
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-dedicated-p window t)
        (select-window window)))))

;; the tree

(defun my/activity-explorer-p ()
  (and (fboundp 'neo-global--window-exists-p) (neo-global--window-exists-p)))

(defun my/activity-explorer ()
  "Show or hide the project's file tree (closing the Git view)."
  (interactive)
  (unless (my/activity-explorer-p)
    (when-let ((git (my/activity-git-p))) (delete-window git)))
  (if (fboundp 'my/neotree-toggle)
      (my/neotree-toggle)
    (user-error "NeoTree isn't set up")))

;; problems

(defun my/activity-problems-p ()
  (get-buffer-window "*Flycheck errors*"))

(defun my/activity-problems ()
  (interactive)
  (if-let ((window (my/activity-problems-p)))
      (delete-window window)
    (if (bound-and-true-p flycheck-mode)
        (flycheck-list-errors)
      (user-error "No checker in this buffer"))))

;; terminal, at the bottom of the tab

(defun my/activity-terminal-p ()
  (my/activity--window-with 'my-activity-terminal))

(defun my/activity-terminal ()
  "Show or hide a terminal at the bottom, in the project's folder."
  (interactive)
  (if-let ((window (my/activity-terminal-p)))
      (delete-window window)
    (let* ((dir (or (and (fboundp 'my/project-root) (my/project-root)) default-directory))
           (buffer (or (seq-find (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'ghostel-mode)
                                          (process-live-p (get-buffer-process b))
                                          (equal (expand-file-name default-directory)
                                                 (expand-file-name dir)))))
                                 (buffer-list))
                       (let ((default-directory dir))
                         (save-window-excursion
                           (call-interactively (if (fboundp 'ghostel) #'ghostel #'shell))
                           (current-buffer)))))
           (window (display-buffer-in-side-window
                    buffer '((side . bottom) (slot . 2) (window-height . 0.3)))))
      (set-window-parameter window 'my-activity-terminal t)
      (select-window window))))

;; the rest are the panels that already exist

(defun my/activity-run-p ()
  (and (fboundp 'my/run-panel--window) (my/run-panel--window)))

(defun my/activity-containers-p ()
  (and (fboundp 'my/containers--panel-window) (my/containers--panel-window)))

(defun my/activity-music-p ()
  (equal (alist-get 'name (cdr (tab-bar--current-tab))) "Music"))

(defun my/activity-menu ()
  "Everything (the same menu as clicking NORMAL in the mode line)."
  (interactive)
  (popup-menu (my/menu "" (if (fboundp 'my/ml--main-items) (my/ml--main-items) nil))))

(defvar my/activity-items
  '((explorer   "Explorer"          "SPC e e"   (cod "nf-cod-files")          my/activity-explorer   my/activity-explorer-p)
    (search     "Search in project" "SPC p SPC" (cod "nf-cod-search")         my/search-project-popup nil)
    (git        "Source control"    "SPC g s"   (cod "nf-cod-source_control") my/activity-git        my/activity-git-p)
    (run        "Run"               "SPC r p"   (cod "nf-cod-debug_alt")      my/run-panel-toggle    my/activity-run-p)
    (containers "Containers"        "SPC d d"   (md "nf-md-docker")           my/containers-panel-toggle my/activity-containers-p)
    (problems   "Problems"          "SPC x w"   (cod "nf-cod-warning")        my/activity-problems   my/activity-problems-p)
    (terminal   "Terminal"          "SPC v t"   (cod "nf-cod-terminal")       my/activity-terminal   my/activity-terminal-p)
    bottom
    (music      "Music"             "SPC m m"   (cod "nf-cod-music")          my/music               my/activity-music-p)
    (menu       "Menu"              nil         (cod "nf-cod-settings_gear")  my/activity-menu       nil))
  "The icons, top to bottom: (ID LABEL KEY (FAMILY NAME) COMMAND ACTIVE-P).
`bottom' starts the group pinned to the bottom edge.")

;;; drawing
;; ---------------------------------------------------------------------

(defface my/activity-icon '((t)) "An icon on the bar.")
(defface my/activity-active '((t)) "The icon of what's open.")
(defface my/activity-marker '((t)) "The accent beside the open icon.")
(defface my/activity-hover '((t)) "An icon under the mouse.")

(defvar my/activity--bg nil)

;;; which icons show (right-click the bar)
;; ---------------------------------------------------------------------

(defvar my/activity-state-file (locate-user-emacs-file "activity-bar.eld")
  "Where the hidden icons are remembered.")

(defvar my/activity-hidden '(problems)
  "Icons not shown on the bar.")

(defun my/activity--load-hidden ()
  (when (file-exists-p my/activity-state-file)
    (let ((hidden (ignore-errors
                    (with-temp-buffer (insert-file-contents my/activity-state-file)
                                      (read (current-buffer))))))
      (when (listp hidden) (setq my/activity-hidden hidden)))))

(defun my/activity-toggle-icon (id)
  "Show or hide the icon ID on the bar."
  (interactive
   (list (intern (completing-read "Show or hide: "
                                  (mapcar (lambda (item) (symbol-name (car item)))
                                          (seq-filter #'consp my/activity-items))
                                  nil t))))
  (setq my/activity-hidden (if (memq id my/activity-hidden)
                               (delq id my/activity-hidden)
                             (cons id my/activity-hidden)))
  (with-temp-file my/activity-state-file (prin1 my/activity-hidden (current-buffer)))
  (my/activity-render))

(defun my/activity-icons-menu (&optional event)
  "A checklist of the bar's icons; untick to hide one (VS Code's right-click)."
  (interactive "e")
  (popup-menu
   (my/menu "Activity bar"
            (append
             (mapcar (lambda (item)
                       (let ((id (car item)))
                         `(,(nth 1 item) ,(lambda () (interactive) (my/activity-toggle-icon id))
                           :button (:toggle . (not (memq ',id my/activity-hidden))))))
                     (seq-filter #'consp my/activity-items))
             '("--" ("Hide the bar" my/activity-bar-mode))))
   event))

(defun my/activity--shown (items)
  (seq-remove (lambda (item) (and (consp item) (memq (car item) my/activity-hidden))) items))

(defun my/activity-apply-theme (&rest _)
  (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
         (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
         (dark (< (my/tab--luma bg) 0.5))
         (accent (or (my/tab--face-color 'cursor :background nil)
                     (my/tab--face-color 'link :foreground "#3574f0"))))
    (setq my/activity--bg (my/tab--mix "#000000" bg (if dark 0.16 0.04)))
    (set-face-attribute 'my/activity-icon nil :foreground (my/tab--mix fg bg 0.42))
    (set-face-attribute 'my/activity-active nil :foreground fg)
    (ignore accent)
    (set-face-attribute 'my/activity-marker nil :background (my/tab--mix fg bg 0.85))
    (set-face-attribute 'my/activity-hover nil :foreground fg)
    ;; light 1px lines between every pane, so each one reads as a box
    (let ((line (my/tab--mix fg bg (if dark 0.16 0.20))))
      (dolist (face '(window-divider window-divider-first-pixel window-divider-last-pixel))
        (set-face-attribute face nil :foreground line))
      (set-face-attribute 'vertical-border nil :foreground line))
    (when-let ((buf (get-buffer my/activity-buffer)))
      (with-current-buffer buf (my/activity--style)))))

(defun my/activity--icon (spec)
  (or (ignore-errors
        (pcase (car spec)
          ('cod (nerd-icons-codicon (cadr spec) :height my/activity-icon-height))
          ('md (nerd-icons-mdicon (cadr spec) :height my/activity-icon-height))))
      "•"))

(defun my/activity--line (item window-px)
  "One row of the bar: the marker, then ITEM's icon centred in WINDOW-PX."
  (pcase-let* ((`(,id ,label ,key ,icon ,_command ,active-p) item)
               (active (and active-p (fboundp active-p) (ignore-errors (funcall active-p))))
               (glyph (my/activity--icon icon))
               (family (or (plist-get (get-text-property 0 'face glyph) :family)
                           "Symbols Nerd Font Mono"))
               (face (list :family family :height my/activity-icon-height
                           :inherit (if active 'my/activity-active 'my/activity-icon)))
               (glyph (propertize (substring-no-properties glyph) 'face face))
               (glyph-px (string-pixel-width glyph))
               (marker-px 2))
    ;; the whole row (not just the glyph) belongs to the icon, so a click
    ;; anywhere on it does the right thing
    (propertize
     (concat
      ;; VS Code's accent: a 2px bar down the left edge of the open one;
      ;; it also sets the row's height, with the icon centred in it
      (propertize " " 'display `(space :width (,marker-px) :height (,my/activity-row) :ascent 62)
                  'face (if active 'my/activity-marker 'default))
      (propertize " " 'display `(space :align-to (,(max marker-px (/ (- window-px glyph-px) 2)))))
      (propertize glyph 'mouse-face 'my/activity-hover)
      "\n")
     'help-echo (if key (format "%s  (%s)" label key) label)
     'my/activity id)))

(defun my/activity--style ()
  (setq-local face-remapping-alist
              `((default :background ,(or my/activity--bg 'unspecified))
                (fringe :background ,(or my/activity--bg 'unspecified)))))

(defun my/activity-render ()
  "Redraw the bar (lights follow what's open in the selected tab)."
  (when-let ((buf (get-buffer my/activity-buffer)))
    (let* ((window (get-buffer-window buf))
           (window-px (if window (window-body-width window t) 48))
           (height-px (if window (window-body-height window t) 800))
           (items (my/activity--shown my/activity-items))
           (top (seq-take-while (lambda (i) (not (eq i 'bottom))) items))
           (bottom (cdr (memq 'bottom items))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (item top) (insert (my/activity--line item window-px)))
          ;; one tall empty line pushes the bottom group to the bottom edge
          (let ((gap (- height-px (* my/activity-row (+ (length top) (length bottom))) 4)))
            (when (> gap 0)
              (insert (propertize " " 'display `(space :height (,gap))) "\n")))
          (dolist (item bottom) (insert (my/activity--line item window-px)))
          (goto-char (point-min)))))))

;;; clicking
;; ---------------------------------------------------------------------

(defun my/activity-click (event)
  "Run the icon clicked in EVENT, without moving focus to the bar."
  (interactive "e")
  (let* ((posn (event-start event))
         (id (and (posn-point posn)
                  (with-current-buffer (window-buffer (posn-window posn))
                    (get-text-property (posn-point posn) 'my/activity))))
         (item (assq id (seq-filter #'consp my/activity-items))))
    (when item
      (let ((command (nth 4 item)))
        (if (fboundp command)
            ;; run as itself, so it gets the same popup as from its key
            ;; (the popup style is picked by the command's name)
            (progn (setq this-command command
                         real-this-command command)
                   (call-interactively command))
          (message "%s isn't available" (nth 1 item))))
      (my/activity-render))))

(defvar-keymap my/activity-map
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'my/activity-click
  "<double-mouse-1>" #'ignore
  "<down-mouse-3>" #'my/activity-icons-menu
  "<mouse-3>" #'ignore
  "<wheel-up>" #'ignore
  "<wheel-down>" #'ignore)

(define-derived-mode my/activity-mode special-mode "Activity"
  "The activity bar."
  (setq-local mode-line-format nil
              header-line-format nil
              cursor-type nil
              truncate-lines t
              line-spacing nil
              cursor-in-non-selected-windows nil
              window-size-fixed 'width)
  (display-line-numbers-mode -1)
  (my/activity--style)
  (use-local-map my/activity-map))

;;; panels: one look for every box
;; ---------------------------------------------------------------------
;; Explorer, Source Control, Run, Terminal, Containers and Problems all
;; get the same title bar (small caps, buttons on the right) and no mode
;; line of their own; only code windows keep a status bar.  The title
;; bar's buttons move a panel into a tab of its own and back again.

(defface my/panel-title '((t :weight semibold)) "A panel's title.")
(defface my/panel-hint '((t)) "Key hints in a panel's title bar.")
(defface my/panel-button '((t)) "A button in a panel's title bar.")

(defun my/panel--apply-theme (&rest _)
  (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
         (fg (my/tab--face-color 'default :foreground "#dfe1e5")))
    (set-face-attribute 'my/panel-title nil :foreground (my/tab--mix fg bg 0.62) :height 0.85)
    (set-face-attribute 'my/panel-hint nil :foreground (my/tab--mix fg bg 0.40) :height 0.85)
    (set-face-attribute 'my/panel-button nil :foreground (my/tab--mix fg bg 0.55))
    ;; title bars are flat: the panel's own background, nothing boxed
    (set-face-attribute 'header-line nil :background bg :foreground fg
                        :box nil :underline nil :overline nil :inherit nil)))

(defun my/panel-kind (window)
  "What kind of panel WINDOW is, or nil for an ordinary window."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (cond ((window-parameter window 'my-activity-bar) nil)
            ((derived-mode-p 'neotree-mode) 'explorer)
            ((window-parameter window 'my-activity-git) 'git)
            ((window-parameter window 'my-activity-terminal) 'terminal)
            ((window-parameter window 'my-run-panel) 'run)
            ((window-parameter window 'my-containers-panel) 'containers)
            ((derived-mode-p 'flycheck-error-list-mode) 'problems)))))

(defun my/panel--title (kind)
  (pcase kind
    ('explorer "EXPLORER") ('git "SOURCE CONTROL") ('terminal "TERMINAL")
    ('run "RUN") ('containers "CONTAINERS") ('problems "PROBLEMS")))

(defun my/panel--hints (buffer)
  "BUFFER's own key hints (its header line, minus its title), if it has some."
  (with-current-buffer buffer
    (when header-line-format
      (let ((text (string-trim (format-mode-line header-line-format nil nil buffer))))
        (string-trim (replace-regexp-in-string "\\`\\(Run · [^ ]+\\|Containers\\)" "" text))))))

(defun my/panel--icon (name fallback)
  (or (ignore-errors (nerd-icons-codicon name)) fallback))

(defun my/panel--button (icon help command)
  "ICON (a nerd-icons string) as a title-bar button running COMMAND on the window."
  (let ((family (plist-get (get-text-property 0 'face icon) :family)))
    (propertize (concat " "
                        (propertize (substring-no-properties icon)
                                    'face (list :family (or family 'unspecified) :height 0.9
                                                :inherit 'my/panel-button))
                        " ")
                'mouse-face 'my/activity-hover
                'help-echo help
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [header-line mouse-1]
                                         (lambda (event) (interactive "e")
                                           (funcall command (posn-window (event-start event)))))
                             map))))

(defun my/panel--header (window kind)
  (let ((hints (unless (eq kind 'terminal) (my/panel--hints (window-buffer window)))))
    `(" " ,(propertize (my/panel--title kind) 'face 'my/panel-title)
      ,(if (and hints (not (string-empty-p hints)))
           ;; trimmed so the buttons always fit
           (concat "   " (propertize (truncate-string-to-width
                                      hints (max 0 (- (window-total-width window)
                                                      (length (my/panel--title kind)) 16))
                                      nil nil "…")
                                     'face 'my/panel-hint))
         "")
      ,@(my/panel--right
         (concat (my/panel--button (my/panel--icon "nf-cod-link_external" "⤢")
                                   "Move to its own tab  (SPC t m)" #'my/panel-to-tab)
                 (my/panel--button (my/panel--icon "nf-cod-close" "✕") "Close" #'my/panel-close))))))

(defun my/panel--right (buttons)
  "BUTTONS pushed flush against the right edge (measured in pixels)."
  (list (propertize " " 'display `(space :align-to (- right (,(string-pixel-width buttons)))))
        buttons))

(defun my/panel--tab-header (window)
  `(" " ,(propertize (or (my/panel--title (buffer-local-value 'my/panel--kind (window-buffer window))) "")
                     'face 'my/panel-title)
    ,@(my/panel--right
       (my/panel--button (my/panel--icon "nf-cod-layout_panel" "⤡")
                         "Dock back into the tab it came from  (SPC t m)" #'my/panel-back))))

(defvar-local my/panel--kind nil "The panel this buffer was, while it has a tab of its own.")
(defvar-local my/panel--home nil "Id of the tab it came from.")

(defun my/panel-decorate ()
  "Give every panel in the selected frame its title bar and no mode line."
  (dolist (window (window-list nil 'nomini))
    (let ((kind (my/panel-kind window)))
      (cond
       (kind
        (set-window-parameter window 'mode-line-format 'none)
        (set-window-parameter window 'header-line-format (my/panel--header window kind))
        (with-current-buffer (window-buffer window)
          (when (bound-and-true-p display-line-numbers-mode)
            (display-line-numbers-mode -1))))
       ((and (buffer-local-value 'my/panel--kind (window-buffer window))
             (alist-get 'my/panel-tab (cdr (tab-bar--current-tab))))
        (set-window-parameter window 'header-line-format (my/panel--tab-header window)))
       ((window-parameter window 'header-line-format)
        ;; it was a panel and isn't any more
        (set-window-parameter window 'header-line-format nil)
        (set-window-parameter window 'mode-line-format nil))))))

(defun my/panel-close (&optional window)
  (interactive)
  (let ((window (or window (selected-window))))
    (if (eq (my/panel-kind window) 'explorer)
        (my/activity-explorer)
      (delete-window window))
    (my/activity-render)))

(defun my/panel--tab-id ()
  "An id for the selected tab (kept in its params), made the first time."
  (let ((tab (tab-bar--current-tab-find)))
    (or (alist-get 'my/tab-id (cdr tab))
        (let ((id (format "%x" (random most-positive-fixnum))))
          (setf (alist-get 'my/tab-id (cdr tab)) id)
          (tab-bar-tabs-set (funcall tab-bar-tabs-function))
          id))))

(defun my/panel-to-tab (&optional window)
  "Move the panel in WINDOW into a tab of its own (SPC t m brings it back)."
  (interactive)
  (let* ((window (or window (selected-window)))
         (kind (or (my/panel-kind window) (user-error "That isn't a panel")))
         (buffer (window-buffer window))
         (home (my/panel--tab-id))
         (project (alist-get 'my-project-id (cdr (tab-bar--current-tab)))))
    (when (eq kind 'explorer)
      (user-error "The explorer stays beside the bar"))
    (with-current-buffer buffer
      (setq my/panel--kind kind
            my/panel--home home))
    (delete-window window)
    (tab-bar-new-tab)
    (my/session--leave-side-window-safe)
    (delete-other-windows)
    (switch-to-buffer buffer)
    (let ((tab (tab-bar--current-tab-find)))
      (setf (alist-get 'my/panel-tab (cdr tab)) t
            (alist-get 'my-project-id (cdr tab)) nil)
      (tab-bar-tabs-set (funcall tab-bar-tabs-function)))
    (tab-bar-rename-tab
     (concat (capitalize (downcase (my/panel--title kind)))
             (when-let ((name (and project (fboundp 'my/project-by-id)
                                   (plist-get (my/project-by-id project) :name))))
               (concat " · " name))))
    (my/panel-decorate)))

(defun my/session--leave-side-window-safe ()
  (when (fboundp 'my/session--leave-side-window) (my/session--leave-side-window)))

(defun my/panel--show (kind buffer)
  "Show BUFFER as a KIND panel in the selected tab."
  (pcase kind
    ('terminal
     (let ((window (display-buffer-in-side-window
                    buffer '((side . bottom) (slot . 2) (window-height . 0.3)))))
       (set-window-parameter window 'my-activity-terminal t)
       (select-window window)))
    ('run (select-window (my/run-panel--display buffer)))
    ('containers
     (let ((window (display-buffer-in-side-window
                    buffer `((side . right) (slot . 0)
                             (window-width . ,(bound-and-true-p my/containers-panel-width))))))
       (set-window-parameter window 'my-containers-panel t)
       (set-window-parameter window 'no-delete-other-windows t)
       (select-window window)))
    ('git
     (let ((window (my/activity--sidebar-display buffer nil)))
       (set-window-parameter window 'my-activity-git t)
       (set-window-parameter window 'no-delete-other-windows t)
       (set-window-dedicated-p window t)
       (select-window window)))
    (_ (select-window (display-buffer-in-side-window buffer '((side . bottom) (slot . 1)))))))

(defun my/panel-back (&optional window)
  "Put the panel shown in this tab back where it came from, and close the tab."
  (interactive)
  (let* ((buffer (window-buffer (or window (selected-window))))
         (kind (or (buffer-local-value 'my/panel--kind buffer)
                   (user-error "This tab isn't a moved panel")))
         (home (buffer-local-value 'my/panel--home buffer))
         (index (seq-position (funcall tab-bar-tabs-function) home
                              (lambda (tab id) (equal (alist-get 'my/tab-id (cdr tab)) id)))))
    (with-current-buffer buffer (setq my/panel--kind nil my/panel--home nil))
    (tab-bar-close-tab)
    (when index
      (tab-bar-select-tab (1+ (seq-position (funcall tab-bar-tabs-function) home
                                            (lambda (tab id) (equal (alist-get 'my/tab-id (cdr tab)) id))))))
    (my/panel--show kind buffer)
    (my/panel-decorate)))

(defun my/panel-toggle-tab ()
  "In a panel: give it its own tab.  In a moved panel's tab: send it back."
  (interactive)
  (cond ((my/panel-kind (selected-window)) (my/panel-to-tab))
        ((buffer-local-value 'my/panel--kind (current-buffer)) (my/panel-back))
        (t (user-error "Not in a panel (Run, Terminal, Containers, Git, Problems)"))))

(add-hook 'vc-dir-mode-hook (lambda () (display-line-numbers-mode -1)))

;;; keeping it in every tab
;; ---------------------------------------------------------------------

(define-minor-mode my/activity-bar-mode
  "Show the activity bar on the left of every tab."
  :global t :init-value nil :group 'convenience
  (if my/activity-bar-mode
      (progn
        (add-hook 'window-configuration-change-hook #'my/activity--changed)
        (add-hook 'tab-bar-tab-post-select-functions #'my/activity--now)
        (add-hook 'tab-bar-tab-post-open-functions #'my/activity--now)
        (my/activity-ensure))
    (remove-hook 'window-configuration-change-hook #'my/activity--changed)
    (remove-hook 'tab-bar-tab-post-select-functions #'my/activity--now)
    (remove-hook 'tab-bar-tab-post-open-functions #'my/activity--now)
    (when-let ((window (my/activity--window))) (delete-window window))))

(defun my/activity--window (&optional frame)
  (seq-find (lambda (w) (window-parameter w 'my-activity-bar))
            (window-list frame 'nomini)))

(defun my/activity--buffer ()
  (or (get-buffer my/activity-buffer)
      (with-current-buffer (get-buffer-create my/activity-buffer)
        (my/activity-mode)
        (current-buffer))))

(defun my/activity--normal-frame-p (&optional frame)
  (and (not (frame-parameter frame 'parent-frame))
       (not (eq (frame-parameter frame 'minibuffer) 'only))
       (display-graphic-p frame)))

(defun my/activity-ensure ()
  "Put the bar in the selected tab if it isn't there."
  (when (and my/activity-bar-mode (my/activity--normal-frame-p))
    (unless (window-live-p (my/activity--window))
      (let ((window (display-buffer-in-side-window
                     (my/activity--buffer)
                     `((side . left) (slot . -1)
                       (window-width . ,my/activity-width)
                       (preserve-size . (t . nil))
                       (dedicated . t)
                       (window-parameters . ((my-activity-bar . t)
                                             (no-other-window . t)
                                             (no-delete-other-windows . t)
                                             (mode-line-format . none)))))))
        (set-window-fringes window 0 0)
        (set-window-margins window 0 0)
        (set-window-scroll-bars window 0 nil 0 nil)))
    (my/activity-render)))

(defun my/activity--now (&rest _)
  "Add the bar right away (tab opened or switched: the layout is settled)."
  (ignore-errors (my/activity-ensure)))

(defvar my/activity--pending nil)

(defun my/activity--soon (&rest _)
  (unless my/activity--pending
    (setq my/activity--pending t)
    (run-at-time 0 nil (lambda ()
                         (setq my/activity--pending nil)
                         (my/activity-ensure)))))

(defun my/activity--changed ()
  (when (my/activity--normal-frame-p)
    (my/panel-decorate)
    (my/activity--soon)))

;; the bar is rebuilt after a session restores its tabs
(with-eval-after-load 'my-session
  (advice-add 'my/session-load :after (lambda (&rest _) (my/activity--now))))

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)

(my/activity--load-hidden)
(my/activity-apply-theme)
(my/panel--apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/activity-apply-theme)
  (advice-add fn :after #'my/panel--apply-theme))

(add-hook 'emacs-startup-hook (lambda () (my/activity-bar-mode 1)))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>ta") #'my/activity-bar-mode
    (kbd "<leader>gs") #'my/activity-git
    (kbd "<leader>tm") #'my/panel-toggle-tab))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC t a" "activity bar")
  (which-key-add-key-based-replacements "SPC g s" "git: changed files")
  (which-key-add-key-based-replacements "SPC t m" "panel ⇄ its own tab"))

(provide 'activity-bar)

;;; activity-bar.el ends here
