;;; ui/modeline.el  -*- lexical-binding: t; -*-

;; modern status bar

;; Left:  mode   project › icon file ●   branch
;; Right: ♪ track   ▶ running   ✗ errors ⚠ warnings   lsp   language   Ln, Col
;;        indent   encoding
;;
;; Colors come from the current theme (same palette approach as ui/tabs.el)
;; and are rebuilt on every theme change.  Inactive windows show a dimmed,
;; reduced line.

(require 'cl-lib)
(require 'subr-x)
(require 'tabs)

;; evil puts its own <N> tag in the mode line; the state label replaces it.
;; must be set before evil loads
(setq evil-mode-line-format nil)

(defvar my/modeline-padding 0
  "Vertical padding of the mode line, in pixels.")

(defvar my/modeline-height 0.85
  "Mode-line text size relative to the default font.")

(defvar my/modeline-wide 110
  "Show indentation and encoding only in windows at least this many columns wide.")

;;; faces
;; ---------------------------------------------------------------------

(defface my/ml-dim      '((t)) "Secondary mode-line text.")
(defface my/ml-strong   '((t)) "Buffer name.")
(defface my/ml-modified '((t)) "Unsaved-changes marker.")
(defface my/ml-error    '((t)) "Error count.")
(defface my/ml-warning  '((t)) "Warning count.")
(defface my/ml-ok       '((t)) "All-clear / running marker.")
(defface my/ml-state    '((t :weight semibold)) "Evil state label.")
(defface my/ml-hover    '((t)) "A clickable part under the mouse (no box, so nothing moves).")

(defvar my/ml--state-colors nil
  "Alist of evil state -> color of the state label.")

(defun my/modeline-apply-theme (&rest _)
  "Rebuild mode-line colors from the current theme."
  (let* ((bg     (my/tab--face-color 'default :background "#1e1f22"))
         (fg     (my/tab--face-color 'default :foreground "#dfe1e5"))
         (dark   (< (my/tab--luma bg) 0.5))
         (strip  (my/tab--mix (if dark "#ffffff" "#000000") bg (if dark 0.045 0.05)))
         (dim    (my/tab--mix fg bg 0.55))
         (accent (or (my/tab--face-color 'cursor :background nil)
                     (my/tab--face-color 'link :foreground "#3574f0")))
         (err    (my/tab--face-color 'error :foreground "#e55561"))
         (warn   (my/tab--face-color 'warning :foreground "#e2b86b"))
         (ok     (my/tab--face-color 'success :foreground "#5fb85f"))
         (border (my/tab--mix fg bg 0.10)))
    ;; flat: editor background, a hairline on top, small text.
    ;; themes style mode-line-active separately, so set every face fully
    (dolist (spec `((mode-line ,bg ,fg) (mode-line-active ,bg ,fg)
                    (mode-line-inactive ,bg ,dim)))
      (when (facep (car spec))
        (set-face-attribute (car spec) nil
                            :background (nth 1 spec) :foreground (nth 2 spec)
                            :box (and (> my/modeline-padding 0)
                                      `(:line-width (1 . ,my/modeline-padding) :color ,(nth 1 spec)))
                            :overline border :underline nil :inherit nil
                            :weight 'normal :height my/modeline-height
                            :family 'unspecified :slant 'normal)))
    (set-face-attribute 'my/ml-dim nil :foreground dim)
    (set-face-attribute 'my/ml-modified nil :foreground accent)
    (set-face-attribute 'my/ml-error nil :foreground err)
    (set-face-attribute 'my/ml-warning nil :foreground warn)
    (set-face-attribute 'my/ml-ok nil :foreground ok)
    (set-face-attribute 'my/ml-hover nil :background (my/tab--mix fg bg 0.14) :box nil)
    (ignore strip)
    (setq my/ml--state-colors `((normal . ,dim) (insert . ,ok) (visual . ,warn)
                                (replace . ,err) (operator . ,dim)
                                (emacs . ,dim) (motion . ,dim))))
  (force-mode-line-update t))

;;; clicking
;; ---------------------------------------------------------------------
;; Every part of the bar does something: a left click does the obvious
;; thing, a right click opens a menu (with each item's key shown, so the
;; mouse teaches the keys), and hovering says what a click does.

(defvar my/ml--maps (make-hash-table :test #'eq)
  "MENU (or LEFT, for parts without a menu) -> the mouse keymap of a part.
Each part has its own menu function, so this stays small even though the
click commands are made afresh on every redisplay.")

(defun my/ml--in-window (event fn)
  "Call FN in the window whose mode line EVENT clicked."
  (let ((window (posn-window (event-start event))))
    (if (window-live-p window)
        (with-selected-window window (funcall fn))
      (funcall fn))))

(defun my/ml--map (left menu)
  "Mouse keymap: LEFT runs on a left click, MENU gives right-click items."
  (with-memoization (gethash (or menu left) my/ml--maps)
    (let ((map (make-sparse-keymap)))
      (when left
        (define-key map [mode-line mouse-1]
                    (lambda (event) (interactive "e")
                      (my/ml--in-window event (lambda () (call-interactively left))))))
      (when menu
        (define-key map [mode-line mouse-3]
                    (lambda (event) (interactive "e")
                      (my/ml--in-window event
                                        (lambda () (popup-menu (my/menu "" (funcall menu)) event))))))
      map)))

(defun my/ml--button (string help &optional left menu)
  "STRING that runs LEFT on a click and shows MENU's items on a right click."
  (propertize string
              'mouse-face 'my/ml-hover
              'help-echo (concat help (and menu "  ·  right-click for more"))
              'local-map (my/ml--map left menu)))

(defun my/ml--pick (command)
  "COMMAND with its prompt in the vertico popup."
  (lambda () (interactive)
    (if (fboundp 'my/with-vertico) (my/with-vertico command) (call-interactively command))))

(defmacro my/ml--do (&rest body)
  `(lambda () (interactive) ,@body))

(defun my/ml--has (fn) (and (fboundp fn) t))

;; menus

(defun my/ml--main-items ()
  `(("Search everywhere" my/search-everywhere-popup :keys "SPC SPC" :enable (fboundp 'my/search-everywhere-popup))
    ("Search in project" my/search-project-popup :keys "SPC p SPC" :enable (fboundp 'my/project-root))
    ("Find file" ,(my/ml--pick #'consult-fd) :keys "SPC f f")
    ("Open project…" ,(my/ml--pick #'my/project-open) :keys "SPC p o")
    "--"
    ("File tree" my/neotree-toggle :keys "SPC e e" :enable (fboundp 'my/neotree-toggle))
    ("Run panel" my/run-panel-toggle :keys "SPC r p" :enable (fboundp 'my/run-panel-toggle))
    ("Containers" my/containers-panel-toggle :keys "SPC d d" :enable (fboundp 'my/containers-panel-toggle))
    ("Services" my/services-open-tab :keys "SPC r S" :enable (fboundp 'my/services-open-tab))
    ("Problems" flycheck-list-errors :keys "SPC x w" :enable (fboundp 'flycheck-list-errors))
    ("Terminal" ghostel :keys "SPC v t" :enable (fboundp 'ghostel))
    "--"
    ("Music" my/music :keys "SPC m m" :enable (fboundp 'my/music))
    ("Mail" my/mail :keys "SPC i i" :enable (fboundp 'my/mail))
    ("Claude" claude-code-ide-menu :keys "SPC a c" :enable (fboundp 'claude-code-ide-menu))
    "--"
    ("Sessions" :sub (("Load…" my/session-pick-popup :keys "SPC q l")
                      ("Save as…" ,(my/ml--pick #'my/session-save) :keys "SPC q s")
                      ("Restore last" my/session-restore-last :keys "SPC q r")))
    ("Theme…" ,(my/ml--pick #'my/load-theme) :keys "SPC l t" :enable (fboundp 'my/load-theme))
    ("Settings" my/settings-open :keys "SPC t s" :enable (fboundp 'my/settings-open))))

(defun my/ml--project-items ()
  `(("Search in project" my/search-project-popup :keys "SPC p SPC")
    ("Find file" ,(my/ml--pick #'my/project-find-file) :keys "SPC p f")
    ("Search text" ,(my/ml--pick #'my/project-grep) :keys "SPC p g")
    "--"
    ("File tree" my/neotree-toggle :keys "SPC e e" :enable (fboundp 'my/neotree-toggle))
    ("Run panel" my/run-panel-toggle :keys "SPC r p" :enable (fboundp 'my/run-panel-toggle))
    ("Containers" my/containers-panel-toggle :keys "SPC d d" :enable (fboundp 'my/containers-panel-toggle))
    ("Terminal here" my/project-terminal :keys "SPC p v")
    ("Files (dired)" my/project-dired :keys "SPC p d")
    ("New tab in this project" my/project-new-tab :keys "SPC p t")
    "--"
    ("Switch project…" ,(my/ml--pick #'my/project-open) :keys "SPC p o")
    ("Close project" my/project-close :keys "SPC p k")))

(defun my/ml--file-items ()
  (let ((file buffer-file-name))
    `(("Switch buffer…" ,(my/ml--pick #'my/find-buffer-floating) :keys "SPC f b"
       :enable (fboundp 'my/find-buffer-floating))
      ("Find in this file…" ,(my/ml--pick #'consult-line) :keys "SPC f s")
      "--"
      ("Save" save-buffer :keys "C-x C-s" :enable (and ,(and file t) (buffer-modified-p)))
      ("Revert to saved" ,(my/ml--do (revert-buffer nil t)) :enable ,(and file t))
      ("Read-only" read-only-mode :button (:toggle . buffer-read-only))
      "--"
      ("Copy path" ,(my/ml--do (kill-new file) (message "Copied %s" file)) :enable ,(and file t))
      ("Copy path from project root"
       ,(my/ml--do (let ((rel (file-relative-name file (or (and (fboundp 'my/project-root) (my/project-root))
                                                             default-directory))))
                     (kill-new rel) (message "Copied %s" rel)))
       :enable ,(and file t))
      ("Open its folder" ,(my/ml--do (dired (file-name-directory file))) :enable ,(and file t))
      "--"
      ("Rename file…" rename-visited-file :enable ,(and file t))
      ("Delete file…" ,(my/ml--do (when (yes-or-no-p (format "Delete %s? " file))
                                    (delete-file file t) (kill-buffer)))
       :enable ,(and file t))
      "--"
      ("Close" kill-current-buffer))))

(defun my/ml--git-items ()
  `(("Switch branch…" my/git-switch-branch :keys "SPC g b")
    ("New branch…" ,(my/ml--pick #'my/git-new-branch))
    "--"
    ("Source Control" my/activity-git :keys "SPC g s")
    ("Commit" my/git-commit :keys "SPC g c")
    ("History" vc-print-root-log)
    "--"
    ("Pull" my/git-pull :keys "SPC g p")
    ("Push" my/git-push :keys "SPC g P")
    ("Fetch" my/git-fetch :keys "SPC g f")))

(defun my/ml--run-items ()
  `(("Run…" ,(my/ml--pick #'my/run) :enable (fboundp 'my/run))
    ("Run the last one again" my/run-last :keys "SPC r l")
    ("Build" my/run-build :keys "SPC r b")
    ("Test" my/run-test :keys "SPC r t")
    ("Stop…" ,(my/ml--pick #'my/run-stop) :keys "SPC r s")
    "--"
    ("Run panel" my/run-panel-toggle :keys "SPC r p")
    ("Services" my/services-open-tab :keys "SPC r S")))

(defun my/ml--problem-items ()
  `(("All problems" flycheck-list-errors :keys "SPC x w")
    ("Next problem" flycheck-next-error :keys "] e")
    ("Previous problem" flycheck-previous-error :keys "[ e")
    "--"
    ("Check again" flycheck-buffer)))

(defun my/ml--lsp-items ()
  `(("Rename symbol…" lsp-rename :keys "SPC r n")
    ("Code actions…" lsp-execute-code-action)
    ("Format file" lsp-format-buffer)
    ("Organize imports" lsp-organize-imports)
    ("Go to declaration" lsp-find-declaration :keys "SPC g d")
    ("Go to implementation" lsp-find-implementation :keys "SPC g i")
    ("Docs for this" lsp-describe-thing-at-point :keys "K")
    "--"
    ("Type hints" ,(my/ml--do (lsp-inlay-hints-mode 'toggle))
     :button (:toggle . (bound-and-true-p lsp-inlay-hints-mode)))
    ("Restart the language server" lsp-workspace-restart)
    ("Session info" lsp-describe-session)))

(defun my/ml--mode-items ()
  `(("Change language…" ,(my/ml--pick #'my/ml-change-mode))
    "--"
    ("Wrap long lines" visual-line-mode :button (:toggle . (bound-and-true-p visual-line-mode)))
    ("Line numbers" display-line-numbers-mode
     :button (:toggle . (bound-and-true-p display-line-numbers-mode)))
    ("Show whitespace" whitespace-mode :button (:toggle . (bound-and-true-p whitespace-mode)))))

(defun my/ml-change-mode (mode)
  "Switch the buffer to major MODE."
  (interactive
   (list (intern (completing-read
                  "Language: "
                  (let (modes)
                    (mapatoms (lambda (sym)
                                (when (and (commandp sym)
                                           (string-suffix-p "-mode" (symbol-name sym))
                                           (get sym 'derived-mode-parent))
                                  (push (symbol-name sym) modes))))
                    (sort modes #'string<))
                  nil t))))
  (funcall mode))

(defun my/ml--position-items ()
  `(("Go to line…" ,(my/ml--pick #'consult-goto-line))
    ("Copy file:line"
     ,(my/ml--do (let ((loc (format "%s:%d" (or buffer-file-name (buffer-name)) (line-number-at-pos))))
                   (kill-new loc) (message "Copied %s" loc))))))

(defun my/ml--indent-items ()
  `(("Spaces" ,(my/ml--do (setq indent-tabs-mode nil) (force-mode-line-update))
     :button (:radio . (not indent-tabs-mode)))
    ("Tabs" ,(my/ml--do (setq indent-tabs-mode t) (force-mode-line-update))
     :button (:radio . indent-tabs-mode))
    "--"
    ,@(mapcar (lambda (n) `(,(format "Width %d" n)
                            ,(my/ml--do (setq tab-width n standard-indent n) (force-mode-line-update))
                            :button (:radio . (= tab-width ,n))))
              '(2 4 8))
    "--"
    ("Convert the file to spaces" ,(my/ml--do (untabify (point-min) (point-max))))
    ("Convert the file to tabs" ,(my/ml--do (tabify (point-min) (point-max))))))

(defun my/ml--eol-items ()
  `(("LF (Linux, macOS)" ,(my/ml--do (set-buffer-file-coding-system 'unix))
     :button (:radio . (eq (coding-system-eol-type buffer-file-coding-system) 0)))
    ("CRLF (Windows)" ,(my/ml--do (set-buffer-file-coding-system 'dos))
     :button (:radio . (eq (coding-system-eol-type buffer-file-coding-system) 1)))))

(defun my/ml--encoding-items ()
  `(("Reopen with another encoding…" revert-buffer-with-coding-system)
    ("Save with another encoding…" set-buffer-file-coding-system)))

(defun my/ml--music-items ()
  (if (fboundp 'my/music--general-items)
      (my/music--general-items)
    '(("Play / pause" emms-pause))))

;;; segments
;; ---------------------------------------------------------------------

(defun my/ml--active-p ()
  (if (fboundp 'mode-line-window-selected-p) (mode-line-window-selected-p)
    (eq (selected-window) (old-selected-window))))

(defun my/ml--fits (columns)
  "Non-nil if the mode line's window is at least COLUMNS wide.
Less important segments disappear first as a window gets narrower."
  (>= (window-total-width) columns))

(defun my/ml--icon (fn &rest args)
  "Call an all-the-icons FN with ARGS, sized for the mode line; \"\" on failure."
  (or (ignore-errors (apply fn (append args '(:height 0.9 :v-adjust 0))))
      ""))

(defun my/ml-state ()
  (when (and (boundp 'evil-state) evil-state (my/ml--active-p))
    (let* ((state evil-state)
           (label (pcase state
                    ('normal "NORMAL") ('insert "INSERT") ('visual
                                                          (pcase (bound-and-true-p evil-visual-selection)
                                                            ('line "V-LINE") ('block "V-BLOCK") (_ "VISUAL")))
                    ('replace "REPLACE") ('operator "OPERATOR") ('emacs "EMACS")
                    ('motion "MOTION") (_ (upcase (symbol-name state)))))
           (color (or (alist-get state my/ml--state-colors) (alist-get 'normal my/ml--state-colors))))
      (concat (my/ml--button (propertize label 'face `(:inherit my/ml-state :foreground ,color))
                             "Everything: search, panels, music…"
                             (lambda () (interactive)
                               (popup-menu (my/menu "" (my/ml--main-items))))
                             #'my/ml--main-items)
              "   "))))

(defun my/ml-buffer ()
  (let* ((active (my/ml--active-p))
         (project (and (fboundp 'my/project-current) (my/project-current)))
         (name (buffer-name))
         (icon (if buffer-file-name
                   (my/ml--icon #'all-the-icons-icon-for-file buffer-file-name)
                 (my/ml--icon #'all-the-icons-icon-for-mode major-mode))))
    (concat
     (when (and project active (my/ml--fits 90))
       (concat (my/ml--button (propertize (plist-get project :name) 'face 'my/ml-dim)
                              (format "Project %s: click to switch project" (plist-get project :name))
                              (my/ml--pick #'my/project-open) #'my/ml--project-items)
               (propertize " › " 'face 'my/ml-dim)))
     (my/ml--button
      (concat (if (and active (stringp icon) (> (length icon) 0)) (concat icon " ") "")
              (propertize name 'face (if active 'my/ml-strong 'my/ml-dim)))
      (concat (or buffer-file-name name) "\nclick to switch buffer")
      (my/ml--pick #'my/find-buffer-floating) #'my/ml--file-items)
     (cond ((and buffer-file-name (buffer-modified-p))
            (my/ml--button (propertize " ●" 'face 'my/ml-modified)
                           "Unsaved changes: click to save" #'save-buffer))
           (buffer-read-only
            (my/ml--button (propertize (concat " " (my/ml--icon #'all-the-icons-octicon "lock"))
                                       'face 'my/ml-dim)
                           "Read-only: click to allow editing" #'read-only-mode))
           (t "")))))

(defun my/ml-vc ()
  (when (and vc-mode (my/ml--active-p) (my/ml--fits 75))
    (let* ((raw (substring-no-properties vc-mode))
           (branch (if (string-match "\\`[ ]*[A-Za-z]+[-:@!?]\\(.*\\)\\'" raw)
                       (match-string 1 raw)
                     (string-trim raw))))
      (concat "   "
              (my/ml--button
               (propertize (concat (my/ml--icon #'all-the-icons-octicon "git-branch") " " branch)
                           'face 'my/ml-dim)
               (format "Branch %s: click to switch branch" branch)
               #'my/git-switch-branch #'my/ml--git-items)))))

(defun my/ml-tasks ()
  (when (and (fboundp 'my/run-running-count) (fboundp 'my/run--root) (my/ml--active-p))
    (let ((n (ignore-errors (my/run-running-count (my/run--root)))))
      (when (and n (> n 0))
        (concat (my/ml--button (propertize (format "▶ %d running" n) 'face 'my/ml-ok)
                               "Click: Run panel (SPC r p)"
                               #'my/run-panel-toggle #'my/ml--run-items)
                "   ")))))

(defun my/ml-music ()
  (when (and (my/ml--active-p) (my/ml--fits 120) (fboundp 'my/music-now-playing))
    (when-let ((playing (my/music-now-playing)))
      (concat (my/ml--button (propertize (concat (if (bound-and-true-p emms-player-paused-p) "⏸ " "♪ ")
                                                 (truncate-string-to-width playing 40 nil nil "…"))
                                         'face 'my/ml-dim)
                             "Click: play / pause (SPC m SPC)"
                             #'emms-pause #'my/ml--music-items)
              "   "))))

(defun my/ml-diagnostics ()
  (when (and (bound-and-true-p flycheck-mode) (my/ml--active-p))
    (pcase flycheck-last-status-change
      ('running (concat (propertize "… checking" 'face 'my/ml-dim) "   "))
      ('finished
       (let* ((counts (flycheck-count-errors flycheck-current-errors))
              (errors (alist-get 'error counts 0))
              (warnings (alist-get 'warning counts 0)))
         (concat
          (my/ml--button
           (if (and (zerop errors) (zerop warnings))
               (propertize "✓" 'face 'my/ml-ok)
             (concat (propertize (format "✗ %d" errors) 'face (if (> errors 0) 'my/ml-error 'my/ml-dim))
                     "  "
                     (propertize (format "⚠ %d" warnings) 'face (if (> warnings 0) 'my/ml-warning 'my/ml-dim))))
           "Click: list problems (SPC x w)"
           #'flycheck-list-errors #'my/ml--problem-items)
          "   ")))
      (_ nil))))

(defun my/ml-lsp ()
  (when (and (bound-and-true-p lsp-mode) (my/ml--active-p) (fboundp 'lsp-workspaces)
             (my/ml--fits 100))
    (let ((names (mapcar (lambda (w) (symbol-name (lsp--workspace-server-id w)))
                         (lsp-workspaces))))
      (concat (my/ml--button (propertize (if names (string-join names ",") "lsp …") 'face 'my/ml-dim)
                             "Language server: click for its actions"
                             (lambda () (interactive) (popup-menu (my/menu "" (my/ml--lsp-items))))
                             #'my/ml--lsp-items)
              "   "))))

(defun my/ml-mode ()
  (when (my/ml--fits 65)
  (let ((name (string-trim (replace-regexp-in-string "/.*" "" (format-mode-line mode-name)))))
    (concat (my/ml--button (propertize name 'face (if (my/ml--active-p) nil 'my/ml-dim))
                           "Click: change language"
                           (my/ml--pick #'my/ml-change-mode) #'my/ml--mode-items)
            "   "))))

(defun my/ml-position ()
  (if (and (use-region-p) (my/ml--active-p))
      (let ((lines (count-lines (region-beginning) (region-end)))
            (chars (- (region-end) (region-beginning))))
        (propertize (format "%d chars, %d line%s" chars lines (if (= lines 1) "" "s"))
                    'face 'my/ml-modified))
    (my/ml--button (propertize (format "Ln %s, Col %d" (format-mode-line "%l") (1+ (current-column)))
                               'face 'my/ml-dim)
                   "Click: go to line"
                   (my/ml--pick #'consult-goto-line) #'my/ml--position-items)))

(defun my/ml-file-info ()
  (when (and buffer-file-name (my/ml--active-p)
             (>= (window-total-width) my/modeline-wide))
    (let* ((coding (symbol-name (coding-system-type buffer-file-coding-system)))
           (eol (pcase (coding-system-eol-type buffer-file-coding-system)
                  (0 "LF") (1 "CRLF") (2 "CR") (_ "")))
           (enc (cond ((string-match-p "utf-8" coding) "UTF-8")
                      ((member coding '("undecided" "prefer-utf-8")) "UTF-8")
                      (t (upcase coding))))
           (indent (if indent-tabs-mode
                       (format "Tab %d" tab-width)
                     (format "%d spaces" (or (and (boundp 'standard-indent) standard-indent) tab-width)))))
      (concat "   "
              (my/ml--button (propertize indent 'face 'my/ml-dim) "Indentation: click to change"
                             (lambda () (interactive) (popup-menu (my/menu "" (my/ml--indent-items))))
                             #'my/ml--indent-items)
              "   "
              (my/ml--button (propertize eol 'face 'my/ml-dim) "Line endings: click to change"
                             (lambda () (interactive) (popup-menu (my/menu "" (my/ml--eol-items))))
                             #'my/ml--eol-items)
              " "
              (my/ml--button (propertize enc 'face 'my/ml-dim) "Encoding: click to change"
                             (lambda () (interactive) (popup-menu (my/menu "" (my/ml--encoding-items))))
                             #'my/ml--encoding-items)))))

;;; layout
;; ---------------------------------------------------------------------

(setq-default mode-line-format
              '(" "
                (:eval (my/ml-state))
                (:eval (my/ml-buffer))
                (:eval (my/ml-vc))
                "    "
                mode-line-format-right-align
                (:eval (my/ml-music))
                (:eval (my/ml-tasks))
                (:eval (my/ml-diagnostics))
                (:eval (my/ml-lsp))
                (:eval (my/ml-mode))
                (:eval (my/ml-position))
                (:eval (my/ml-file-info))
                "  "))

(setq mode-line-right-align-edge 'window)


(my/modeline-apply-theme)
(advice-add 'load-theme :after #'my/modeline-apply-theme)
(advice-add 'enable-theme :after #'my/modeline-apply-theme)
(advice-add 'disable-theme :after #'my/modeline-apply-theme)
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (my/modeline-apply-theme))))

(provide 'modeline)

;;; modeline.el ends here
