;;; ui/modeline.el  -*- lexical-binding: t; -*-

;; modern status bar

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
         (purple (my/tab--face-color 'font-lock-constant-face :foreground "#c678dd"))
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
    (ignore strip)
    (setq my/ml--state-colors `((normal . ,dim) (insert . ,ok) (visual . ,warn)
                                (replace . ,err) (operator . ,dim)
                                (emacs . ,purple) (motion . ,dim))))
  (force-mode-line-update t))

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
      (concat (propertize label 'face `(:inherit my/ml-state :foreground ,color))
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
       (concat (propertize (plist-get project :name) 'face 'my/ml-dim)
               (propertize " › " 'face 'my/ml-dim)))
     (if (and active (stringp icon) (> (length icon) 0)) (concat icon " ") "")
     (propertize name 'face (if active 'my/ml-strong 'my/ml-dim)
                 'help-echo (or buffer-file-name name))
     (cond ((and buffer-file-name (buffer-modified-p))
            (propertize " ●" 'face 'my/ml-modified 'help-echo "Unsaved changes"))
           (buffer-read-only
            (propertize (concat " " (my/ml--icon #'all-the-icons-octicon "lock"))
                        'face 'my/ml-dim 'help-echo "Read-only"))
           (t "")))))

(defun my/ml-vc ()
  (when (and vc-mode (my/ml--active-p) (my/ml--fits 75))
    (let* ((raw (substring-no-properties vc-mode))
           (branch (if (string-match "\\`[ ]*[A-Za-z]+[-:@!?]\\(.*\\)\\'" raw)
                       (match-string 1 raw)
                     (string-trim raw))))
      (concat "   "
              (propertize (concat (my/ml--icon #'all-the-icons-octicon "git-branch") " " branch)
                          'face 'my/ml-dim)))))

(defun my/ml-tasks ()
  (when (and (fboundp 'my/run-running-count) (fboundp 'my/run--root) (my/ml--active-p))
    (let ((n (ignore-errors (my/run-running-count (my/run--root)))))
      (when (and n (> n 0))
        (concat (propertize (format "▶ %d running" n)
                            'face 'my/ml-ok
                            'help-echo "Run panel (SPC r p)"
                            'mouse-face 'mode-line-highlight
                            'local-map (make-mode-line-mouse-map 'mouse-1 #'my/run-panel-toggle))
                "   ")))))

(defun my/ml-diagnostics ()
  (when (and (bound-and-true-p flycheck-mode) (my/ml--active-p))
    (pcase flycheck-last-status-change
      ('running (concat (propertize "… checking" 'face 'my/ml-dim) "   "))
      ('finished
       (let* ((counts (flycheck-count-errors flycheck-current-errors))
              (errors (alist-get 'error counts 0))
              (warnings (alist-get 'warning counts 0)))
         (concat
          (propertize
           (if (and (zerop errors) (zerop warnings))
               (propertize "✓" 'face 'my/ml-ok)
             (concat (propertize (format "✗ %d" errors) 'face (if (> errors 0) 'my/ml-error 'my/ml-dim))
                     "  "
                     (propertize (format "⚠ %d" warnings) 'face (if (> warnings 0) 'my/ml-warning 'my/ml-dim))))
           'help-echo "List problems (SPC x w)"
           'mouse-face 'mode-line-highlight
           'local-map (make-mode-line-mouse-map 'mouse-1 #'flycheck-list-errors))
          "   ")))
      (_ nil))))

(defun my/ml-lsp ()
  (when (and (bound-and-true-p lsp-mode) (my/ml--active-p) (fboundp 'lsp-workspaces)
             (my/ml--fits 100))
    (let ((names (mapcar (lambda (w) (symbol-name (lsp--workspace-server-id w)))
                         (lsp-workspaces))))
      (concat (propertize (if names (string-join names ",") "lsp …") 'face 'my/ml-dim)
              "   "))))

(defun my/ml-mode ()
  (when (my/ml--fits 65)
  (let ((name (string-trim (replace-regexp-in-string "/.*" "" (format-mode-line mode-name)))))
    (concat (propertize name 'face (if (my/ml--active-p) nil 'my/ml-dim)) "   "))))

(defun my/ml-position ()
  (if (and (use-region-p) (my/ml--active-p))
      (let ((lines (count-lines (region-beginning) (region-end)))
            (chars (- (region-end) (region-beginning))))
        (propertize (format "%d chars, %d line%s" chars lines (if (= lines 1) "" "s"))
                    'face 'my/ml-modified))
    (propertize (format "Ln %s, Col %d" (format-mode-line "%l") (1+ (current-column)))
                'face 'my/ml-dim)))

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
      (propertize (format "   %s   %s %s" indent eol enc) 'face 'my/ml-dim))))

;;; layout
;; ---------------------------------------------------------------------

(setq-default mode-line-format
              '(" "
                (:eval (my/ml-state))
                (:eval (my/ml-buffer))
                (:eval (my/ml-vc))
                "    "
                mode-line-format-right-align
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
