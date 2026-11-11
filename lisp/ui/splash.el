;;; ui/splash.el  -*- lexical-binding: t; -*-

;; the Welcome page: what the code area shows when nothing else is there

;; Closing the last code window (:q, C-x 0, ...) while the tree or a
;; panel is still open used to hand the whole frame to the tree, and then
;; nothing had anywhere to open.  Now that window stays and shows this
;; page instead, like VS Code's empty editor.  :q on the Welcome page
;; itself does what :q on a lone window always did: closes the tab, or
;; Emacs when it's the last one.
;;
;; The page: quick actions, this project's recent files (or recent
;; files anywhere), and recent projects.  Click, or j/k and RET.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function my/project-current "projects")
(declare-function my/project-known-roots "projects")
(declare-function my/project-switch "projects")
(declare-function my/activity--sidebar-p "activity-bar")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(defvar recentf-list)

(defvar my/splash-buffer "*Welcome*")

;;; looks
;; ---------------------------------------------------------------------

(defface my/splash-title '((t :weight light)) "The big name at the top.")
(defface my/splash-heading '((t :weight semibold)) "START, RECENT...")
(defface my/splash-item '((t)) "A clickable line.")
(defface my/splash-dim '((t)) "Folders and key hints.")
(defface my/splash-hover '((t)) "A line under the mouse.")

(defun my/splash-apply-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
           (fg (my/tab--face-color 'default :foreground "#dfe1e5")))
      (set-face-attribute 'my/splash-title nil :foreground (my/tab--mix fg bg 0.5) :height 2.4)
      (set-face-attribute 'my/splash-heading nil :foreground (my/tab--mix fg bg 0.55) :height 0.85)
      (set-face-attribute 'my/splash-item nil :foreground (my/tab--mix fg bg 0.9))
      (set-face-attribute 'my/splash-dim nil :foreground (my/tab--mix fg bg 0.42))
      (set-face-attribute 'my/splash-hover nil :background (my/tab--mix fg bg 0.1) :extend nil))))

(defun my/splash--glyph (name)
  (or (ignore-errors
        (let ((icon (nerd-icons-codicon name)))
          (propertize (substring-no-properties icon)
                      'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                              :height 1.0 :inherit my/splash-dim))))
      " "))

;;; what's on it
;; ---------------------------------------------------------------------

(defun my/splash--vertico (command)
  (lambda () (interactive)
    (if (fboundp 'my/with-vertico) (my/with-vertico command) (call-interactively command))))

(defun my/splash--start-items ()
  "(ICON LABEL KEYS COMMAND) for the Start section."
  `(("nf-cod-new_file" "New file…" "" ,(my/splash--vertico #'find-file))
    ("nf-cod-go_to_file" "Open a file…" "SPC f f"
     ,(lambda () (interactive)
        (let ((cmd (lookup-key (and (boundp 'evil-normal-state-map) evil-normal-state-map)
                               (kbd "<leader>ff"))))
          (if (commandp cmd) (call-interactively cmd) (call-interactively #'find-file)))))
    ("nf-cod-search" "Search everywhere" "SPC SPC" my/search-everywhere-popup)
    ("nf-cod-folder_opened" "Open a project…" "SPC p o"
     ,(my/splash--vertico #'my/project-open))
    ("nf-cod-terminal" "Terminal" "" my/activity-terminal)
    ("nf-cod-history" "Load a session…" "SPC q l" my/session-pick-popup)))

(defun my/splash--root ()
  (and (fboundp 'my/project-current) (plist-get (my/project-current) :root)))

(defun my/splash--recent-files (root n)
  "Up to N recent files, inside ROOT when it's given."
  (require 'recentf)
  (seq-take (seq-filter (lambda (f)
                          (and (or (null root) (string-prefix-p root (expand-file-name f)))
                               (not (file-remote-p f))
                               (file-exists-p f)))
                        recentf-list)
            n))

(defun my/splash--recent-projects (n)
  (seq-take (and (fboundp 'my/project-known-roots) (my/project-known-roots)) n))

;;; drawing
;; ---------------------------------------------------------------------

(defun my/splash--fit (string width)
  "STRING cut to WIDTH, keeping its end (the interesting part of a path)."
  (if (<= (string-width string) width) string
    (concat "…" (substring string (- (length string) (1- width))))))

(defvar-local my/splash--width 0)

(defun my/splash--margin (window)
  "Spaces that centre a 60-column page in WINDOW."
  (make-string (max 2 (/ (- (window-max-chars-per-line window) 60) 2)) ?\s))

(defun my/splash--line (margin text action &optional help)
  "One clickable line."
  (insert margin
          (propertize text
                      'my/splash-action action
                      'mouse-face 'my/splash-hover
                      'help-echo help)
          "\n"))

(defun my/splash--row (margin icon label right action &optional help)
  (my/splash--line
   margin
   (concat (my/splash--glyph icon) "  "
           (propertize label 'face 'my/splash-item)
           (propertize " " 'display `(space :align-to ,(+ (length margin) 36)))
           (propertize (my/splash--fit right 24) 'face 'my/splash-dim))
   action help))

(defun my/splash--file-row (margin file root)
  (let* ((name (file-name-nondirectory file))
         (dir (abbreviate-file-name
               (directory-file-name
                (if root (file-relative-name (file-name-directory file) root)
                  (file-name-directory file)))))
         (icon (or (ignore-errors (nerd-icons-icon-for-file name)) " ")))
    (my/splash--line
     margin
     (concat icon "  " (propertize name 'face 'my/splash-item) "  "
             (propertize (truncate-string-to-width (if (equal dir ".") "" dir) (max 0 (- 50 (length name))) nil nil "…")
                         'face 'my/splash-dim))
     (lambda () (interactive) (find-file file))
     file)))

(defun my/splash-render ()
  "Draw the Welcome page for its window's width."
  (let* ((inhibit-read-only t)
         (window (or (get-buffer-window (current-buffer)) (selected-window)))
         (margin (my/splash--margin window))
         (project (and (fboundp 'my/project-current) (my/project-current)))
         (root (my/splash--root))
         (top (max 1 (/ (- (window-body-height window) 26) 3))))
    (erase-buffer)
    (insert (make-string top ?\n))
    (insert margin (propertize "limacs" 'face 'my/splash-title) "\n")
    (insert margin (propertize (if project
                                   (let ((name (plist-get project :name)))
                                     (concat name "   "
                                             (my/splash--fit (abbreviate-file-name (directory-file-name root))
                                                             (- 57 (string-width name)))))
                                 "No project in this tab")
                               'face 'my/splash-dim)
            "\n\n\n")
    (insert margin (propertize "START" 'face 'my/splash-heading) "\n")
    (dolist (item (my/splash--start-items))
      (pcase-let ((`(,icon ,label ,keys ,command) item))
        (when (or (not (symbolp command)) (fboundp command))
          (my/splash--row margin icon label keys command))))
    (insert "\n")
    (let ((files (my/splash--recent-files root 7)))
      (when files
        (insert margin (propertize (if root "RECENT IN THIS PROJECT" "RECENT FILES")
                                   'face 'my/splash-heading)
                "\n")
        (dolist (f files) (my/splash--file-row margin f root))
        (insert "\n")))
    (let ((projects (seq-remove (lambda (r) (equal r root)) (my/splash--recent-projects 5))))
      (when projects
        (insert margin (propertize "PROJECTS" 'face 'my/splash-heading) "\n")
        (dolist (r projects)
          (my/splash--row margin "nf-cod-folder"
                          (file-name-nondirectory (directory-file-name r))
                          (abbreviate-file-name (directory-file-name r))
                          (lambda () (interactive) (my/project-switch r))))))
    (setq my/splash--width (window-width window))
    (goto-char (point-min))
    (my/splash-next)))

;;; moving and doing
;; ---------------------------------------------------------------------

(defun my/splash-next ()
  "To the next clickable line."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (my/splash--action-here)))
      (forward-line 1))
    (if (my/splash--action-here)
        (my/splash--to-text)
      (goto-char start))))

(defun my/splash-previous ()
  "To the previous clickable line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (my/splash--action-here)))
      (forward-line -1))
    (if (my/splash--action-here)
        (my/splash--to-text)
      (goto-char start))))

(defun my/splash--action-here ()
  (let ((pos (next-single-property-change (line-beginning-position) 'my/splash-action
                                          nil (line-end-position))))
    (or (get-text-property (line-beginning-position) 'my/splash-action)
        (and pos (< pos (line-end-position)) (get-text-property pos 'my/splash-action)))))

(defun my/splash--to-text ()
  (goto-char (line-beginning-position))
  (unless (get-text-property (point) 'my/splash-action)
    (goto-char (next-single-property-change (point) 'my/splash-action nil (line-end-position)))))

(defun my/splash-open ()
  "Do the line at point."
  (interactive)
  (when-let ((action (my/splash--action-here)))
    (call-interactively action)))

(defun my/splash-click (event)
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (select-window (posn-window (event-start event)))
    (when pos
      (goto-char pos)
      (my/splash-open))))

(defvar my/splash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'my/splash-click)
    (define-key map (kbd "RET") #'my/splash-open)
    (define-key map (kbd "j") #'my/splash-next)
    (define-key map (kbd "k") #'my/splash-previous)
    map))

(define-derived-mode my/splash-mode special-mode "Welcome"
  "The Welcome page."
  (setq-local truncate-lines t
              cursor-type nil
              evil-normal-state-cursor '(nil)
              line-spacing 0.25)
  (display-line-numbers-mode -1)
  (hl-line-mode 1)
  (face-remap-add-relative 'hl-line 'my/splash-hover)
  (add-hook 'window-size-change-functions
            (lambda (w)
              (with-current-buffer (window-buffer w)
                (when (and (derived-mode-p 'my/splash-mode)
                           (/= my/splash--width (window-width w)))
                  (my/splash-render))))
            nil t))

(defun my/splash-buffer ()
  "The Welcome page, drawn for the current tab."
  (let ((buffer (get-buffer-create my/splash-buffer))
        (root (my/splash--root)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/splash-mode) (my/splash-mode))
      (setq default-directory (or root (expand-file-name "~/"))))
    buffer))

(defun my/splash-show (&optional window)
  "Show the Welcome page in WINDOW (or the selected one)."
  (interactive)
  (let ((window (or window (selected-window)))
        (buffer (my/splash-buffer)))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (with-selected-window window
      (with-current-buffer buffer (my/splash-render)))
    (when (eq window (selected-window)) (set-buffer buffer))
    window))

;;; keeping a place for code
;; ---------------------------------------------------------------------

(defun my/splash--editor-p (window)
  "Non-nil if WINDOW is part of the code area (not the bar, tree or a panel)."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-parameter window 'my-activity-bar))
       (not (and (fboundp 'my/activity--sidebar-p) (my/activity--sidebar-p window)))))

(defun my/splash--keep-code-area (fn &optional window)
  "Run FN (`delete-window'), unless WINDOW is the last of the code area:
then it shows the Welcome page instead.  Deleting the Welcome page itself
signals an error, so :q goes on to close the tab, as on a lone window."
  (let ((w (window-normalize-window window t)))
    (if (and (my/splash--editor-p w)
             ;; something else (the tree, a panel) would be left holding the frame
             (seq-some (lambda (o) (and (not (eq o w)) (not (window-parameter o 'window-side))
                                        (not (window-parameter o 'my-activity-bar))))
                       (window-list (window-frame w) 'nomini))
             (not (seq-some (lambda (o) (and (not (eq o w)) (my/splash--editor-p o)))
                            (window-list (window-frame w) 'nomini))))
        (if (eq (buffer-local-value 'major-mode (window-buffer w)) 'my/splash-mode)
            (error "The Welcome page is the last window")
          (my/splash-show w))
      (funcall fn window))))

(advice-add 'delete-window :around #'my/splash--keep-code-area)

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/splash-mode 'normal)
  (evil-define-key 'normal my/splash-mode-map
    (kbd "RET") #'my/splash-open
    (kbd "<return>") #'my/splash-open
    "j" #'my/splash-next
    "k" #'my/splash-previous
    [down-mouse-1] #'ignore
    [mouse-1] #'my/splash-click
    "g" (lambda () (interactive) (my/splash-render))))

(my/splash-apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/splash-apply-theme))

(provide 'splash)

;;; splash.el ends here
