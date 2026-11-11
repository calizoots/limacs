;;; ui/problems.el  -*- lexical-binding: t; -*-

;; Problems: every error and warning in the project, in a bottom panel

;; Like VS Code's Problems view.  Everything the language servers report
;; for the current project (rust-analyzer checks the whole crate, so
;; files you haven't opened are there too), plus flycheck's findings in
;; buffers without a language server.  Grouped by file, errors first,
;; updated as they change.
;;
;; Keys
;; ---------------------------------------------------------------------
;;   SPC x w  open / close (also the bar icon, and the counts in the status bar)
;; In the panel:
;;   RET  go there (into the code)    TAB  show it, staying here
;;   e / w / i  errors, warnings, info on / off       /  filter text
;;   za / TAB on a file  fold it      g  refresh      q  close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function lsp-diagnostics "lsp-mode")
(declare-function lsp-get "lsp-protocol")
(declare-function flycheck-error-level "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-column "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-error-checker "flycheck")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function my/project-root "projects")
(declare-function my/activity--editor-window "activity-bar")
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(defvar flycheck-current-errors)

(defvar my/problems-buffer "*Problems*")
(defvar my/problems-height 0.28 "Height of the panel, as a share of the frame.")

;;; gathering
;; ---------------------------------------------------------------------

;; A problem: (:file F :line L :col C :severity error|warning|info
;;             :message M :source S)  (line and column from 1)

(defun my/problems--severity (n)
  (pcase n (1 'error) (2 'warning) (_ 'info)))

(defun my/problems--from-lsp (root)
  "What the language servers report for files under ROOT."
  (when (fboundp 'lsp-diagnostics)
    (let (out)
      (maphash
       (lambda (file diags)
         (when (or (null root) (string-prefix-p root (file-truename file)))
           (dolist (d diags)
             (let ((severity (or (lsp-get d :severity) 1)))
               ;; hints (4) show as faded text in the code, not here
               (unless (= severity 4)
                 (let ((start (lsp-get (lsp-get d :range) :start)))
                   (push (list :file file
                               :line (1+ (lsp-get start :line))
                               :col (1+ (lsp-get start :character))
                               :severity (my/problems--severity severity)
                               :message (string-trim (lsp-get d :message))
                               :source (or (lsp-get d :source) "")
                               :code (let ((c (lsp-get d :code))) (and c (format "%s" c))))
                         out)))))))
       ;; every workspace's: with t it'd be only this buffer's, and the
       ;; panel itself has no language server
       (lsp-diagnostics))
      out)))

(defun my/problems--from-flycheck (root)
  "Flycheck's findings in open buffers that have no language server."
  (let (out)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and buffer-file-name
                   (bound-and-true-p flycheck-mode)
                   (not (bound-and-true-p lsp-mode))
                   (or (null root) (string-prefix-p root (file-truename buffer-file-name))))
          (dolist (e flycheck-current-errors)
            (push (list :file buffer-file-name
                        :line (or (flycheck-error-line e) 1)
                        :col (or (flycheck-error-column e) 1)
                        :severity (pcase (flycheck-error-level e)
                                    ('error 'error) ('warning 'warning) (_ 'info))
                        :message (string-trim (or (flycheck-error-message e) ""))
                        :source (format "%s" (flycheck-error-checker e))
                        :code (and (fboundp 'flycheck-error-id) (flycheck-error-id e)))
                  out)))))
    out))

(defun my/problems--rank (severity)
  (pcase severity ('error 0) ('warning 1) (_ 2)))

(defun my/problems-collect (root)
  "Every problem under ROOT (nil: everywhere), grouped: ((FILE . PROBLEMS) ...).
Files with errors come first; inside a file, by severity then line."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (p (append (my/problems--from-lsp root) (my/problems--from-flycheck root)))
      (push p (gethash (plist-get p :file) groups)))
    (let (out)
      (maphash (lambda (file problems)
                 (push (cons file (sort problems
                                        (lambda (a b)
                                          (let ((ra (my/problems--rank (plist-get a :severity)))
                                                (rb (my/problems--rank (plist-get b :severity))))
                                            (if (/= ra rb) (< ra rb)
                                              (< (plist-get a :line) (plist-get b :line)))))))
                       out))
               groups)
      (sort out (lambda (a b)
                  (let ((ra (apply #'min (mapcar (lambda (p) (my/problems--rank (plist-get p :severity))) (cdr a))))
                        (rb (apply #'min (mapcar (lambda (p) (my/problems--rank (plist-get p :severity))) (cdr b)))))
                    (if (/= ra rb) (< ra rb) (string< (car a) (car b)))))))))

;;; looks
;; ---------------------------------------------------------------------

(defface my/problems-error '((t)) "Error icon.")
(defface my/problems-warning '((t)) "Warning icon.")
(defface my/problems-info '((t)) "Info icon.")
(defface my/problems-dim '((t)) "Where, and which checker.")
(defface my/problems-file '((t :weight semibold)) "A file's row.")
(defface my/problems-badge '((t)) "A count.")
(defface my/problems-off '((t)) "A filter that's off.")
(defface my/problems-hover '((t)) "Under the mouse.")
(defface my/problems-source '((t)) "rustc E0308.")
(defface my/problems-count '((t)) "A count in the title bar.")
(defface my/problems-ok '((t)) "The tick when there's nothing.")
(defface my/problems-selected '((t)) "The row you're on.")

(defun my/problems-apply-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
           (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
           (red (my/tab--face-color 'error :foreground "#f75464"))
           (yellow (my/tab--face-color 'warning :foreground "#d5b778"))
           (blue (my/tab--face-color 'link :foreground "#56a8f5")))
      ;; softened: they mark, they don't shout
      (set-face-attribute 'my/problems-error nil :foreground (my/tab--mix red bg 0.75))
      (set-face-attribute 'my/problems-warning nil :foreground (my/tab--mix yellow bg 0.85))
      (set-face-attribute 'my/problems-info nil :foreground (my/tab--mix blue bg 0.8))
      (set-face-attribute 'my/problems-dim nil :foreground (my/tab--mix fg bg 0.45))
      (set-face-attribute 'my/problems-file nil :foreground (my/tab--mix fg bg 0.9))
      (set-face-attribute 'my/problems-badge nil :foreground (my/tab--mix fg bg 0.85)
                          :background (my/tab--mix fg bg 0.14) :height 0.8
                          :box (list :line-width '(4 . 1) :color (my/tab--mix fg bg 0.14)))
      (set-face-attribute 'my/problems-off nil :foreground (my/tab--mix fg bg 0.3) :strike-through t)
      (set-face-attribute 'my/problems-hover nil :background (my/tab--mix fg bg 0.10) :extend t)
      (set-face-attribute 'my/problems-source nil :foreground (my/tab--mix fg bg 0.6))
      (set-face-attribute 'my/problems-count nil :foreground (my/tab--mix fg bg 0.85))
      (set-face-attribute 'my/problems-ok nil :foreground (my/tab--mix (my/tab--face-color 'success :foreground "#6aab73") bg 0.85))
      (set-face-attribute 'my/problems-selected nil :background (my/tab--mix fg bg 0.07) :extend t))))

(defun my/problems--icon (severity)
  (let ((name (pcase severity ('error "nf-cod-error") ('warning "nf-cod-warning") (_ "nf-cod-info")))
        (face (pcase severity ('error 'my/problems-error) ('warning 'my/problems-warning)
                (_ 'my/problems-info))))
    (or (ignore-errors
          (let ((icon (nerd-icons-codicon name)))
            (propertize (substring-no-properties icon)
                        'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                                :height 1.0 :inherit ,face))))
        (propertize (pcase severity ('error "✗") ('warning "⚠") (_ "ⓘ")) 'face face))))

;;; drawing
;; ---------------------------------------------------------------------

(defvar-local my/problems--root nil "The project shown (nil: everything).")
(defvar-local my/problems--shown '(error warning info) "Severities listed.")
(defvar-local my/problems--filter nil "Only problems whose message has this.")
(defvar-local my/problems--folded nil "Files folded shut.")
(defvar-local my/problems--stamp nil "What was last drawn, to skip redraws.")

(defun my/problems--counts (groups)
  (let ((counts (list 0 0 0)))
    (dolist (g groups)
      (dolist (p (cdr g))
        (cl-incf (nth (my/problems--rank (plist-get p :severity)) counts))))
    counts))

(defun my/problems--visible (problems)
  (seq-filter (lambda (p)
                (and (memq (plist-get p :severity) my/problems--shown)
                     (or (null my/problems--filter)
                         (string-match-p (regexp-quote my/problems--filter)
                                         (downcase (plist-get p :message))))))
              problems))

(defun my/problems--message (p)
  "PROBLEM's message on one line: the first line, then the rest dimmed."
  (let* ((lines (seq-remove #'string-blank-p
                            (mapcar #'string-trim (split-string (plist-get p :message) "\n"))))
         (rest (cdr lines)))
    (concat (car lines)
            (if rest (propertize (concat "  · " (string-join rest "  ")) 'face 'my/problems-dim) ""))))

(defun my/problems--where (p)
  "rustc E0308   Ln 5, Col 25"
  (concat (propertize (string-trim (concat (plist-get p :source) " " (or (plist-get p :code) "")))
                      'face 'my/problems-source)
          "   "
          (propertize (format "Ln %d, Col %d" (plist-get p :line) (plist-get p :col))
                      'face 'my/problems-dim)))

(defun my/problems--right (string)
  "STRING against the right edge, two columns in."
  (concat (propertize " " 'display `(space :align-to (- right (,(+ (string-pixel-width string)
                                                                   (* 2 (frame-char-width)))))))
          string))

(defun my/problems--line (text &rest props)
  (insert (apply #'propertize (concat text "\n") 'mouse-face 'my/problems-hover props)))

(defun my/problems-title ()
  "The counts for the panel's title bar; each one shows or hides its kind."
  (with-current-buffer (get-buffer-create my/problems-buffer)
   (let ((counts (my/problems--counts (car my/problems--stamp))))
    (mapconcat
     (lambda (severity)
       (let* ((on (memq severity my/problems--shown))
              (n (nth (my/problems--rank severity) counts)))
         (propertize (concat " " (my/problems--icon severity) " "
                             (propertize (format "%d" n) 'face (if on 'my/problems-count 'my/problems-off))
                             " ")
                     'mouse-face 'my/problems-hover
                     'help-echo (format "%s %ss: click to %s them" n severity (if on "hide" "show"))
                     'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [header-line mouse-1]
                                              (lambda () (interactive)
                                                (with-current-buffer my/problems-buffer
                                                  (my/problems-toggle severity))))
                                  map))))
     '(error warning info) " "))))

(defun my/problems--render (&optional force)
  (let* ((groups (my/problems-collect my/problems--root))
         (stamp (list groups my/problems--shown my/problems--filter my/problems--folded)))
    (when (or force (not (equal stamp my/problems--stamp)))
      (setq my/problems--stamp stamp)
      (let* ((inhibit-read-only t)
             (window (get-buffer-window (current-buffer) t))
             (here (get-text-property (if window (window-point window) (point)) 'my/problems-key))
             (line (line-number-at-pos (if window (window-point window) (point))))
             (shown 0))
        (erase-buffer)
        (when my/problems--filter
          (insert (propertize (format "  Showing problems matching \"%s\"   (/ to change)\n" my/problems--filter)
                              'face 'my/problems-dim)))
        (dolist (g groups)
          (let* ((file (car g))
                 (problems (my/problems--visible (cdr g)))
                 (folded (member file my/problems--folded))
                 (rel (if my/problems--root (file-relative-name file my/problems--root)
                        (abbreviate-file-name file)))
                 (dir (file-name-directory rel)))
            (when problems
              (cl-incf shown (length problems))
              (my/problems--line
               (concat "  " (my/problems--chevron folded) " "
                       (or (ignore-errors (nerd-icons-icon-for-file (file-name-nondirectory file))) " ")
                       "  " (propertize (file-name-nondirectory file) 'face 'my/problems-file)
                       (if dir (propertize (concat "  " (directory-file-name dir)) 'face 'my/problems-dim) "")
                       "  " (propertize (format " %d " (length problems)) 'face 'my/problems-badge))
               'my/problems-file file 'my/problems-key (list 'file file)
               'help-echo (abbreviate-file-name file))
              (unless folded
                (dolist (p problems)
                  (my/problems--line
                   (concat "       " (my/problems--icon (plist-get p :severity)) "  "
                           (my/problems--message p)
                           (my/problems--right (my/problems--where p)))
                   'my/problems-item p
                   'help-echo (plist-get p :message)
                   'my/problems-key (list 'item file (plist-get p :line) (plist-get p :col)
                                          (plist-get p :message))))))))
        (when (zerop shown)
          (insert "\n   " (my/problems--empty-icon) "  "
                  (propertize (cond (my/problems--filter "Nothing matches the filter.")
                                    (groups "Everything here is hidden by the filters in the title bar.")
                                    (t "No problems have been detected in this project."))
                              'face 'my/problems-dim)
                  "\n"))
        ;; back where we were
        (goto-char (point-min))
        (let ((pos (and here (my/problems--find here))))
          (if pos (goto-char pos) (forward-line (1- line))))
        (when window (set-window-point window (point)))
        ;; the title bar's counts
        (when (fboundp 'my/panel-decorate) (my/panel-decorate))
        (force-mode-line-update t)))))

(defun my/problems--chevron (folded)
  (or (ignore-errors
        (let ((icon (nerd-icons-codicon (if folded "nf-cod-chevron_right" "nf-cod-chevron_down"))))
          (propertize (substring-no-properties icon)
                      'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                              :inherit my/problems-dim))))
      (propertize (if folded "›" "⌄") 'face 'my/problems-dim)))

(defun my/problems--empty-icon ()
  (or (ignore-errors
        (let ((icon (nerd-icons-codicon "nf-cod-check")))
          (propertize (substring-no-properties icon)
                      'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                              :inherit my/problems-ok))))
      "✓"))

(defun my/problems--find (key)
  "Where the row with KEY starts (compared with `equal'), or nil."
  (let ((pos (point-min)) found)
    (while (and pos (not found))
      (if (equal (get-text-property pos 'my/problems-key) key)
          (setq found pos)
        (setq pos (next-single-property-change pos 'my/problems-key))))
    found))

;;; acting on it
;; ---------------------------------------------------------------------

(defun my/problems--goto (problem &optional stay)
  "Show PROBLEM's spot in the code area; go there unless STAY."
  (let* ((panel (selected-window))
         (window (or (and (fboundp 'my/activity--editor-window) (my/activity--editor-window))
                     (get-mru-window nil nil t))))
    (select-window window)
    (find-file (plist-get problem :file))
    (goto-char (point-min))
    (forward-line (1- (plist-get problem :line)))
    (move-to-column (1- (plist-get problem :col)))
    (recenter)
    (pulse-momentary-highlight-one-line (point))
    (when stay (select-window panel))))

(defun my/problems-open ()
  "Go to the problem at point (or fold the file at point)."
  (interactive)
  (cond ((get-text-property (point) 'my/problems-item)
         (my/problems--goto (get-text-property (point) 'my/problems-item)))
        ((get-text-property (point) 'my/problems-file) (my/problems-fold))))

(defun my/problems-preview ()
  "Show the problem at point in the code, staying in the panel."
  (interactive)
  (cond ((get-text-property (point) 'my/problems-item)
         (my/problems--goto (get-text-property (point) 'my/problems-item) t))
        ((get-text-property (point) 'my/problems-file) (my/problems-fold))))

(defun my/problems-fold ()
  "Fold or unfold the file at point (or the file of the problem at point)."
  (interactive)
  (let ((file (or (get-text-property (point) 'my/problems-file)
                  (plist-get (get-text-property (point) 'my/problems-item) :file))))
    (when file
      (setq my/problems--folded (if (member file my/problems--folded)
                                    (delete file my/problems--folded)
                                  (cons file my/problems--folded)))
      (my/problems--render t)
      (when-let ((pos (my/problems--find (list 'file file)))) (goto-char pos)))))

(defun my/problems-toggle (severity)
  "Show or hide problems of SEVERITY."
  (setq my/problems--shown (if (memq severity my/problems--shown)
                               (delq severity (copy-sequence my/problems--shown))
                             (cons severity my/problems--shown)))
  (my/problems--render t))

(defun my/problems-toggle-errors () (interactive) (my/problems-toggle 'error))
(defun my/problems-toggle-warnings () (interactive) (my/problems-toggle 'warning))
(defun my/problems-toggle-info () (interactive) (my/problems-toggle 'info))

(defun my/problems-filter (text)
  "Only list problems whose message contains TEXT (empty: all)."
  (interactive (list (read-string "Problems containing: " my/problems--filter)))
  (setq my/problems--filter (unless (string-blank-p text) (downcase (string-trim text))))
  (my/problems--render t))

(defun my/problems-refresh ()
  (interactive)
  (my/problems--render t))

(defun my/problems-click (event)
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (select-window (posn-window (event-start event)))
    (when pos
      (goto-char pos)
      (if-let ((action (get-text-property pos 'my/problems-click)))
          (funcall action)
        (my/problems-open)))))

(defvar my/problems-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'my/problems-click)
    map))

(define-derived-mode my/problems-mode special-mode "Problems"
  "Every problem in the project."
  (setq-local truncate-lines t
              cursor-type nil
              evil-normal-state-cursor '(nil)
              line-spacing 0.2
              revert-buffer-function (lambda (&rest _) (my/problems--render t)))
  (display-line-numbers-mode -1)
  (face-remap-add-relative 'hl-line 'my/problems-selected)
  (hl-line-mode 1)
  (add-hook 'window-size-change-functions
            (lambda (w) (with-current-buffer (window-buffer w)
                          (when (derived-mode-p 'my/problems-mode) (my/problems--render t))))
            nil t))

;;; keeping it current
;; ---------------------------------------------------------------------

(defvar my/problems--timer nil)

(defun my/problems--soon (&rest _)
  "Redraw shortly (diagnostics arrive in bursts)."
  (when (get-buffer-window my/problems-buffer t)
    (when my/problems--timer (cancel-timer my/problems--timer))
    (setq my/problems--timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (when-let ((b (get-buffer my/problems-buffer)))
                                   (with-current-buffer b (my/problems--render))))))))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-diagnostics-updated-hook #'my/problems--soon))
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-after-syntax-check-hook #'my/problems--soon))

;;; the panel
;; ---------------------------------------------------------------------

(defun my/problems--window ()
  (seq-find (lambda (w) (window-parameter w 'my-problems-panel)) (window-list nil 'nomini)))

(defun my/problems-p () (my/problems--window))

(defun my/problems--root-here ()
  (let ((root (or (and (fboundp 'my/project-root) (my/project-root))
                  (and buffer-file-name (bound-and-true-p lsp-mode)
                       (fboundp 'lsp-workspace-root) (lsp-workspace-root)))))
    (and root (file-name-as-directory (file-truename root)))))

(defun my/problems--display (buffer)
  (let ((window (display-buffer-in-side-window
                 buffer `((side . bottom) (slot . 1) (window-height . ,my/problems-height)
                          (preserve-size . (nil . t))))))
    (set-window-parameter window 'my-problems-panel t)
    (set-window-parameter window 'no-delete-other-windows t)
    window))

(defun my/problems-toggle-panel ()
  "Show or hide the Problems panel for this project."
  (interactive)
  (if-let ((window (my/problems--window)))
      (delete-window window)
    (let ((root (my/problems--root-here))
          (buffer (get-buffer-create my/problems-buffer)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'my/problems-mode) (my/problems-mode))
        (setq my/problems--root root default-directory (or root default-directory)))
      (select-window (my/problems--display buffer))
      (my/problems--render t))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/problems-mode 'normal)
  (evil-define-key 'normal my/problems-mode-map
    (kbd "RET") #'my/problems-open
    (kbd "<return>") #'my/problems-open
    (kbd "TAB") #'my/problems-preview
    (kbd "<tab>") #'my/problems-preview
    "za" #'my/problems-fold
    "e" #'my/problems-toggle-errors
    "w" #'my/problems-toggle-warnings
    "i" #'my/problems-toggle-info
    "/" #'my/problems-filter
    "g" #'my/problems-refresh
    "q" #'my/problems-toggle-panel
    [down-mouse-1] #'ignore
    [mouse-1] #'my/problems-click)
  (evil-define-key 'normal 'global
    (kbd "<leader>xw") #'my/problems-toggle-panel))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC x w" "problems"))

(my/problems-apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/problems-apply-theme))

(provide 'problems)

;;; problems.el ends here
