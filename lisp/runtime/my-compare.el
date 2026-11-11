;;; runtime/my-compare.el  -*- lexical-binding: t; -*-

;; compare runs side by side, keep the last run, rerun on save

;; Three things for "change the code, run it, see what changed":
;;
;; Side by side: a run configuration with :group runs other
;; configurations at once and shows their output next to each other in
;; a Compare tab.  In .emacs-run.eld:
;;
;;   ((:name "bench: sqlite"   :command "cd bench/sqlite && cargo run" :watch t)
;;    (:name "bench: postgres" :command "cd bench/postgres && cargo run" :watch t)
;;    (:name "bench: both" :group ("bench: sqlite" "bench: postgres")))
;;
;; This run vs last run: before a configuration runs again, its last
;; finished output is kept, so the Compare tab can put it beside the new
;; one.  Lines that differ are tinted on both sides, and the header says
;; how long each took.
;;
;; Rerun on save: configurations marked :watch t (or toggled with w)
;; rerun whenever you save a file in the project (target/ and .git/
;; aside).  A run still going is stopped first.
;;
;; Cargo's own chatter (Compiling..., warnings) is hidden in the Compare
;; tab so the program's output stands out; n shows it again.
;;
;; Keys
;; ---------------------------------------------------------------------
;;   SPC r c  open the Compare tab        (c in the Run panel)
;;   w        rerun on save, on / off      (also in the Run panel)
;; In the Compare tab:
;;   r  run again      s  side by side      l  this side: last run vs this one
;;   n  show / hide cargo's chatter         w  rerun on save   q  close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'my-task)
(require 'my-run)

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function my/tab--mix "tabs")
(declare-function my/tab--face-color "tabs")

;;; the last finished run of each configuration
;; ---------------------------------------------------------------------

(defvar my/compare--previous (make-hash-table :test #'equal)
  "(ROOT . NAME) -> (:text :status :duration) of its last finished run
before the current one.")

(defun my/compare--latest (root name &optional finished)
  "The newest task of configuration NAME in ROOT (only FINISHED ones if asked)."
  (seq-find (lambda (task)
              (and (equal (my/task-name task) name)
                   (or (not finished) (memq (my/task-status task) '(succeeded failed)))))
            (my/run--tasks root)))

(defun my/compare--snapshot (task)
  (list :text (with-current-buffer (my/task-buffer task) (buffer-string))
        :status (my/task-status task)
        :duration (my/task-duration task)))

(defun my/compare--keep-last (&rest args)
  "Before a task starts, keep its configuration's last finished run."
  (let ((name (plist-get args :name))
        (root (plist-get args :project)))
    (when (and name root)
      (when-let ((done (my/compare--latest (file-name-as-directory root) name t)))
        (when (buffer-live-p (my/task-buffer done))
          (puthash (cons (file-name-as-directory root) name)
                   (my/compare--snapshot done) my/compare--previous))))))

(defun my/compare--keep-last-run (spec root)
  "Before SPEC runs again (which drops its old run), keep its last finished run."
  (my/compare--keep-last :name (plist-get spec :name) :project root))

;; my/run-start drops the old run first, so keep it before that;
;; restarts from the panel go straight to my/task-start
(advice-add 'my/run-start :before #'my/compare--keep-last-run)
(advice-add 'my/task-start :before #'my/compare--keep-last)

;;; rerun on save
;; ---------------------------------------------------------------------

(defvar my/compare--watch (make-hash-table :test #'equal)
  "ROOT -> names of its configurations that rerun on save.")

(defun my/compare--watch-names (root)
  (let ((root (file-name-as-directory root)))
    (unless (gethash root my/compare--watch)
      ;; first time: whatever .emacs-run.eld marks :watch t
      (puthash root (cons 'init (mapcar (lambda (s) (plist-get s :name))
                                        (seq-filter (lambda (s) (plist-get s :watch))
                                                    (ignore-errors (my/run-configs root)))))
               my/compare--watch))
    (cdr (gethash root my/compare--watch))))

(defun my/compare-watched-p (root name)
  (and root (member name (my/compare--watch-names root))))

(defun my/compare--set-watch (root names)
  (puthash (file-name-as-directory root) (cons 'init names) my/compare--watch))

(defun my/compare-toggle-watch (&optional names root)
  "Rerun on save for the configuration at point (or NAMES), on or off."
  (interactive)
  (let* ((root (or root (my/compare--root-here)))
         (names (or names (my/compare--names-here)
                    (user-error "Not on a configuration")))
         (on (not (seq-every-p (lambda (n) (my/compare-watched-p root n)) names)))
         (current (my/compare--watch-names root)))
    (my/compare--set-watch root (if on (seq-uniq (append current names))
                                  (seq-difference current names)))
    (message "%s %s on save" (string-join names ", ") (if on "reruns" "no longer reruns"))
    (when (fboundp 'my/run-panel--refresh-all) (my/run-panel--refresh-all))
    (my/compare--refresh-all t)))

(defvar my/compare--save-timers (make-hash-table :test #'equal))

(defun my/compare--after-save ()
  "Saving a file in a project with watched configurations reruns them."
  (when buffer-file-name
    (let ((file (file-truename buffer-file-name)))
      (maphash
       (lambda (root entry)
         (when (and (cdr entry)
                    (string-prefix-p (file-truename root) file)
                    (not (string-match-p "/\\(target\\|\\.git\\|node_modules\\)/"
                                         (substring file (length (file-truename root))))))
           (when-let ((old (gethash root my/compare--save-timers))) (cancel-timer old))
           (puthash root (run-at-time 0.4 nil #'my/compare--rerun-watched root)
                    my/compare--save-timers)))
       my/compare--watch))))

(add-hook 'after-save-hook #'my/compare--after-save)

(defun my/compare--spec (root name)
  (seq-find (lambda (s) (equal (plist-get s :name) name)) (my/run-configs root)))

(defun my/compare--rerun-watched (root)
  (let ((names (my/compare--watch-names root)))
    (dolist (name names)
      (when-let ((spec (my/compare--spec root name)))
        (if (plist-get spec :group)
            (dolist (member (plist-get spec :group))
              (unless (member member names)
                (when-let ((m (my/compare--spec root member))) (my/run-start m root))))
          (my/run-start spec root))))
    (message "Saved: rerunning %s" (string-join names ", "))))

;;; groups: run together, look side by side
;; ---------------------------------------------------------------------

(defun my/compare-run-group (spec root)
  "Run every member of group SPEC and show the first two side by side."
  (let ((names (plist-get spec :group)))
    (dolist (name names)
      (if-let ((member (my/compare--spec root name)))
          (my/run-start member root)
        (message "No configuration called %s" name)))
    (my/compare-show root (list :mode 'sides :names (seq-take names 2)))))

;;; the Compare tab
;; ---------------------------------------------------------------------

(defface my/compare-title '((t :weight semibold)) "Side titles.")
(defface my/compare-dim '((t)) "Quiet header text.")
(defface my/compare-left '((t)) "A line only the left side has.")
(defface my/compare-right '((t)) "A line only the right side has.")

(defun my/compare-apply-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
           (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
           (warm (my/tab--face-color 'warning :foreground "#d5b778"))
           (cool (my/tab--face-color 'link :foreground "#56a8f5")))
      (set-face-attribute 'my/compare-title nil :foreground (my/tab--mix fg bg 0.75) :height 0.9)
      (set-face-attribute 'my/compare-dim nil :foreground (my/tab--mix fg bg 0.45) :height 0.9)
      (set-face-attribute 'my/compare-left nil :background (my/tab--mix warm bg 0.13) :extend t)
      (set-face-attribute 'my/compare-right nil :background (my/tab--mix cool bg 0.13) :extend t))))

(defvar my/compare--states (make-hash-table :test #'equal)
  "ROOT -> (:mode sides|last :names (A B) | :name N, :noise BOOL).")

(defvar-local my/compare--root nil)
(defvar-local my/compare--side nil "left or right.")
(defvar-local my/compare--shown nil "What was last drawn, to skip redraws.")

(defun my/compare--sources (root)
  "(LEFT RIGHT), each (:title :sub :task TASK) or (:title :sub :snapshot PLIST)."
  (let ((state (gethash root my/compare--states)))
    (pcase (plist-get state :mode)
      ('last
       (let ((name (plist-get state :name)))
         (list (list :title name :sub "last run"
                     :snapshot (gethash (cons root name) my/compare--previous))
               (list :title name :sub "this run"
                     :task (my/compare--latest root name)
                     :against (gethash (cons root name) my/compare--previous)))))
      (_
       (mapcar (lambda (name) (list :title name :sub "latest run" :task (my/compare--latest root name)))
               (plist-get state :names))))))

(defconst my/compare--noise
  (concat "\\`\\s-*\\(Compiling\\|Checking\\|Finished\\|Running\\|Blocking\\|Downloaded\\|"
          "Downloading\\|Updating\\|Locking\\|Adding\\|Fresh\\|Building\\|Packaging\\|Removed\\)\\_>")
  "Cargo's progress lines.")

(defun my/compare--meta-p (line)
  "Non-nil for the task's own lines ($ command, in dir, finished)."
  (or (and (> (length line) 0)
           (let ((f (get-text-property 0 'face line)))
             (or (eq f 'my/task-meta) (and (consp f) (memq 'my/task-meta f)))))
      (string-match-p "\\`\\(\\$ \\|[✓✗■] \\(finished\\|exited\\|stopped\\|failed\\)\\)" line)))

(defun my/compare--clean (text noise)
  "TEXT without task meta lines, and without cargo's chatter unless NOISE."
  (let ((in-warning nil) out)
    (dolist (line (split-string text "\n"))
      (cond
       ((my/compare--meta-p line) nil)
       (noise (push line out))
       ((string-match-p "\\`warning\\(\\[\\|:\\)" line) (setq in-warning t))
       ;; a warning goes on while its lines are blank, indented or code excerpts
       ((and in-warning (or (string-blank-p line)
                            (string-match-p "\\`\\([ \t]\\|[0-9]+ *|\\)" line)))
        nil)
       (t (setq in-warning nil)
          (unless (string-match-p my/compare--noise line) (push line out)))))
    (string-trim (string-join (nreverse out) "\n") "\n+" "\n+")))

(defun my/compare--source-text (source noise)
  (cond ((plist-get source :task)
         (let ((buffer (my/task-buffer (plist-get source :task))))
           (if (buffer-live-p buffer)
               (my/compare--clean (with-current-buffer buffer (buffer-string)) noise)
             "")))
        ((plist-get source :snapshot)
         (my/compare--clean (plist-get (plist-get source :snapshot) :text) noise))
        (t nil)))

(defun my/compare--status (source)
  "Header text: how the run went and how long it took."
  (let* ((task (plist-get source :task))
         (snap (plist-get source :snapshot))
         (status (cond (task (my/task-status task)) (snap (plist-get snap :status))))
         (duration (cond (task (my/task-duration task)) (snap (plist-get snap :duration))))
         (against (plist-get source :against)))
    (cond
     ((null status) "")
     (t (concat (pcase status
                  ('succeeded "✓ ") ('failed "✗ ") ('cancelled "■ ")
                  (_ "● running "))
                (if duration (my/task-format-duration duration) "")
                (when (and against duration (plist-get against :duration)
                           (memq status '(succeeded failed)))
                  (let ((delta (- duration (plist-get against :duration))))
                    (format "  (%s%.2fs vs last)" (if (>= delta 0) "+" "−") (abs delta)))))))))

(defun my/compare--header (source)
  (concat " " (propertize (upcase (plist-get source :title)) 'face 'my/compare-title)
          (propertize (concat "  " (plist-get source :sub)) 'face 'my/compare-dim)
          (propertize "   " 'face 'my/compare-dim)
          (my/compare--status source)
          (if (my/compare-watched-p my/compare--root (plist-get source :title))
              (propertize "   ↻ on save" 'face 'my/compare-dim)
            "")))

(defun my/compare--diff (left right)
  "Line numbers (1-based) only LEFT has, and only RIGHT has: (LEFTS . RIGHTS)."
  (let ((a (make-temp-file "cmp-a")) (b (make-temp-file "cmp-b")) lefts rights)
    (unwind-protect
        (progn
          (with-temp-file a (insert left "\n"))
          (with-temp-file b (insert right "\n"))
          (with-temp-buffer
            (call-process "diff" nil t nil
                          "--old-line-format=<%dn\n" "--new-line-format=>%dn\n"
                          "--unchanged-line-format=" a b)
            (goto-char (point-min))
            (while (re-search-forward "^\\([<>]\\)\\([0-9]+\\)$" nil t)
              (if (equal (match-string 1) "<")
                  (push (string-to-number (match-string 2)) lefts)
                (push (string-to-number (match-string 2)) rights)))))
      (delete-file a) (delete-file b))
    (cons lefts rights)))

(defun my/compare--fill (buffer source text lines face)
  "Put TEXT in BUFFER with SOURCE's header, tinting LINES with FACE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (window (get-buffer-window buffer t)))
      (let ((at-end (and window (>= (window-point window) (1- (point-max))))))
        (erase-buffer)
        (insert (or text (propertize (if (eq my/compare--side 'left)
                                         "\n  No earlier run kept yet: run it again and the\n  last one shows up here.\n"
                                       "\n  Not run yet: press r.\n")
                                     'face 'my/compare-dim)))
        (dolist (n lines)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- n))
            (let ((ov (make-overlay (line-beginning-position) (min (point-max) (1+ (line-end-position))))))
              (overlay-put ov 'face face))))
        (setq header-line-format (my/compare--header source))
        (when (and window at-end) (set-window-point window (point-max)))))))

(defun my/compare--render (root &optional force)
  "Redraw ROOT's two sides (only if something changed, unless FORCE)."
  (let* ((state (gethash root my/compare--states))
         (noise (plist-get state :noise))
         (sources (my/compare--sources root))
         (texts (mapcar (lambda (s) (my/compare--source-text s noise)) sources))
         (stamp (list state texts (mapcar #'my/compare--status sources)
                      (my/compare--watch-names root)))
         (left (get-buffer (my/compare--buffer-name root 'left)))
         (right (get-buffer (my/compare--buffer-name root 'right))))
    (when (and left right
               (or force (not (equal stamp (buffer-local-value 'my/compare--shown left)))))
      (let ((diff (if (and (car texts) (cadr texts))
                      (my/compare--diff (car texts) (cadr texts))
                    '(nil))))
        (my/compare--fill left (car sources) (car texts) (car diff) 'my/compare-left)
        (my/compare--fill right (cadr sources) (cadr texts) (cdr diff) 'my/compare-right)
        (dolist (b (list left right))
          (with-current-buffer b (setq my/compare--shown stamp)))))))

(defun my/compare--refresh-all (&optional force)
  (maphash (lambda (root _) (my/compare--render root force)) my/compare--states))

(defvar my/compare--timer nil)

(defun my/compare--tick ()
  (if (seq-some (lambda (w) (eq (buffer-local-value 'major-mode (window-buffer w)) 'my/compare-mode))
                (window-list-1 nil 'nomini t))
      (my/compare--refresh-all)
    (cancel-timer my/compare--timer)
    (setq my/compare--timer nil)))

(define-derived-mode my/compare-mode special-mode "Compare"
  "One side of a comparison of two runs."
  ;; long error lines wrap, so they can be read whole
  (setq-local truncate-lines nil word-wrap t)
  (display-line-numbers-mode -1))

(defun my/compare--buffer-name (root side)
  (format "*compare · %s · %s*" (my/run--project-name root) side))

(defun my/compare--buffer (root side)
  (let ((buffer (get-buffer-create (my/compare--buffer-name root side))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/compare-mode) (my/compare-mode))
      (setq my/compare--root root my/compare--side side default-directory root))
    buffer))

(defun my/compare--tab-name (root)
  (format "Compare · %s" (my/run--project-name root)))

(defun my/compare-show (root state)
  "Show ROOT's Compare tab with STATE (see `my/compare--states')."
  (let* ((root (file-name-as-directory root))
         (old (gethash root my/compare--states)))
    (puthash root (plist-put (copy-sequence state) :noise (plist-get old :noise)) my/compare--states)
    (let ((left (my/compare--buffer root 'left))
          (right (my/compare--buffer root 'right))
          (index (seq-position (mapcar (lambda (tab) (alist-get 'name tab)) (funcall tab-bar-tabs-function))
                               (my/compare--tab-name root))))
      (if index
          (tab-bar-select-tab (1+ index))
        (tab-bar-new-tab)
        (tab-bar-rename-tab (my/compare--tab-name root)))
      (when (fboundp 'my/session--leave-side-window) (my/session--leave-side-window))
      (delete-other-windows)
      (switch-to-buffer left)
      (set-window-buffer (split-window-right) right)
      (my/compare--render root t)
      (unless my/compare--timer
        (setq my/compare--timer (run-at-time 0.5 0.5 #'my/compare--tick))))))

;;; commands
;; ---------------------------------------------------------------------

(defun my/compare--root-here ()
  (or my/compare--root (bound-and-true-p my/run-panel--root) (my/run--root)))

(defun my/compare--names-here ()
  "The configuration names at point (Run panel row, or the Compare tab's)."
  (cond
   ((derived-mode-p 'my/compare-mode)
    (let ((state (gethash my/compare--root my/compare--states)))
      (if (eq (plist-get state :mode) 'last)
          (list (plist-get state :name))
        (plist-get state :names))))
   ((derived-mode-p 'my/run-panel-mode)
    (let ((item (get-text-property (point) 'my-run-item)))
      (cond ((my/task-p item) (list (my/task-name item)))
            ((plist-get item :group) (plist-get item :group))
            (item (list (plist-get item :name))))))))

(defun my/compare-open ()
  "Open the Compare tab: the group side by side, or this run vs the last."
  (interactive)
  (let* ((root (my/compare--root-here))
         (item (and (derived-mode-p 'my/run-panel-mode) (get-text-property (point) 'my-run-item)))
         (group (or (and (listp item) (plist-get item :group) item)
                    (seq-find (lambda (s) (plist-get s :group)) (my/run-configs root)))))
    (cond
     ((and item (my/task-p item))
      (my/compare-show root (list :mode 'last :name (my/task-name item))))
     ((and item (not (plist-get item :group)))
      (my/compare-show root (list :mode 'last :name (plist-get item :name))))
     (group (my/compare-show root (list :mode 'sides :names (seq-take (plist-get group :group) 2))))
     (t (let ((spec (my/run--read-spec root "Compare runs of: ")))
          (my/compare-show root (list :mode 'last :name (plist-get spec :name))))))))

(defun my/compare-rerun ()
  "Run again what this Compare tab shows."
  (interactive)
  (let ((root my/compare--root))
    (dolist (name (my/compare--names-here))
      (when-let ((spec (my/compare--spec root name)))
        (my/run-start spec root)))))

(defun my/compare-sides ()
  "Back to side by side (the project's group)."
  (interactive)
  (let* ((root my/compare--root)
         (group (seq-find (lambda (s) (plist-get s :group)) (my/run-configs root))))
    (unless group (user-error "No :group configuration in this project's .emacs-run.eld"))
    (my/compare-show root (list :mode 'sides :names (seq-take (plist-get group :group) 2)))))

(defun my/compare-last ()
  "This side's configuration: its last run next to this one."
  (interactive)
  (let* ((root my/compare--root)
         (state (gethash root my/compare--states))
         (name (if (eq (plist-get state :mode) 'last)
                   (plist-get state :name)
                 (nth (if (eq my/compare--side 'left) 0 1) (plist-get state :names)))))
    (my/compare-show root (list :mode 'last :name name))))

(defun my/compare-toggle-noise ()
  "Show or hide cargo's Compiling / warning lines."
  (interactive)
  (let ((state (gethash my/compare--root my/compare--states)))
    (puthash my/compare--root (plist-put state :noise (not (plist-get state :noise))) my/compare--states)
    (my/compare--render my/compare--root t)
    (message (if (plist-get state :noise) "Showing cargo's output too" "Program output only"))))

(defun my/compare-quit ()
  (interactive)
  (if (> (length (funcall tab-bar-tabs-function)) 1)
      (tab-bar-close-tab)
    (quit-window)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/compare-mode 'normal)
  (evil-define-key 'normal my/compare-mode-map
    "r" #'my/compare-rerun
    "s" #'my/compare-sides
    "l" #'my/compare-last
    "n" #'my/compare-toggle-noise
    "w" #'my/compare-toggle-watch
    "q" #'my/compare-quit)
  (evil-define-key 'normal 'global
    (kbd "<leader>rc") #'my/compare-open))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC r c" "compare runs"))

(my/compare-apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/compare-apply-theme))

(provide 'my-compare)

;;; my-compare.el ends here
