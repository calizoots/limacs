;;; runtime/my-session.el  -*- lexical-binding: t; -*-

;; sessions: save and restore every tab, and pick one at startup

;; A session is every tab with its windows, project, pin, Run panel and
;; NeoTree.  The current one is saved as "last" every few minutes and on
;; quit (the one before it is kept as "previous").  Save it under a name
;; to keep a workspace ("rust", "music + mail") and load it any time.
;;
;; At startup a picker offers the last session (RET), your saved ones,
;; or a fresh start.  Restoring asks for drives that are missing: plug
;; in the USB drive, unlock the VeraCrypt part (see my-drives).  Tabs
;; whose drive you skip are left out.
;;
;; Buffers are rebuilt from what they showed: files and folders reopen,
;; terminals start again in the same folder, the music page, Run panel
;; and Services page come back.  Running processes don't.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC q l  load a session (same picker as at startup)
;;   SPC q s  save this session under a name
;;   SPC q r  restore the last session
;;   SPC q d  delete a saved session

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'my-drives)

(defvar my/session-directory (locate-user-emacs-file "sessions/")
  "Where sessions are kept, one NAME.eld each.")

(defvar my/session-autosave-seconds 300
  "Save the session as \"last\" this often (and on quit).")

(defvar my/session-startup-picker t
  "Non-nil: offer the session picker when Emacs starts without files.")

(defvar my/session--active nil
  "Non-nil once this Emacs has a session worth saving.
Stays nil while the startup picker is open, so a crash there can't
overwrite the last session with an empty one.")

(defvar my/session--rotated nil
  "Non-nil once \"last\" was copied to \"previous\" this run.")

(defvar my/session-current nil
  "Name of the session loaded last, the default for SPC q s.")

;;; files
;; ---------------------------------------------------------------------

(defun my/session--file (name)
  (expand-file-name (concat name ".eld") my/session-directory))

(defun my/session--read (name)
  (let ((file (my/session--file name)))
    (when (file-exists-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))))))

(defun my/session--write (name data)
  (make-directory my/session-directory t)
  (let ((print-length nil) (print-level nil) (print-circle nil))
    (with-temp-file (my/session--file name)
      (insert ";; -*- mode: lisp-data -*-\n")
      (prin1 data (current-buffer))
      (insert "\n"))))

(defun my/session-names ()
  "Saved sessions, newest first (\"last\" and \"previous\" included)."
  (when (file-directory-p my/session-directory)
    (mapcar (lambda (file) (file-name-base file))
            (sort (directory-files my/session-directory t "\\.eld\\'")
                  (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                             (file-attribute-modification-time (file-attributes a))))))))

;;; what a buffer was
;; ---------------------------------------------------------------------

(defvar my/run-panel--root)
(defvar my/containers--root)
(defvar my/music-directory)
(defvar my/services-buffer)
(defvar my/services--timer)
(defvar my/services-interval)

(defun my/session--describe (buffer)
  "How to bring BUFFER back, as a list (KIND ARG), or nil if we can't."
  (with-current-buffer buffer
    (cond
     ((derived-mode-p 'neotree-mode) '(neotree))
     (buffer-file-name (list 'file buffer-file-name))
     ((derived-mode-p 'dired-mode) (list 'dired (expand-file-name default-directory)))
     ((derived-mode-p 'my/music-mode 'my/music-library-mode) '(music))
     ((derived-mode-p 'my/run-panel-mode 'my/task-log-mode)
      (list 'run (or (bound-and-true-p my/run-panel--root) default-directory)))
     ((derived-mode-p 'my/services-mode) '(services))
     ((derived-mode-p 'my/containers-mode)
      (list 'containers (and (stringp (bound-and-true-p my/containers--root)) my/containers--root)))
     ((derived-mode-p 'ghostel-mode 'vterm-mode 'shell-mode 'eshell-mode)
      (list 'terminal (expand-file-name default-directory)))
     ((equal (buffer-name) "*scratch*") '(scratch)))))

(defun my/session--desc-path (desc)
  "The file or folder DESC needs, if any."
  (pcase desc
    (`(,(or 'file 'dired 'run 'terminal) ,path) path)
    ('(music) (and (boundp 'my/music-directory) my/music-directory))))

(defun my/session--services-buffer ()
  (let ((buf (get-buffer-create my/services-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/services-mode) (my/services-mode)))
    (ignore-errors (my/services-refresh))
    (unless my/services--timer
      (setq my/services--timer (run-at-time my/services-interval my/services-interval
                                            #'my/services--tick)))
    buf))

(defun my/session--terminal (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (save-window-excursion
      (call-interactively (if (fboundp 'ghostel) #'ghostel #'shell))
      (current-buffer))))

(defun my/session--recreate (desc)
  "A live buffer for DESC, or nil."
  (pcase desc
    (`(file ,file) (and (file-exists-p file) (find-file-noselect file)))
    (`(dired ,dir) (and (file-directory-p dir) (dired-noselect dir)))
    ('(music) (and (fboundp 'my/music-page--buffer)
                   (progn (my/music--ensure-library) (my/music-page--buffer))))
    (`(run ,root) (and (fboundp 'my/run-panel--buffer) (file-directory-p root)
                       (my/run-panel--buffer root)))
    ('(services) (and (fboundp 'my/services-mode) (my/session--services-buffer)))
    (`(containers ,root) (and (fboundp 'my/containers--buffer) (my/containers--buffer root)))
    (`(terminal ,dir) (and (file-directory-p dir) (my/session--terminal dir)))
    ('(scratch) (get-scratch-buffer-create))))

;;; window states
;; ---------------------------------------------------------------------

(defun my/session--buffer-entries (state)
  "Every (buffer NAME . PROPS) entry in window STATE."
  (let (found)
    (cl-labels ((walk (x)
                  (when (consp x)
                    (if (and (eq (car x) 'buffer) (stringp (cadr x)))
                        (push x found)
                      (walk (car x))
                      (walk (cdr x))))))
      (walk state))
    (nreverse found)))

;;; saving
;; ---------------------------------------------------------------------

(defun my/session--tab-data (tab current-p)
  (let* ((params (cdr tab))
         (state (if current-p
                    (window-state-get (frame-root-window) 'writable)
                  (alist-get 'ws params)))
         (names (delete-dups (mapcar #'cadr (my/session--buffer-entries state))))
         (project (and (fboundp 'my/project-by-id)
                       (alist-get 'my-project-id params)
                       (my/project-by-id (alist-get 'my-project-id params)))))
    (list :name (alist-get 'name params)
          :explicit (alist-get 'explicit-name params)
          :pinned (alist-get 'my/pinned params)
          :project (plist-get project :root)
          :windows state
          :buffers (mapcar (lambda (name)
                             (cons name (and (get-buffer name)
                                             (my/session--describe (get-buffer name)))))
                           names))))

(defun my/session--data ()
  (let ((current (tab-bar--current-tab-index)))
    (list :version 1
          :saved (float-time)
          :current current
          :tabs (seq-map-indexed (lambda (tab i) (my/session--tab-data tab (= i current)))
                                 (funcall tab-bar-tabs-function)))))

(defun my/session--empty-p (data)
  "Non-nil if DATA is just a lone scratch tab (not worth saving)."
  (let ((tabs (plist-get data :tabs)))
    (and (= (length tabs) 1)
         (not (plist-get (car tabs) :project))
         (seq-every-p (lambda (entry) (member (cdr entry) '(nil (scratch))))
                      (plist-get (car tabs) :buffers)))))

(defun my/session-autosave ()
  "Save the session as \"last\" (the one before is kept as \"previous\")."
  (when my/session--active
    (let ((data (ignore-errors (my/session--data))))
      (when (and data (not (my/session--empty-p data)))
        (unless my/session--rotated
          (setq my/session--rotated t)
          (when (file-exists-p (my/session--file "last"))
            (copy-file (my/session--file "last") (my/session--file "previous") t)))
        (my/session--write "last" data)))))

(defun my/session-save (name)
  "Save every tab as session NAME."
  (interactive
   (list (completing-read (format-prompt "Save session as" my/session-current)
                          (seq-remove (lambda (n) (member n '("last" "previous")))
                                      (my/session-names))
                          nil nil nil nil my/session-current)))
  (setq name (string-trim name))
  (when (or (string-empty-p name) (member name '("last" "previous")))
    (user-error "Pick another name"))
  (when (and (file-exists-p (my/session--file name))
             (not (equal name my/session-current))
             (not (y-or-n-p (format "Replace session %s? " name))))
    (user-error "Not saved"))
  (my/session--write name (my/session--data))
  (setq my/session-current name
        my/session--active t)
  (message "Saved session %s" name))

;;; restoring
;; ---------------------------------------------------------------------

(defvar my/session--gone " *session: gone*"
  "Stands in for buffers that couldn't come back; its windows are removed.")

(defun my/session--fallback-buffer (root)
  (or (and root (file-directory-p root) (dired-noselect root))
      (get-scratch-buffer-create)))

(defun my/session--restore-tab (tab skipped-p)
  "Fill the current tab from TAB data.
SKIPPED-P tells whether a path's drive was skipped."
  (let* ((root (plist-get tab :project))
         (root (and root (file-directory-p root) root))
         (neotree nil)
         (map (mapcar
               (lambda (entry)
                 (let* ((desc (cdr entry))
                        (path (my/session--desc-path desc))
                        (buffer (cond ((equal desc '(neotree)) (setq neotree t) nil)
                                      ((and path (funcall skipped-p path)) nil)
                                      (t (ignore-errors (my/session--recreate desc))))))
                   (cons (car entry) (and (buffer-live-p buffer) (buffer-name buffer)))))
               (plist-get tab :buffers)))
         (state (copy-tree (plist-get tab :windows)))
         (gone (get-buffer-create my/session--gone)))
    ;; the project first: buffers and the tree belong to it
    (when (and root (fboundp 'my/project-set-current))
      (my/project-set-current (my/project-create root)))
    (when state
      (dolist (entry (my/session--buffer-entries state))
        (setcar (cdr entry) (or (cdr (assoc (cadr entry) map)) my/session--gone)))
      (condition-case err
          (window-state-put state (frame-root-window) 'safe)
        (error (message "Session: couldn't lay out tab %s: %s"
                        (plist-get tab :name) (error-message-string err))
               (delete-other-windows)
               (switch-to-buffer (my/session--fallback-buffer root)))))
    ;; windows whose buffer didn't come back go away
    (dolist (window (window-list nil 'nomini))
      (when (and (window-live-p window) (eq (window-buffer window) gone))
        (if (or (one-window-p t) (not (ignore-errors (delete-window window) t)))
            (set-window-buffer window (my/session--fallback-buffer root)))))
    (when (window-live-p (get-buffer-window gone))
      (switch-to-buffer (my/session--fallback-buffer root)))
    (kill-buffer gone)
    (when (plist-get tab :explicit)
      (tab-bar-rename-tab (plist-get tab :name)))
    (when (and neotree root (fboundp 'my/neotree-open))
      (save-selected-window (ignore-errors (my/neotree-open))))))

(defun my/session--needed-paths (data)
  "(PATH . PURPOSE) for everything DATA needs."
  (let (paths)
    (dolist (tab (plist-get data :tabs))
      (let ((purpose (or (and (plist-get tab :project)
                              (file-name-nondirectory (directory-file-name (plist-get tab :project))))
                         (plist-get tab :name))))
        (when (plist-get tab :project)
          (push (cons (plist-get tab :project) purpose) paths))
        (dolist (entry (plist-get tab :buffers))
          (when-let ((path (my/session--desc-path (cdr entry))))
            (push (cons path (if (equal (cdr entry) '(music)) "Music" purpose)) paths)))))
    (nreverse paths)))

(defun my/session-load (name)
  "Replace the tabs with session NAME's."
  (interactive (list (my/session--read-name "Load session: ")))
  (let ((data (or (my/session--read name)
                  (user-error "No session called %s" name))))
    (let* ((skipped (my/drive-ensure-paths (my/session--needed-paths data)))
           (skipped-p (lambda (path) (memq (my/drive-for-path path) skipped)))
           ;; tabs whose project is on a skipped drive (or gone) stay out
           (tabs (seq-remove (lambda (tab)
                               (let ((root (plist-get tab :project)))
                                 (and root (or (funcall skipped-p root)
                                               (not (file-directory-p root))))))
                             (plist-get data :tabs)))
           (current (min (or (plist-get data :current) 0) (max 0 (1- (length tabs)))))
           (tab-bar-new-tab-choice t))
      (unless tabs (user-error "Nothing in %s could be opened" name))
      (tab-bar-mode 1)
      ;; start from one plain tab, out of any side panel
      (tab-bar-close-other-tabs)
      (my/session--leave-side-window)
      (let ((tab (my/project-current-tab-safe)))
        (setf (alist-get 'my/pinned (cdr tab)) nil
              (alist-get 'my-project-id (cdr tab)) nil
              (alist-get 'explicit-name (cdr tab)) nil))
      (delete-other-windows)
      (cl-loop for tab in tabs for i from 0 do
               (unless (zerop i) (tab-bar-new-tab-to -1) (my/session--leave-side-window))
               (my/session--restore-tab tab skipped-p))
      ;; pins last, so making tabs didn't shuffle them
      (let ((all (funcall tab-bar-tabs-function)))
        (cl-loop for tab in tabs for data-tab in all do
                 (setf (alist-get 'my/pinned (cdr data-tab)) (plist-get tab :pinned)))
        (tab-bar-tabs-set all))
      (when (fboundp 'my/tab-keep-pinned-right) (my/tab-keep-pinned-right))
      (tab-bar-select-tab (1+ current))
      (force-mode-line-update t)
      (unless (member name '("last" "previous"))
        (setq my/session-current name))
      (setq my/session--active t)
      (message "Restored %s (%d tab%s)%s" name (length tabs) (if (= (length tabs) 1) "" "s")
               (if skipped
                   (format ", skipped %s" (mapconcat (lambda (d) (plist-get d :title)) skipped " and "))
                 "")))))

(defun my/session--leave-side-window ()
  "Select a normal window if a side panel (Run, containers...) has focus."
  (when (window-parameter (selected-window) 'window-side)
    (when-let ((main (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                               (window-list nil 'nomini))))
      (select-window main))))

(defun my/project-current-tab-safe ()
  (nth (tab-bar--current-tab-index) (funcall tab-bar-tabs-function)))

(defun my/session-restore-last ()
  "Restore the last session."
  (interactive)
  (my/session-load "last"))

(defun my/session-delete (name)
  "Delete saved session NAME."
  (interactive (list (my/session--read-name "Delete session: " t)))
  (when (y-or-n-p (format "Delete session %s? " name))
    (delete-file (my/session--file name))
    (when (equal name my/session-current) (setq my/session-current nil))
    (message "Deleted session %s" name)))

;;; the picker
;; ---------------------------------------------------------------------

(defun my/session--ago (time)
  (let ((s (- (float-time) time)))
    (cond ((< s 90) "just now")
          ((< s 3600) (format "%d min ago" (/ s 60)))
          ((< s 86400) (format "%dh ago" (/ s 3600)))
          ((< s (* 7 86400)) (format "%d days ago" (/ s 86400)))
          (t (format-time-string "%e %b" time)))))

(defun my/session--summary (name)
  "One line about session NAME: its tabs and when it was saved."
  (let* ((data (my/session--read name))
         (tabs (plist-get data :tabs))
         (names (delete-dups
                 (mapcar (lambda (tab)
                           (or (and (plist-get tab :project)
                                    (file-name-nondirectory
                                     (directory-file-name (plist-get tab :project))))
                               (plist-get tab :name)))
                         tabs)))
         (missing (delete-dups
                   (delq nil (mapcar (lambda (entry)
                                       (let ((drive (my/drive-for-path (car entry))))
                                         (and drive (not (my/drive-mounted-p drive)) drive)))
                                     (my/session--needed-paths data))))))
    (concat (format "%d tab%s · " (length tabs) (if (= (length tabs) 1) "" "s"))
            (string-join (seq-take names 5) ", ")
            (if (> (length names) 5) ", …" "")
            " · " (my/session--ago (or (plist-get data :saved) 0))
            (if missing
                (propertize (concat "  needs " (mapconcat (lambda (d) (plist-get d :name)) missing " + "))
                            'face 'warning)
              ""))))

(defun my/session--label (name)
  (pcase name
    ("last" "Last session")
    ("previous" "Session before that")
    (_ name)))

(defun my/session--read-name (prompt &optional named-only)
  (let* ((names (seq-remove (lambda (n) (and named-only (member n '("last" "previous"))))
                            (my/session-names)))
         ;; last first, then your named ones, then previous
         (names (append (and (member "last" names) '("last"))
                        (seq-remove (lambda (n) (member n '("last" "previous"))) names)
                        (and (member "previous" names) '("previous"))))
         (table (mapcar (lambda (name)
                          (cons (concat (my/session--label name) "  "
                                        (propertize (my/session--summary name) 'face 'shadow))
                                name))
                        names)))
    (unless table (user-error "No saved sessions yet (SPC q s saves one)"))
    (cdr (assoc (completing-read prompt
                                 (lambda (string pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (display-sort-function . identity)
                                                  (cycle-sort-function . identity))
                                     (complete-with-action action table string pred)))
                                 nil t)
                table))))

(defvar my/session--fresh "Fresh start")

(defun my/session-pick ()
  "Choose a session to open: the last one, a saved one, or a fresh start."
  (interactive)
  (let* ((names (my/session-names))
         (names (append (and (member "last" names) '("last"))
                        (seq-remove (lambda (n) (member n '("last" "previous"))) names)
                        (and (member "previous" names) '("previous"))))
         (table (append
                 (mapcar (lambda (name)
                           (cons (concat (my/session--label name) "  "
                                         (propertize (my/session--summary name) 'face 'shadow))
                                 name))
                         names)
                 (list (cons my/session--fresh nil))))
         (choice (cdr (assoc (completing-read
                              "Open session: "
                              (lambda (string pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity)
                                               (cycle-sort-function . identity))
                                  (complete-with-action action table string pred)))
                              nil t)
                             table))))
    (if choice
        (my/session-load choice)
      (setq my/session--active t)
      ;; a fresh start opens on the Welcome page
      (when (fboundp 'my/splash-show) (my/splash-show)))))

(defun my/session-pick-popup ()
  "`my/session-pick' in the vertico popup."
  (interactive)
  (if (fboundp 'my/with-vertico)
      (my/with-vertico #'my/session-pick)
    (call-interactively #'my/session-pick)))

(with-eval-after-load 'vertico-multiform
  (dolist (command '(my/session-pick my/session-pick-popup my/session-load
                     my/session-delete my/session-save))
    (add-to-list 'vertico-multiform-commands (list command 'posframe))))

;;; startup and autosave
;; ---------------------------------------------------------------------

(defun my/session--started-with-files-p ()
  (seq-some (lambda (buffer)
              (with-current-buffer buffer
                (or buffer-file-name (derived-mode-p 'dired-mode))))
            (buffer-list)))

(defun my/session--startup ()
  (if (and my/session-startup-picker
           (not noninteractive)
           (not (daemonp))
           (not (my/session--started-with-files-p))
           (my/session-names))
      (run-at-time 0.3 nil
                   (lambda ()
                     (condition-case nil
                         (my/session-pick-popup)
                       (quit (setq my/session--active t)))))
    (setq my/session--active t)))

(add-hook 'emacs-startup-hook #'my/session--startup 90)
(add-hook 'kill-emacs-hook #'my/session-autosave)
(run-at-time my/session-autosave-seconds my/session-autosave-seconds #'my/session-autosave)

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>ql") #'my/session-pick-popup
    (kbd "<leader>qs") (lambda () (interactive) (my/with-vertico #'my/session-save))
    (kbd "<leader>qr") #'my/session-restore-last
    (kbd "<leader>qd") (lambda () (interactive) (my/with-vertico #'my/session-delete))))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC q" "session")
  (which-key-add-key-based-replacements "SPC q l" "load session")
  (which-key-add-key-based-replacements "SPC q s" "save session as")
  (which-key-add-key-based-replacements "SPC q r" "restore last")
  (which-key-add-key-based-replacements "SPC q d" "delete session"))

(provide 'my-session)

;;; my-session.el ends here
