;;; editor/projects.el --- tabs as projects  -*- lexical-binding: t; -*-

;; A project is a root directory.  Tabs are linked to a project; every tab
;; of a project shares that project's NeoTree, and file search
;; (consult-fd / consult-ripgrep) is scoped to the current tab's project.
;;
;; Known projects persist across restarts through project.el's list
;; (~/.emacs.d/projects).
;;
;; Keys (normal state):
;;   SPC p o  open/switch to a project (known list, or browse for a folder)
;;   SPC p t  new tab in the current project
;;   SPC p a  attach the current tab to a project
;;   SPC p b  switch to a buffer of the current project
;;   SPC p f  find file in project      SPC p g  grep in project
;;   SPC p d  dired at project root     SPC p v  terminal at project root
;;   SPC p k  close project (its tabs, buffers and tree)
;;   SPC p l  list projects (same as SPC p o)

(require 'cl-lib)
(require 'seq)
(require 'tab-bar)
(require 'project)

(defvar my/project-next-id 0
  "Next ID assigned to a project.")

(defvar my/projects nil
  "Projects opened in this session.")

;; ---------------------------------------------------------------------
;; Records
;; ---------------------------------------------------------------------

(defun my/project--normalize (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun my/project-by-id (id)
  "Return the project with ID."
  (seq-find (lambda (p) (eql (plist-get p :id) id)) my/projects))

(defun my/project-by-root (root)
  "Return the open project whose root is ROOT."
  (let ((root (my/project--normalize root)))
    (seq-find (lambda (p) (string= (plist-get p :root) root)) my/projects)))

(defun my/project-create (directory)
  "Return the project for DIRECTORY, registering it if it is new."
  (let ((root (my/project--normalize directory)))
    (or (my/project-by-root root)
        (let ((project (list :id (cl-incf my/project-next-id)
                             :name (file-name-nondirectory (directory-file-name root))
                             :root root
                             :neotree nil)))
          (push project my/projects)
          project))))

;; ---------------------------------------------------------------------
;; Persistence (shared with project.el)
;; ---------------------------------------------------------------------

(defun my/project--remember (root)
  "Put ROOT at the front of project.el's saved project list."
  (ignore-errors
    (project--ensure-read-project-list)
    (let ((root (my/project--normalize root)))
      (setq project--list
            (cons (list root)
                  (seq-remove (lambda (e)
                                (string= (my/project--normalize (car e)) root))
                              project--list)))
      (project--write-project-list))))

(defun my/project-known-roots ()
  "Saved project roots, most recently used first, that still exist."
  (seq-filter #'file-directory-p
              (delete-dups
               (mapcar #'my/project--normalize
                       (append (mapcar (lambda (p) (plist-get p :root)) my/projects)
                               (ignore-errors (project-known-project-roots)))))))

;; ---------------------------------------------------------------------
;; Tabs
;; ---------------------------------------------------------------------

(defun my/project-current-tab ()
  "Return the current tab data."
  (nth (tab-bar--current-tab-index) (tab-bar-tabs)))

(defun my/project-current ()
  "Return the project linked to the current tab."
  (let ((id (alist-get 'my-project-id (cdr (my/project-current-tab)))))
    (and id (my/project-by-id id))))

(defun my/project-root ()
  "Root directory of the current tab's project, or nil."
  (plist-get (my/project-current) :root))

(defun my/project-set-current (project)
  "Link the current tab to PROJECT."
  (let ((tab (my/project-current-tab)))
    (unless tab (user-error "Could not find current tab"))
    (setf (alist-get 'my-project-id (cdr tab)) (plist-get project :id))
    (tab-bar-tabs-set (tab-bar-tabs))
    (force-mode-line-update t)))

(defun my/project-tabs (project)
  "1-based indices of the tabs linked to PROJECT."
  (let ((i 0) result)
    (dolist (tab (tab-bar-tabs))
      (cl-incf i)
      (when (eql (alist-get 'my-project-id (cdr tab)) (plist-get project :id))
        (push i result)))
    (nreverse result)))

(defun my/project--tab-reusable-p ()
  "Non-nil if the current tab is an empty scratch tab we can take over."
  (and (not (my/project-current))
       (one-window-p t)
       (member (buffer-name (window-buffer))
               '("*scratch*" "*GNU Emacs*" "*Messages*" "*dashboard*"))))

;; ---------------------------------------------------------------------
;; Buffers
;; ---------------------------------------------------------------------

(defun my/project-buffer-p (buffer project)
  "Non-nil if BUFFER belongs to PROJECT."
  (let ((root (plist-get project :root))
        (file (or (buffer-file-name buffer)
                  (buffer-local-value 'default-directory buffer))))
    (and file
         (not (string-prefix-p " " (buffer-name buffer)))
         (string-prefix-p root (expand-file-name file)))))

(defun my/project-buffers (project)
  (seq-filter (lambda (b) (my/project-buffer-p b project)) (buffer-list)))

(defun my/project--recent-file-buffer (project)
  (seq-find #'buffer-file-name (my/project-buffers project)))

(defun my/project--show-default (project)
  "Fill the current tab with something useful for PROJECT."
  (let* ((root (plist-get project :root))
         (readme (car (directory-files root t "\\`README" t))))
    (delete-other-windows)
    (cond ((my/project--recent-file-buffer project)
           (switch-to-buffer (my/project--recent-file-buffer project)))
          (readme (find-file readme))
          (t (dired root)))
    (when (fboundp 'my/neotree-open)
      (save-selected-window
        (ignore-errors (my/neotree-open))))))

;; ---------------------------------------------------------------------
;; Picking a project
;; ---------------------------------------------------------------------

(defconst my/project--browse "  Browse for a folder…")

(defun my/project--read (prompt)
  "Ask for a project directory with PROMPT; return its root."
  (let* ((roots (my/project-known-roots))
         (table (mapcar (lambda (root)
                          (cons (format "%-24s %s"
                                        (file-name-nondirectory (directory-file-name root))
                                        (propertize (abbreviate-file-name root)
                                                    'face 'shadow))
                                root))
                        roots))
         (choice (completing-read prompt
                                  (append (mapcar #'car table)
                                          (list my/project--browse))
                                  nil t)))
    (or (cdr (assoc choice table))
        (read-directory-name "Project folder: " default-directory nil t))))

;; ---------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------

(defun my/project-switch (directory)
  "Go to the project at DIRECTORY: its existing tab, or a new one."
  (let* ((project (my/project-create directory))
         (tabs (my/project-tabs project)))
    (my/project--remember (plist-get project :root))
    (if tabs
        (tab-bar-select-tab (car tabs))
      (unless (my/project--tab-reusable-p)
        (tab-new))
      (my/project-set-current project)
      (my/project--show-default project))
    project))

(defun my/project-open ()
  "Open a project, or switch to its tab if it is already open."
  (interactive)
  (my/project-switch (my/project--read "Open project: ")))

(defun my/project-list ()
  "Same as `my/project-open'."
  (interactive)
  (my/project-open))

(defun my/project-enter (project)
  "Link the current tab to PROJECT and move to its root."
  (my/project-set-current project)
  (my/project--remember (plist-get project :root))
  (cd (plist-get project :root)))

(defun my/project-attach ()
  "Link the current tab to a project, keeping its windows."
  (interactive)
  (my/project-enter (my/project-create (my/project--read "Attach tab to project: "))))

(defun my/project-new-tab ()
  "Open another tab in the current project."
  (interactive)
  (let ((project (or (my/project-current)
                     (user-error "This tab has no project (SPC p o to open one)"))))
    (tab-new)
    (my/project-set-current project)
    (my/project--show-default project)))

(defun my/project--require ()
  (or (my/project-current)
      (user-error "This tab has no project (SPC p o to open one)")))

(defun my/project-find-buffer ()
  "Switch to a buffer belonging to the current project."
  (interactive)
  (let* ((project (my/project--require))
         (names (mapcar #'buffer-name
                        (seq-remove (lambda (b) (eq b (current-buffer)))
                                    (my/project-buffers project)))))
    (switch-to-buffer
     (completing-read (format "[%s] buffer: " (plist-get project :name)) names nil t))))

(defun my/project-find-file ()
  "Find a file in the current project."
  (interactive)
  (let ((default-directory (plist-get (my/project--require) :root)))
    (if (fboundp 'consult-fd)
        (consult-fd default-directory)
      (project-find-file))))

(defun my/project-grep ()
  "Search text in the current project."
  (interactive)
  (let ((default-directory (plist-get (my/project--require) :root)))
    (if (fboundp 'consult-ripgrep)
        (consult-ripgrep default-directory)
      (call-interactively #'rgrep))))

(defun my/project-dired ()
  "Open dired at the project root."
  (interactive)
  (dired (plist-get (my/project--require) :root)))

(defun my/project-terminal ()
  "Open a terminal at the project root."
  (interactive)
  (let ((default-directory (plist-get (my/project--require) :root)))
    (call-interactively (if (fboundp 'ghostel) #'ghostel #'shell))))

(defun my/project-close ()
  "Close the current project: its tabs, its buffers and its NeoTree."
  (interactive)
  (let* ((project (my/project--require))
         (name (plist-get project :name)))
    (when (yes-or-no-p (format "Close project %s? " name))
      ;; save-some-buffers asks about unsaved files of this project only
      (save-some-buffers nil (lambda () (my/project-buffer-p (current-buffer) project)))
      (dolist (buf (my/project-buffers project))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (when-let ((tree (and (plist-get project :neotree)
                            (get-buffer (plist-get project :neotree)))))
        (kill-buffer tree))
      ;; close from the right so indices stay valid
      (dolist (i (reverse (my/project-tabs project)))
        (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab i)
          (my/project--unlink-current-tab)
          (delete-other-windows)
          (switch-to-buffer "*scratch*")))
      (setq my/projects (delq project my/projects))
      (message "Closed project %s" name))))

(defun my/project--unlink-current-tab ()
  (let ((tab (my/project-current-tab)))
    (setf (alist-get 'my-project-id (cdr tab) nil t) nil)
    (tab-bar-tabs-set (tab-bar-tabs))))

;; ---------------------------------------------------------------------
;; NeoTree bookkeeping (used by pkgs/my-neotree.el)
;; ---------------------------------------------------------------------

(defun my/project-neotree-buffer (project)
  "Return PROJECT's NeoTree buffer, or nil if it doesn't exist."
  (let ((name (plist-get project :neotree)))
    (and name (get-buffer name))))

(defun my/project-neotree-name (project)
  "Return the buffer name for PROJECT's NeoTree."
  (or (plist-get project :neotree)
      (format "*NeoTree<%s>*" (plist-get project :name))))

(defun my/project-set-neotree (project buffer)
  "Associate BUFFER with PROJECT's NeoTree."
  (setf (plist-get project :neotree) (buffer-name buffer))
  buffer)

;; ---------------------------------------------------------------------
;; Keys
;; ---------------------------------------------------------------------

(defun my/project--vertico (fn)
  "Run FN with the floating vertico picker if it is available."
  (lambda ()
    (interactive)
    (if (fboundp 'my/with-vertico)
        (my/with-vertico fn)
      (call-interactively fn))))

(with-eval-after-load 'vertico-multiform
  (dolist (cmd '(my/project-open my/project-list my/project-attach my/project-find-buffer))
    (add-to-list 'vertico-multiform-commands (list cmd 'posframe))))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>po") (my/project--vertico #'my/project-open)
    (kbd "<leader>pl") (my/project--vertico #'my/project-list)
    (kbd "<leader>pa") (my/project--vertico #'my/project-attach)
    (kbd "<leader>pb") (my/project--vertico #'my/project-find-buffer)
    (kbd "<leader>pf") (my/project--vertico #'my/project-find-file)
    (kbd "<leader>pg") (my/project--vertico #'my/project-grep)
    (kbd "<leader>pt") #'my/project-new-tab
    (kbd "<leader>pd") #'my/project-dired
    (kbd "<leader>pv") #'my/project-terminal
    (kbd "<leader>pk") #'my/project-close))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC p" "project"))

(provide 'projects)

;;; projects.el ends here
