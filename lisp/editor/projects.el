;;; editor/projects.el

(require 'cl-lib)
(require 'seq)
(require 'tab-bar)

(defvar my/project-next-id 0
  "Next ID assigned to a project.")

(defvar my/project-next-tab-id 0
  "Next ID assigned to a tab.")

(defvar my/projects nil
  "List of known project records.")

;(defun my/debug-tab-new ()
;  (interactive)
;  (tab-new)
;  (message "%S" (tab-bar-tabs)))

(defun my/project-create (directory)
  "Create and register a project for DIRECTORY."
  (setq directory
        (file-name-as-directory
         (expand-file-name directory)))

  (let ((project
         (list :id (cl-incf my/project-next-id)
               :name (file-name-nondirectory
                      (directory-file-name directory))
               :root directory
               :tabs nil
               :neotree nil)))

    (push project my/projects)
    project))

(defun my/project-by-id (id)
  "Return the project with ID."
  (seq-find
   (lambda (project)
     (= (plist-get project :id) id))
   my/projects))

(defun my/project-current-tab ()
  "Return the current tab data."
  (nth (tab-bar--current-tab-index)
       (tab-bar-tabs)))

(defun my/project-neotree-buffer (project)
  "Return PROJECT's NeoTree buffer, or nil if it doesn't exist."
  (let ((name (plist-get project :neotree)))
    (and name
         (get-buffer name))))

(defun my/project-neotree-name (project)
  "Return the buffer name for PROJECT's NeoTree."
  (or (plist-get project :neotree)
      (format "*NeoTree<%d>*"
              (plist-get project :id))))

(defun my/project-set-neotree (project buffer)
  "Associate BUFFER with PROJECT's NeoTree resource."
  (setf (plist-get project :neotree)
        (buffer-name buffer))
  buffer)

(defun my/project-current ()
  "Return the project associated with the current tab."
  (let* ((tab (my/project-current-tab))
         (id (alist-get 'my-project-id (cdr tab))))
    (and id
         (my/project-by-id id))))

(defun my/project-tab-id (tab)
  "Return TAB's ID, assigning one if necessary."
  (or (alist-get 'my-tab-id (cdr tab))
      (let ((id (cl-incf my/project-next-tab-id)))
        (setf (alist-get 'my-tab-id (cdr tab)) id)
        id)))

(defun my/project-set-current (project)
  "Associate the current tab with PROJECT."
  (let* ((tab (my/project-current-tab))
         (id (plist-get project :id))
         (tab-id (my/project-tab-id tab)))

    (unless tab
      (user-error "Could not find current tab"))

    ;; Remove this tab from its previous project.
    (when-let ((old-project (my/project-current)))
      (setf (plist-get old-project :tabs)
            (delq tab-id (plist-get old-project :tabs))))

    ;; Store our metadata in the tab's property alist.
    (setf (alist-get 'my-tab-id (cdr tab)) tab-id)
    (setf (alist-get 'my-project-id (cdr tab)) id)

    ;; Add the tab to the new project.
    (cl-pushnew tab-id
                (plist-get project :tabs))

    ;; Persist the modified tab list.
    (tab-bar-tabs-set (tab-bar-tabs))))

(defun my/project-enter (project)
  "Enter PROJECT in the current tab."
  (interactive)

  (my/project-set-current project)

  (cd (plist-get project :root)))

(defun my/project-open ()
  "Open a new project in a new tab."
  (interactive)

  (let ((directory
         (read-directory-name
          "Open project: "
          default-directory
          nil
          t)))

    (let ((project (my/project-create directory)))
      (tab-new)
      (my/project-enter project))))

(defun my/project-list ()
  "Choose a known project and enter it in the current tab."
  (interactive)

  (unless my/projects
    (user-error "No projects known yet"))

  (let* ((choices
          (mapcar
           (lambda (project)
             (cons
              (format "%s"
                      (plist-get project :root))
              project))
           my/projects))

         (choice
          (completing-read
           "Project: "
           choices
           nil
           t))

         (project
          (cdr (assoc choice choices))))

    (my/project-enter project)))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>po") #'my/project-open))

(with-eval-after-load 'my-vertico
  (global-set-key (kbd "<leader>pl")
     (lambda ()
       (interactive)
       (my/with-vertico #'my/project-list))))

(provide 'projects)

;;; projects.el ends here
