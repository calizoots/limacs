;;; my-task-ui.el

(require 'tabulated-list)
(require 'my-task)

(defvar my/task-manager-buffer "*Task Manager*"
  "Name of the task manager buffer.")

(defun my/task-ui--entries ()
  "Generate task manager entries."
  (mapcar
   (lambda (task)
     (let ((id (my/task-id task)))
       (list
        id
        (vector
         (number-to-string id)
         (my/task-name task)
         (symbol-name (my/task-status task))
         (if (my/task-exit-code task)
             (number-to-string (my/task-exit-code task))
           "-")
         (mapconcat #'identity
                    (my/task-command task)
                    " ")))))
   (my/task-all)))

(defun my/task-ui--selected ()
  "Return task selected at point."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No task selected"))
    (or (my/task-get id)
        (user-error "Task no longer exists"))))

(defun my/task-ui-open ()
  "Open output of selected task."
  (interactive)
  (my/task-open (my/task-ui--selected)))

(defun my/task-ui-output ()
  "Open stdout of the selected task."
  (interactive)
  (my/task-open-output (my/task-ui--selected)))

(defun my/task-ui-errors ()
  "Open stderr of the selected task."
  (interactive)
  (my/task-open-errors (my/task-ui--selected)))

(defun my/task-ui-cancel ()
  "Cancel selected task."
  (interactive)
  (my/task-cancel (my/task-ui--selected))
  (revert-buffer))

(defun my/task-ui-rerun ()
  "Rerun selected task."
  (interactive)
  (let ((task (my/task-ui--selected)))
    (my/task-start
     :name (my/task-name task)
     :command (my/task-command task)
     :directory (my/task-directory task)))
  (revert-buffer))

(define-derived-mode my/task-manager-mode
  tabulated-list-mode "Task Manager"
  "Major mode for managing task executions."

  (setq tabulated-list-format
        [("ID" 6 t)
         ("Name" 24 t)
         ("Status" 14 t)
         ("Exit" 8 t)
         ("Command" 0 t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'my/task-ui--entries)

  (tabulated-list-init-header))

(let ((map my/task-manager-mode-map))
  (define-key map (kbd "RET") #'my/task-ui-open)
  (define-key map (kbd "c") #'my/task-ui-cancel)
  (define-key map (kbd "r") #'my/task-ui-rerun)
  (define-key map (kbd "g") #'revert-buffer)
  (define-key map (kbd "q") #'quit-window))

(defun my/task-manager ()
  "Open the task manager."
  (interactive)
  (let ((buffer (get-buffer-create my/task-manager-buffer)))
    (with-current-buffer buffer
      (my/task-manager-mode)
      (tabulated-list-revert))
    (pop-to-buffer buffer)))

(with-eval-after-load 'evil
  (let ((map my/task-manager-mode-map))
    (evil-define-key 'normal my/task-manager-mode-map
      (kbd "RET") #'my/task-ui-open
      (kbd "o")   #'my/task-ui-output
      (kbd "e")   #'my/task-ui-errors
      (kbd "c")   #'my/task-ui-cancel
      (kbd "r")   #'my/task-ui-rerun
      (kbd "g")   #'revert-buffer
      (kbd "q")   #'quit-window))
  
  (evil-set-initial-state 'my/task-manager-mode 'normal))

(provide 'my-task-ui)

;;; my-task-ui.el ends here
