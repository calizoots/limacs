;;; pkgs/my-neotree.el

(use-package neotree
  :ensure t
  :after evil
  :config

  (setq neo-theme 'icons
        neo-vc-integration nil
        neo-smart-open t
        neo-window-width 35
        neo-show-hidden-files nil)   ; dotfiles hidden (' in the tree shows them)

  (defun my/neotree-open ()
    "Open the NeoTree belonging to the current project."
    (interactive)

    (let ((project (my/project-current)))
      (unless project
        (user-error "Current tab has no project"))

      (let* ((root (plist-get project :root))
             (name (my/project-neotree-name project))
             (buffer (get-buffer name)))

        ;; tell NeoTree which buffer represents this project
        (setq neo-buffer-name name)

        ;; open or reuse the project's NeoTree
        (if buffer
            (progn
              (setq neo-global--buffer buffer)
              (neo-global--open))
          (neo-global--open-dir root)

          ;; NeoTree creates the buffer during the operation
          (when-let ((buffer (get-buffer name)))
            (my/project-set-neotree project buffer)))))))

  (defun my/neotree-close ()
    "Close the current project's NeoTree window."
    (interactive)

    (let ((project (my/project-current)))
      (unless project
        (user-error "Current tab has no project"))

      (when (window-live-p neo-global--window)
        (delete-window neo-global--window))

      (setq neo-global--window nil)))

  (defun my/neotree-sync-project ()
      "Synchronize NeoTree with the selected project's tree."
        (when-let* ((project (my/project-current))
		      (buffer (my/project-neotree-buffer project)))
	    
	    (setq neo-buffer-name (buffer-name buffer)
		    neo-global--buffer buffer)
	      
	      (let ((window (get-buffer-window buffer nil)))
		  (setq neo-global--window window))))
  
  (add-hook 'tab-bar-tab-post-select-functions
              (lambda (&rest _)
		  (my/neotree-sync-project)))

  (defun my/neotree-toggle ()
    "Toggle the current project's NeoTree."
    (interactive)

    (if (neo-global--window-exists-p)
        (my/neotree-close)
      (my/neotree-open)))

  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
              (display-line-numbers-mode -1)))

(provide 'my-neotree)

;;; my-neotree.el ends here
