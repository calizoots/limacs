;;; core/my-keys.el

;; glitched file

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>fb")
    (lambda ()
      (interactive)
      (my/with-vertico #'my/find-buffer-floating)))

  (evil-define-key 'normal 'global
    (kbd "<leader>lt")
    (lambda ()
      (interactive)
      (my/with-vertico #'my/load-theme)))

  ;; escape keys in shell
  (define-key evil-normal-state-map (kbd "C-z") nil)
  (define-key evil-insert-state-map (kbd "C-z") nil)

  ;; move cursor across windows

  (evil-define-key 'normal 'global
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right)

  ;; split windows

  (evil-define-key 'normal 'global (kbd "<leader>sv") 
    (lambda () (interactive) (split-window-right) (other-window 1)))
  
  (evil-define-key 'normal 'global (kbd "<leader>sh") 
    (lambda () (interactive) (split-window-below) (other-window 1)))

  ;; list buffers

  (evil-define-key 'normal 'global (kbd "<leader>bl")
    (lambda () (interactive) (list-buffers)))
  
  ;; neotree bindings

  (with-eval-after-load 'neotree
    ;; toggle
    (evil-define-key 'normal 'global
      (kbd "<leader>ee") #'my/neotree-toggle)

    (evil-define-key 'normal neotree-mode-map
      (kbd "l") 'neotree-enter
      (kbd "a") 'neotree-change-root
      (kbd "r") 'neotree-refresh
      (kbd "'") 'neotree-hidden-file-toggle
      (kbd "h") 'neotree-select-up-node))

  ;; consult

  (with-eval-after-load 'consult
    (evil-define-key 'normal 'global
      (kbd "<leader>fg")
      (lambda ()
	(interactive)
	(my/with-vertico #'consult-ripgrep)))

    (evil-define-key 'normal 'global
      (kbd "<leader>fs")
      (lambda ()
	(interactive)
	(my/with-vertico #'consult-line)))

    (evil-define-key 'normal 'global
      (kbd "<leader>ff")
      (lambda ()
	(interactive)
	(my/with-vertico #'consult-fd))))

  (with-eval-after-load 'lsp-mode
    (evil-define-key 'normal 'global
      (kbd "K") #'lsp-describe-thing-at-point)

    (evil-define-key 'normal 'global
      (kbd "<leader> r n") #'lsp-rename)

    (evil-define-key 'normal 'global
      (kbd "<leader> g d") #'lsp-find-declaration)

    (evil-define-key 'normal 'global
      (kbd "<leader> g i") #'lsp-find-implementation))

  ;; flycheck

  (with-eval-after-load 'flycheck
    (evil-define-key 'normal 'global (kbd "<leader>xw") #'my/problems-toggle-panel))

  ;; nerd commenter

  (with-eval-after-load 'evil-nerd-commenter
    (evil-define-key 'normal 'global (kbd "g c") #'evilnc-comment-or-uncomment-lines)
    (evil-define-key 'visual 'global (kbd "g c") #'evilnc-comment-or-uncomment-lines))

  ;; terminal

  (evil-define-key 'normal 'global (kbd "<leader>vt") #'ghostel)
  (global-set-key (kbd "M-]") #'ghostel)

  ;; useless smartparens shi

(defvar my/compilation-buffers nil
  "List of compilation buffers managed by my configuration.")

(defun my/compilation-track ()
  "Track the current compilation buffer."
  (when (derived-mode-p 'compilation-mode)
    (add-to-list 'my/compilation-buffers (current-buffer))))

(add-hook 'compilation-mode-hook #'my/compilation-track)

(defun my/compilation-cleanup ()
  "Remove dead compilation buffers from the tracking list."
  (setq my/compilation-buffers
        (seq-filter #'buffer-live-p my/compilation-buffers)))

(defun my/compilation-toggle ()
  "Toggle the most recently used compilation buffer."
  (interactive)
  (my/compilation-cleanup)
  (if-let ((buffer (car my/compilation-buffers)))
      (if-let ((window (get-buffer-window buffer)))
          (delete-window window)
        (pop-to-buffer buffer))
    (message "No compilation buffers")))

(defun my/compilation-list ()
  "Select a compilation buffer."
  (interactive)
  (setq my/compilation-buffers
        (seq-filter #'buffer-live-p my/compilation-buffers))
  (if my/compilation-buffers
      (let ((buffer
             (completing-read
              "Compilation: "
              (mapcar #'buffer-name my/compilation-buffers)
              nil t)))
        (pop-to-buffer buffer))
    (message "No compilation buffers")))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>cc") #'compile
    (kbd "<leader>ct") #'my/compilation-toggle
    (kbd "<leader>co") #'my/compilation-list))

  (with-eval-after-load 'my-vertico
    (evil-define-key 'normal 'global
      (kbd "<leader>co")
      (lambda ()
        (interactive)
        (my/with-vertico #'my/compilation-list))))

  (with-eval-after-load 'smartparens
            (define-key evil-insert-state-map (kbd "C-h") 'sp-backward-delete-char)
            (define-key evil-insert-state-map (kbd "C-d") 'sp-delete-char)))

;; other bindings

(global-set-key (kbd "C-=") #'rename-buffer)

(with-eval-after-load 'my-vertico
  (global-set-key (kbd "M-X")
     (lambda ()
       (interactive)
       (my/with-vertico #'execute-extended-command-for-buffer)))
  (global-set-key (kbd "s-p")
                  (lambda ()
                    (interactive)
                    (my/with-vertico #'execute-extended-command))))

(provide 'my-keys)

;;; my-keys.el ends here
