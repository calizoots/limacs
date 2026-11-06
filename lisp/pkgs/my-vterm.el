;;; pkgs/my-vterm.el

(use-package vterm
  :after evil
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-S-z") 
    (lambda () (interactive) (vterm-send-key "z" nil t)))

  (define-key vterm-mode-map (kbd "C-c :") 
    (lambda () (interactive) (vterm-send-key ":" nil t)))

  (evil-set-initial-state 'vterm-mode 'emacs)

  (defun my/vterm-insert ()
    "jump to the prompt smoothly and enter emacs state"
    (interactive)
    (goto-char (point-max))
    (vterm-reset-cursor-point)
    (evil-emacs-state)
    (recenter -1))

  (evil-define-key 'emacs vterm-mode-map (kbd "<escape>") 'evil-normal-state)
  
  (evil-define-key 'normal vterm-mode-map (kbd "i") 'my/vterm-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "a") 'my/vterm-insert))

(provide 'my-vterm)

;; my-vterm.el ends here
