;;; pkgs/my-evil.el

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  ;; set leader to space
  (evil-set-leader 'normal (kbd "SPC")))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(provide 'my-evil)

;; my-evil.el ends here
