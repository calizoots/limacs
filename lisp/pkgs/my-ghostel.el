;;; pkgs/my-ghostel.el

;; terminal emulator built on libghostty, with evil support

(use-package ghostel
  :ensure t
  :defer t
  :config
  ;; send control keys that Emacs would otherwise keep
  (define-key ghostel-semi-char-mode-map (kbd "C-S-z")
    (lambda () (interactive) (ghostel-send-key "z" "ctrl")))

  (define-key ghostel-semi-char-mode-map (kbd "C-c :")
    (lambda () (interactive) (ghostel-send-key ":" "ctrl"))))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  ;; send C-h to the shell, not the smartparens delete from my-keys
  (evil-define-key 'insert evil-ghostel-mode-map (kbd "C-h")
    (lambda () (interactive) (ghostel-send-key "h" "ctrl"))))

(provide 'my-ghostel)

;; my-ghostel.el ends here
