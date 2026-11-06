;;; pkgs/my-flycheck.el

(use-package flycheck
  :ensure t)

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  ;; show errors automatically for the current line
  (setq flycheck-posframe-position 'point-bottom-left-corner
        flycheck-posframe-border-width 1
        flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-error-prefix "❌ ")
  (add-hook 'flycheck-posframe-after-refresh-hook #'flycheck-posframe-show-posframe))

(provide 'my-flycheck)

;; my-flycheck.el ends here
