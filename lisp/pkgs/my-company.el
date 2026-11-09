;;; pkgs/my-company.el

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0 ;; no delay
        ;; don't pop up in the minibuffer (breaks evil :w / :q)
        company-global-minibuffer nil))

(provide 'my-company)

;;; my-company.el ends here
