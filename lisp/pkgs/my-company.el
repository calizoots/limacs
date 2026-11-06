;;; pkgs/my-company.el

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; No delay

(provide 'my-company)

;; company.el ends here
