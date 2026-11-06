;;; pkgs/my-consult.el

(use-package consult
  :ensure t
  :config
  (setq consult-project-function
        (lambda (_)
          (project-root (project-current)))))

(provide 'my-consult)

;;; my-consult.el ends here
