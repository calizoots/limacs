;;; pkgs/my-consult.el

(use-package consult
  :ensure t
  :config
  ;; search the current tab's project; otherwise fall back to project.el
  (setq consult-project-function
        (lambda (may-prompt)
          (or (and (fboundp 'my/project-root) (my/project-root))
              (consult--default-project-function may-prompt)))))

(provide 'my-consult)

;;; my-consult.el ends here
