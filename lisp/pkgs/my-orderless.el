;;; pkgs/my-orderless.el

(use-package orderless
  :ensure t
  :pin melpa
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'my-orderless)

;; my-orderless.el ends here
