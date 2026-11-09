;;; pkgs/my-diff-hl.el

;; mark lines changed since the last git commit

(use-package diff-hl
  :ensure t
  :demand t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(provide 'my-diff-hl)

;;; my-diff-hl.el ends here
