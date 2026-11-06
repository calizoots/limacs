;;; editor/line-numbers.el

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(dolist (mode '(org-mode
                term-mode
                eshell-mode
                shell-mode
                vterm-mode
                magit-status-mode
                help-mode
                compilation-mode
                minibuffer-inactive-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode -1))))

(provide 'line-numbers)

;; line-numbers.el ends here
