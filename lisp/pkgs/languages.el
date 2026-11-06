;;; editor/languages.el

(use-package emmet-mode
  :hook (web-mode . emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t))

(use-package markdown-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package typescript-mode
  :config
  (add-hook 'tsx-ts-mode-hook (lambda () (setq typescript-indent-level 4)))
  (add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 4)))
  :ensure t)

(use-package zig-mode
  :ensure t)

(defun my/web-mode-setup ()
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq sgml-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.htm\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xhtml\\'" . web-mode))
  (my/after '(evil-mode simplezen)
    (define-key evil-insert-state-map (kbd "TAB") 'simplezen-expand))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'"   . web-mode)))

(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'my/web-mode-setup))

;; auto tagging

(use-package simplezen
  :ensure t)

;; lsp-mode

(use-package lsp-mode
  :ensure t
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (web-mode . lsp)
         (typescript-mode . lsp)
         (zig-mode . lsp)
         (rust-mode . lsp)
		 (objc-mode . lsp)
         (lsp-mode . flycheck-mode)
		 (lsp-mode . flycheck-posframe-mode)
         )
  :config
  (setq lsp-eldoc-enable-hover nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
		sp-indent-quiet t
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate nil
        lsp-signature-function 'ignore))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; smali-mode

(with-eval-after-load 'smali-mode
  (autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali" t)
  (add-to-list 'auto-mode-alist '(".smali$" . smali-mode)))

;; tab offsets

(setq-default c-basic-offset 4
	      evil-shift-width 4
	      js-indent-level 4
	      ts-indent-level 4
	      json-indent-level 4
	      css-indent-offset 4
	      rust-indent-offset 4)

(provide 'languages)

;; languages.el ends here
