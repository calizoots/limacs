;;; pkgs/languages.el

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
         (tsx-ts-mode . emmet-mode)
         (js-ts-mode . emmet-mode))
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
  ;; typing <div> adds </div> straight away (like VS Code), not only on </
  (setq web-mode-auto-close-style 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq sgml-basic-offset 4))

;; TAB expands div.box>p*2 style abbreviations in HTML (and indents
;; otherwise), in HTML buffers only
(with-eval-after-load 'web-mode
  (with-eval-after-load 'evil
    (evil-define-key 'insert web-mode-map
      (kbd "TAB") #'simplezen-expand-or-indent-for-tab
      (kbd "<tab>") #'simplezen-expand-or-indent-for-tab)))

(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'my/web-mode-setup))

;;; web: which mode opens what
;; ---------------------------------------------------------------------
;; (these used to be set inside web-mode's own hook, so they only took
;; effect after the first web-mode file: .html opened as mhtml-mode and
;; .tsx as nothing at all)

(use-package web-mode :mode ("\\.html?\\'" "\\.xhtml\\'" "\\.tpl\\'"))

(defun my/ts-mode-or (ts-mode fallback lang)
  "TS-MODE when the tree-sitter grammar for LANG is installed, else FALLBACK."
  (if (and (fboundp 'treesit-language-available-p) (treesit-language-available-p lang))
      ts-mode fallback))

(dolist (entry `(("\\.[mc]?jsx?\\'" . ,(my/ts-mode-or 'js-ts-mode 'js-mode 'javascript))
                 ("\\.ts\\'" . ,(my/ts-mode-or 'typescript-ts-mode 'typescript-mode 'typescript))
                 ("\\.tsx\\'" . ,(my/ts-mode-or 'tsx-ts-mode 'web-mode 'tsx))
                 ("\\.css\\'" . ,(my/ts-mode-or 'css-ts-mode 'css-mode 'css))
                 ("\\.json\\'" . ,(my/ts-mode-or 'json-ts-mode 'js-json-mode 'json))))
  (add-to-list 'auto-mode-alist entry))

(setq-default typescript-ts-mode-indent-offset 4
              json-ts-mode-indent-offset 4)

;; the language servers live in ~/.emacs.d/.cache/lsp/npm (installed with
;; npm --prefix, no sudo): typescript-language-server + TypeScript 5's
;; tsserver, and vscode-langservers-extracted (html, css, json, eslint).
;; Ahead of PATH so the snap copy (which can't see /tmp) isn't used.
(dolist (bin (file-expand-wildcards
              (expand-file-name ".cache/lsp/npm/*/bin" user-emacs-directory)))
  (add-to-list 'exec-path bin))

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
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (js-mode . lsp)
         (js-ts-mode . lsp)
         (css-mode . lsp)
         (css-ts-mode . lsp)
         (scss-mode . lsp)
         (less-css-mode . lsp)
         (json-ts-mode . lsp)
         (js-json-mode . lsp)
         (mhtml-mode . lsp)
         (html-mode . lsp)
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
        lsp-signature-function 'ignore
        ;; no breadcrumb bar above the code, ever
        lsp-headerline-breadcrumb-enable nil))

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

;;; languages.el ends here
