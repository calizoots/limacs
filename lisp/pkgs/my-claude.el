;;; pkgs/my-claude.el

;; run Claude Code inside Emacs and review its edits in ediff

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'ghostel)
  :config
  (claude-code-ide-emacs-tools-setup))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>ac") #'claude-code-ide-menu))

(provide 'my-claude)

;;; my-claude.el ends here
