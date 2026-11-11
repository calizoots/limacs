;;; init.el

;;; Author: lim3rence - s.c
;;; a real ass nigga was here

;; Bootstrap to user-emacs-directory/lisp

(let ((local-lisp (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path local-lisp)
  (dolist (dir (directory-files-recursively local-lisp "\\`[^.]" t))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;;(require 'presence)

;; keybinds
(require 'my-keys)

;; core modules

(require 'etc)
(require 'languages)
(require 'settings)
(require 'platform)
(require 'packages)
(require 'buffers)
(require 'themes)
(require 'tabs)
(require 'modeline)
(require 'activity-bar)
(require 'splash)
(require 'projects)

;; runtime
(require 'my-task)
(require 'my-task-ui)
(require 'my-run)
(require 'my-services)
(require 'my-drives)
(require 'my-session)
(require 'my-containers)
(require 'my-git)
(require 'my-compare)

;; treesitter

(require 'my-treesit)
(require 'my-code)

;; my packages

(require 'my-vertico)
(require 'my-evil)
(require 'my-ghostel)
(require 'my-company)
(require 'my-flycheck)
(require 'my-orderless)
(require 'my-which-key)
(require 'my-neotree)
(require 'my-smartparens)
(require 'my-all-the-icons)
(require 'my-consult)
(require 'my-marginalia)
(require 'my-ligatures)
(require 'my-diff-hl)
(require 'my-claude)
(require 'my-music)
(require 'my-mail)
(require 'my-search)
(require 'my-java)

(require 'line-numbers)
;; last: it reads every module's defaults, then puts back what you changed
(require 'settings-page)

;;; init.el ends here
