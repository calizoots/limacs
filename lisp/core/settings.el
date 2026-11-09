;;; core/settings.el

(setq inhibit-splash-screen t)
(setq ring-bell-function #'ignore)
(setq custom-file ".emacs.custom.el")
(setq-default truncate-lines t)
(setq tab-width 4)

;; allow horizontal scrolling with trackpad/scroll wheel
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 110)
(ido-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-indent-mode 1)
(pixel-scroll-precision-mode 1)
(global-display-line-numbers-mode)

(put 'dired-find-alternate-file 'disabled nil)

;; refresh open buffers when their files change on disk
(setq auto-revert-verbose nil
      auto-revert-interval 1
      global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(provide 'settings)

;;; settings.el ends here
