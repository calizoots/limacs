;;; pkgs/my-padding.el

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 4
     :header-line-width 0
     :mode-line-width 3
     :tab-width 0
     :right-divider-width 1
     :scroll-bar-width 0))
  :config
  (spacious-padding-mode 1))

(set-face-attribute 'tab-bar nil :height 120)
(set-face-attribute 'tab-bar-tab nil :height 1.5)
(set-face-attribute 'tab-bar-tab-inactive nil :height 1.5)

(provide 'my-padding)

;;; my-padding.el ends here
