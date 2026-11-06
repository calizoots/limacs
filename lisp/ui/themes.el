;;; ui/themes.el

(use-package doom-themes
  :ensure t
  :demand t)

;; :config
;;   (unless custom-enabled-themes
;;     (load-theme 'doom-one t))

(use-package modus-themes
  :ensure t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (setq modus-themes-to-toggle '(modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)
  (load-theme 'ef-tritanopia-dark t))

(use-package ef-themes
  :ensure t
  :demand t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t))

(add-to-list 'custom-theme-load-path "/Users/crack/.emacs.d/themes/pink-bliss-uwu")

;; (use-package pink-bliss-uwu-theme
;;   :config
;;   (load-theme 'pink-bliss-uwu t))

(use-package autothemer :ensure t)

(defcustom my/current-theme nil
  "currently active theme"
  :type 'symbol
  :group 'my-config)

(advice-add 'my/load-theme :after
            (lambda (theme)
              (customize-save-variable 'my/current-theme theme)))

;; load saved theme

(when my/current-theme
  (my/load-theme my/current-theme))

(defcustom my/themes
  '(doom-one
    ef-summer
    ef-trio-light
    ef-tritanopia-light
    ef-trio-dark
    ef-tritanopia-dark
    modus-vivendi
    modus-vivendi-tinted
    modus-vivendi-deuteranopia
    pink-bliss-uwu)
  "list of themes"
  :type '(repeat symbol)
  :group 'my-config)

(defun my/add-theme (theme)
  (interactive
   (list (intern
          (completing-read
           "add theme: "
           (mapcar #'symbol-name
                   (custom-available-themes))))))

  (unless (memq theme my/themes)
    (customize-save-variable
     'my/themes
     (append my/themes (list theme)))
    (message "added %s theme" theme)))

(defun my/load-theme (theme)
  "disable current theme(s) and load a specified new theme from my/themes list."
  (interactive
   (list (intern (completing-read "theme: " my/themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'themes)

;;; themes.el ends here
