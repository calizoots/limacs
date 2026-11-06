;;; core/platform.el

;; mac (first because is main now)

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'none)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-alh --group-directories-first"))

(provide 'platform)

;;; platform.el ends here
