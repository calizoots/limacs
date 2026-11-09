;;; pkgs/my-vertico.el

(use-package vertico
  :ensure t
  :init
  (vertico-multiform-mode 1)
  :config
  (vertico-mode -1))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (setq vertico-posframe-width 80
        vertico-posframe-height 15
        vertico-posframe-border-width 2)

  (setq vertico-posframe-poshandler
        #'posframe-poshandler-frame-center)

  ;; commands that should use posframe when Vertico is active
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep posframe))
  (add-to-list 'vertico-multiform-commands
               '(consult-fd posframe))
  (add-to-list 'vertico-multiform-commands
               '(consult-line posframe))
  (add-to-list 'vertico-multiform-commands
               '(my/find-buffer-floating posframe))
  (add-to-list 'vertico-multiform-commands
               '(my/load-theme posframe))
  (add-to-list 'vertico-multiform-commands
               '(my/compilation-list posframe))
  (add-to-list 'vertico-multiform-commands
               '(my/project-list posframe))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command posframe))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command-for-buffer posframe)))

(defun my/with-vertico (fn)
  (let ((was-enabled (bound-and-true-p vertico-mode)))
    (vertico-mode 1)
    (vertico-posframe-mode 1)
    (unwind-protect
        (call-interactively fn)
      (unless was-enabled
        (vertico-mode -1)))))

(provide 'my-vertico)

;;; my-vertico.el ends here
