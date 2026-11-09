;;; editor/line-numbers.el

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(dolist (mode '(org-mode
                term-mode
                eshell-mode
                shell-mode
                ghostel-mode
                magit-status-mode
                help-mode
                compilation-mode
                minibuffer-inactive-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode -1))))

;; global-display-line-numbers-mode turns numbers back on after mode hooks
;; run, so also skip these modes at the point where it turns them on
(defvar my/no-line-numbers-modes
  '(org-mode term-mode eshell-mode shell-mode ghostel-mode
    magit-status-mode help-mode compilation-mode
    my/run-panel-mode my/task-log-mode neotree-mode
    emms-browser-mode emms-playlist-mode my/music-mode my/music-library-mode my/music-match-mode
    mu4e-main-mode mu4e-headers-mode mu4e-view-mode)
  "Major modes (and modes derived from them) that never show line numbers.")

(defun my/skip-line-numbers-p ()
  (apply #'derived-mode-p my/no-line-numbers-modes))

(advice-add 'display-line-numbers--turn-on :before-until #'my/skip-line-numbers-p)

(provide 'line-numbers)

;;; line-numbers.el ends here
