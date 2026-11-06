;;; core/etc.el

(defun my/after (features fn)
  (if (null features)
      (funcall fn)
    (with-eval-after-load (car features)
      (my/after (cdr features) fn))))

(defun er-sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'etc)

;; etc.el ends here
