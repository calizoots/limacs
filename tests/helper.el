;;; tests/helper.el  -*- lexical-binding: t; -*-

;; shared setup for the config's tests (tests/run loads it first)

;; Tests run in batch Emacs with the config's own load path and packages,
;; but never touch real state: sessions, the lsp project list, container
;; and music files all point into a throwaway sandbox, and every prompt
;; is answered "no" so nothing can wait for input.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar my-test-root (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "The ~/.emacs.d being tested.")

;; packages and the config's folders
(require 'package)
(setq package-user-dir (expand-file-name "elpa" my-test-root))
(package-initialize)
(dolist (dir (append (directory-files (expand-file-name "lisp" my-test-root) t "\\`[a-z]")
                     (directory-files (expand-file-name "lisp/extpkgs" my-test-root) t "\\`[a-z]")))
  (when (file-directory-p dir) (add-to-list 'load-path dir)))

;; the sandbox
(defvar my-test-sandbox (make-temp-file "limacs-tests-" t))
(add-hook 'kill-emacs-hook (lambda () (ignore-errors (delete-directory my-test-sandbox t))))

(setq my/session-directory (expand-file-name "sessions/" my-test-sandbox)
      my/session-startup-picker nil
      lsp-session-file (expand-file-name "lsp-session" my-test-sandbox)
      recentf-save-file (expand-file-name "recentf" my-test-sandbox)
      project-list-file (expand-file-name "projects" my-test-sandbox))

;; nothing may ask anything
(advice-add 'yes-or-no-p :override (lambda (&rest _) nil))
(advice-add 'y-or-n-p :override (lambda (&rest _) nil))
(advice-add 'read-string :override (lambda (&rest _) (error "A test asked for input")))

;; some modules set evil states as they load
(require 'evil)

;;; helpers
;; ---------------------------------------------------------------------

(defmacro my-test-with-dir (var &rest body)
  "Run BODY with VAR bound to a fresh folder (removed afterwards)."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dir-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun my-test-write (dir file content)
  "Write CONTENT to DIR/FILE (making folders), return its path."
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun my-test-git (dir &rest args)
  "Run git ARGS in DIR (quietly, as a test identity); return its output."
  (let ((default-directory dir)
        (process-environment (append '("GIT_AUTHOR_NAME=test" "GIT_AUTHOR_EMAIL=test@example.invalid"
                                       "GIT_COMMITTER_NAME=test" "GIT_COMMITTER_EMAIL=test@example.invalid"
                                       "GIT_CONFIG_GLOBAL=/dev/null")
                                     process-environment)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (buffer-string))))

(defmacro my-test-with-repo (var &rest body)
  "Run BODY with VAR bound to a fresh git repository with one commit."
  (declare (indent 1))
  `(my-test-with-dir ,var
     (my-test-git ,var "init" "-q" "-b" "main")
     (my-test-write ,var "README.md" "hello\n")
     (my-test-git ,var "add" "-A")
     (my-test-git ,var "commit" "-qm" "first")
     ,@body))

(provide 'helper)

;;; helper.el ends here
