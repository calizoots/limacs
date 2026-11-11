;;; tests/test-ui.el  -*- lexical-binding: t; -*-

;; the Welcome page, scratch buffers, menus and colours

(require 'tabs)
(require 'activity-bar)   ; knows what counts as the sidebar
(require 'splash)
(require 'buffers)

(ert-deftest ui/fit-keeps-the-end-of-a-path ()
  (should (equal (my/splash--fit "short" 10) "short"))
  (should (equal (my/splash--fit "/a/very/long/path/name" 10) "…path/name")))

(ert-deftest ui/closing-the-last-code-window-shows-welcome ()
  ;; a sidebar (like the tree) and one code window
  (save-window-excursion
    (delete-other-windows)
    (let* ((code (selected-window))
           (tree (split-window code 20 'left)))
      (set-window-parameter tree 'my-activity-git t)   ; counts as the sidebar
      (switch-to-buffer (get-buffer-create "code.rs"))
      (delete-window code)
      (should (window-live-p code))                     ; it stayed
      (should (equal (buffer-name (window-buffer code)) "*Welcome*"))
      ;; deleting the Welcome page itself is refused, so :q moves on to the tab
      (should-error (delete-window code))
      (set-window-parameter tree 'my-activity-git nil)
      (kill-buffer "code.rs"))))

(ert-deftest ui/closing-one-of-two-code-windows-still-closes ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((a (selected-window))
           (b (split-window a nil 'right)))
      (delete-window b)
      (should-not (window-live-p b)))))

(ert-deftest ui/scratch-names ()
  (let ((made nil))
    (unwind-protect
        (progn
          (push (my/scratch-new-in 'text-mode) made)
          (should (equal (buffer-name (car made)) "*scratch: text*"))
          (push (my/scratch-new-in 'text-mode) made)
          (should (equal (buffer-name (car made)) "*scratch: text 2*"))
          (push (my/scratch-new-in 'emacs-lisp-mode) made)
          (should (equal (buffer-name (car made)) "*scratch: emacs-lisp*")))
      (mapc #'kill-buffer made))))

(ert-deftest ui/menu-builds-items-separators-and-submenus ()
  (let* ((map (my/menu "T" '(("One" ignore) "--" nil ("Sub" :sub (("Two" ignore))))))
         (items (cdr map)))
    ;; title, then three entries (nil skipped)
    (should (= (length (seq-filter #'consp items)) 3))))

(ert-deftest ui/colour-mix ()
  (should (equal (my/tab--mix "#ffffff" "#000000" 1.0) "#ffffff"))
  (should (equal (my/tab--mix "#ffffff" "#000000" 0.0) "#000000"))
  (should (equal (my/tab--mix "#ffffff" "#000000" 0.5) "#7f7f7f")))

;;; test-ui.el ends here
