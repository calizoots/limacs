;;; tests/test-code.el  -*- lexical-binding: t; -*-

;; code actions: their order, and the right-click menu around them

(require 'lsp-mode)
(require 'my-code)

(defun my-test-action (title &optional kind preferred)
  (let ((h (make-hash-table :test #'equal)))
    (puthash "title" title h)
    (when kind (puthash "kind" kind h))
    (when preferred (puthash "isPreferred" t h))
    h))

(ert-deftest code/fixes-come-first ()
  (let ((sorted (my/code--sort (list (my-test-action "Extract" "refactor.extract")
                                     (my-test-action "Organise" "source.organizeImports")
                                     (my-test-action "Import List" "quickfix")
                                     (my-test-action "Fill match arms" "quickfix" t)))))
    (should (equal (mapcar #'my/code--title sorted)
                   '("Fill match arms" "Import List" "Extract" "Organise")))))

(ert-deftest code/kinds ()
  (should (eq (my/code--kind (my-test-action "a" "quickfix")) 'quickfix))
  (should (eq (my/code--kind (my-test-action "a" "refactor.inline")) 'refactor))
  (should (eq (my/code--kind (my-test-action "a")) nil)))

(ert-deftest code/disabled-actions-are-dropped ()
  (let ((off (my-test-action "Nope" "quickfix")))
    (puthash "disabled" (let ((h (make-hash-table :test #'equal))) (puthash "reason" "no" h) h) off)
    (should (equal (mapcar #'my/code--title (my/code--clean (vector off (my-test-action "Yes"))))
                   '("Yes")))))

(ert-deftest code/menu-lists-actions-then-the-rest ()
  (with-temp-buffer
    (let* ((items (my/code--menu-items (list (my-test-action "Import List" "quickfix"))))
           (labels (mapcar (lambda (i) (if (consp i) (car i) i)) items)))
      (should (equal (car labels) "Import List"))
      (should (member "Copy" labels))
      ;; no language server: no lsp items
      (should-not (member "Rename symbol…" labels)))))

(ert-deftest code/menu-moves-extra-actions-into-more ()
  (with-temp-buffer
    (let* ((my/code-menu-max-actions 2)
           (items (my/code--menu-items (mapcar (lambda (n) (my-test-action (format "a%d" n))) '(1 2 3 4)))))
      (should (equal (car (nth 2 items)) "More actions"))
      (should (= (length (nth 2 (nth 2 items))) 2)))))

(ert-deftest code/cache-is-per-edit ()
  (with-temp-buffer
    (insert "hello")
    (my/code--cache-put (buffer-chars-modified-tick) 3 '(x))
    (should (equal (my/code--cache-get 3) '(t x)))
    (should-not (my/code--cache-get 4))
    (insert "!")
    (should-not (my/code--cache-get 3))))

;;; test-code.el ends here
