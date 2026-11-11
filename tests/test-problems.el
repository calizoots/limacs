;;; tests/test-problems.el  -*- lexical-binding: t; -*-

;; the Problems panel: gathering, ordering, filtering

(require 'lsp-mode)
(require 'tabs)
(require 'problems)

(defun my-test-diag (line col severity message &optional source)
  "A diagnostic as lsp-mode keeps it (a hash table)."
  (let ((pos (lambda (l c) (let ((h (make-hash-table :test #'equal)))
                             (puthash "line" l h) (puthash "character" c h) h)))
        (range (make-hash-table :test #'equal))
        (d (make-hash-table :test #'equal)))
    (puthash "start" (funcall pos line col) range)
    (puthash "end" (funcall pos line (1+ col)) range)
    (puthash "range" range d)
    (puthash "severity" severity d)
    (puthash "message" message d)
    (puthash "source" (or source "rust-analyzer") d)
    d))

(defmacro my-test-with-diagnostics (root alist &rest body)
  "BODY with lsp reporting ALIST ((FILE . DIAGS)...) for files under ROOT."
  (declare (indent 2))
  `(let ((table (make-hash-table :test #'equal)))
     (dolist (entry ,alist) (puthash (expand-file-name (car entry) ,root) (cdr entry) table))
     (cl-letf (((symbol-function 'lsp-diagnostics) (lambda (&rest _) table)))
       ,@body)))

(ert-deftest problems/grouped-errors-first-hints-left-out ()
  (my-test-with-dir root
    (my-test-with-diagnostics root
        (list (cons "src/a.rs" (list (my-test-diag 9 0 2 "unused x") (my-test-diag 2 4 1 "mismatched types")))
              (cons "src/b.rs" (list (my-test-diag 0 0 2 "dead code") (my-test-diag 3 0 4 "a hint")))
              (cons "/elsewhere/c.rs" (list (my-test-diag 0 0 1 "not ours"))))
      (let ((groups (my/problems-collect (file-truename root))))
        ;; only this project's files, the one with an error first
        (should (equal (mapcar (lambda (g) (file-name-nondirectory (car g))) groups) '("a.rs" "b.rs")))
        ;; inside a file: errors, then by line; lines and columns from 1
        (should (equal (mapcar (lambda (p) (list (plist-get p :severity) (plist-get p :line) (plist-get p :col)))
                               (cdar groups))
                       '((error 3 5) (warning 10 1))))
        ;; hints aren't problems
        (should (= (length (cdadr groups)) 1))))))

(ert-deftest problems/counts-filters-and-folding ()
  (my-test-with-dir root
    (my-test-with-diagnostics root
        (list (cons "src/a.rs" (list (my-test-diag 0 0 1 "mismatched types") (my-test-diag 1 0 2 "unused x")
                                     (my-test-diag 2 0 3 "note this"))))
      (with-temp-buffer
        (my/problems-mode)
        (setq my/problems--root (file-truename root))
        (my/problems--render t)
        (should (string-match-p "mismatched types" (buffer-string)))
        (should (string-match-p "a\\.rs" (buffer-string)))
        ;; warnings off
        (my/problems-toggle 'warning)
        (should-not (string-match-p "unused x" (buffer-string)))
        (should (string-match-p "mismatched" (buffer-string)))
        ;; text filter
        (my/problems-toggle 'warning)
        (my/problems-filter "UNUSED")
        (should (string-match-p "unused x" (buffer-string)))
        (should-not (string-match-p "mismatched" (buffer-string)))
        ;; folding a file hides its problems
        (my/problems-filter "")
        (goto-char (point-min))
        (search-forward "a.rs")
        (my/problems-fold)
        (should-not (string-match-p "mismatched" (buffer-string)))))))

(ert-deftest problems/nothing-to-report ()
  (my-test-with-dir root
    (my-test-with-diagnostics root nil
      (with-temp-buffer
        (my/problems-mode)
        (setq my/problems--root (file-truename root))
        (my/problems--render t)
        (should (string-match-p "No problems" (buffer-string)))))))

;;; test-problems.el ends here
