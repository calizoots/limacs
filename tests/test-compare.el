;;; tests/test-compare.el  -*- lexical-binding: t; -*-

;; comparing runs: what's kept, what's hidden, what's different

(require 'my-compare)

(ert-deftest compare/clean-hides-cargo-chatter ()
  (let ((out (concat "warning: unused import: `x`\n --> src/main.rs:1:5\n  |\n1 | use x;\n  |     ^\n\n"
                     "warning: `b` (bin \"b\") generated 4 warnings\n"
                     "   Compiling luhorm v0.0.4\n    Finished `dev` profile in 0.4s\n     Running `target/debug/b`\n"
                     "Hello, world!\nError: boom\n  1: failed to x\n")))
    (should (equal (my/compare--clean out nil) "Hello, world!\nError: boom\n  1: failed to x"))
    ;; n shows everything again
    (should (string-match-p "Compiling" (my/compare--clean out t)))))

(ert-deftest compare/clean-drops-the-task-header ()
  (let ((out (concat (propertize "$ sh -c cargo\n  in ~/x\n\n" 'face 'my/task-meta) "hi\n✓ finished · 1.2s\n")))
    (should (equal (my/compare--clean out nil) "hi"))))

(ert-deftest compare/diff-finds-lines-on-each-side ()
  (let ((d (my/compare--diff "a\nb\nc" "a\nB\nc\nd")))
    (should (equal (car d) '(2)))
    (should (equal (sort (cdr d) #'<) '(2 4)))))

(ert-deftest compare/watch-starts-from-the-config-file ()
  (my-test-with-dir root
    (my-test-write root ".emacs-run.eld"
                   "((:name \"a\" :command \"true\" :watch t) (:name \"b\" :command \"true\") (:name \"both\" :group (\"a\" \"b\")))")
    (should (equal (my/compare--watch-names root) '("a")))
    (should (my/compare-watched-p root "a"))
    (should-not (my/compare-watched-p root "b"))
    (my/compare--set-watch root '("a" "b"))
    (should (my/compare-watched-p root "b"))))

(ert-deftest compare/groups-are-read-from-the-config-file ()
  (my-test-with-dir root
    (my-test-write root ".emacs-run.eld" "((:name \"both\" :group (\"a\" \"b\")))")
    (let ((spec (car (my/run--custom-configs root))))
      (should (equal (plist-get spec :group) '("a" "b")))
      (should (equal (plist-get spec :source) "group")))))

(ert-deftest compare/the-last-finished-run-is-kept ()
  (my-test-with-dir root
    (let* ((spec (list :name "echo" :command (list "sh" "-c" "echo one")))
           (task (my/run-start spec root)))
      (while (my/task-live-p task) (accept-process-output nil 0.1))
      (should (eq (my/task-status task) 'succeeded))
      ;; running it again keeps the first one's output
      (let ((again (my/run-start spec root)))
        (while (my/task-live-p again) (accept-process-output nil 0.1))
        (let ((kept (gethash (cons root "echo") my/compare--previous)))
          (should kept)
          (should (eq (plist-get kept :status) 'succeeded))
          (should (string-match-p "one" (plist-get kept :text))))))))

;;; test-compare.el ends here
