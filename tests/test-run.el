;;; tests/test-run.el  -*- lexical-binding: t; -*-

;; run configurations found in a project

(require 'my-run)

(ert-deftest run/cargo-targets-examples-and-bins ()
  (my-test-with-dir root
    (my-test-write root "Cargo.toml" "[package]\nname = \"x\"\n")
    (my-test-write root "examples/demo.rs" "fn main() {}")
    (my-test-write root "examples/big/main.rs" "fn main() {}")
    (my-test-write root "src/bin/tool.rs" "fn main() {}")
    (let ((names (mapcar (lambda (s) (plist-get s :name)) (my/run--detect-cargo root))))
      (dolist (n '("run" "build" "test" "example demo" "example big" "bin tool"))
        (should (member n names))))))

(ert-deftest run/no-cargo-no-configs ()
  (my-test-with-dir root
    (should-not (my/run--detect-cargo root))))

(ert-deftest run/custom-configs ()
  (my-test-with-dir root
    (my-test-write root ".emacs-run.eld" "((:name \"dev\" :command \"npm run dev\" :kind server) (:name \"x\" :command (\"ls\" \"-l\")))")
    (let ((specs (my/run--custom-configs root)))
      (should (equal (plist-get (car specs) :command) '("sh" "-c" "npm run dev")))
      (should (eq (plist-get (car specs) :kind) 'server))
      (should (equal (plist-get (cadr specs) :command) '("ls" "-l"))))))

(ert-deftest run/broken-config-file-is-survivable ()
  (my-test-with-dir root
    (my-test-write root ".emacs-run.eld" "((:name \"dev\"")
    (should-not (my/run--custom-configs root))))

(ert-deftest run/command-string ()
  (should (equal (my/run--command-string '("sh" "-c" "cargo run")) "cargo run"))
  (should (equal (my/run--command-string '("cargo" "test")) "cargo test")))

;;; test-run.el ends here
