;;; tests/test-git.el  -*- lexical-binding: t; -*-

;; the Source Control panel's reading of git

(require 'my-git)

(ert-deftest git/after-fields ()
  (should (equal (my/git--after-fields "1 .M N... 100644 100644 100644 abc def src/my file.rs" 8)
                 "src/my file.rs")))

(ert-deftest git/first-error-picks-the-useful-line ()
  (should (equal (my/git--first-error "hint: do this\nfatal: no upstream\nmore")
                 "fatal: no upstream"))
  (should (equal (my/git--first-error "hint: x\nsomething else") "something else")))

(ert-deftest git/ago ()
  (should (equal (my/git--ago (float-time)) "now"))
  (should (equal (my/git--ago (- (float-time) 7200)) "2h"))
  (should (equal (my/git--ago (- (float-time) (* 3 86400))) "3d")))

(ert-deftest git/status-sorts-staged-changes-and-untracked ()
  (my-test-with-repo dir
    (my-test-write dir "README.md" "changed\n")          ; modified, unstaged
    (my-test-write dir "new.rs" "fn x() {}\n")           ; untracked
    (my-test-write dir "staged.rs" "fn y() {}\n")
    (my-test-git dir "add" "staged.rs")                   ; added, staged
    (let ((status (my/git--status dir)))
      (should (equal (plist-get status :branch) "main"))
      (should (equal (mapcar (lambda (e) (plist-get e :path)) (plist-get status :staged))
                     '("staged.rs")))
      (should (equal (plist-get (car (plist-get status :staged)) :code) "A"))
      (let ((changes (plist-get status :changes)))
        (should (member '("README.md" "M") (mapcar (lambda (e) (list (plist-get e :path) (plist-get e :code))) changes)))
        (should (member '("new.rs" "U") (mapcar (lambda (e) (list (plist-get e :path) (plist-get e :code))) changes))))
      (should (equal (mapcar (lambda (c) (plist-get c :subject)) (plist-get status :commits)) '("first"))))))

(ert-deftest git/status-of-a-rename ()
  (my-test-with-repo dir
    (my-test-git dir "mv" "README.md" "READ ME.md")
    (let ((staged (plist-get (my/git--status dir) :staged)))
      (should (equal (plist-get (car staged) :path) "READ ME.md"))
      (should (equal (plist-get (car staged) :orig) "README.md"))
      (should (equal (plist-get (car staged) :code) "R")))))

(ert-deftest git/status-outside-a-repo ()
  (my-test-with-dir dir
    (should-not (plist-get (my/git--status dir) :branch))))

;;; test-git.el ends here
