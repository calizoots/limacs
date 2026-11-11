;;; tests/test-tree.el  -*- lexical-binding: t; -*-

;; the file tree's file operations

(require 'my-tree)

(ert-deftest tree/move-into-a-folder-and-open-buffers-follow ()
  (my-test-with-dir dir
    (let* ((file (my-test-write dir "a.rs" "fn a() {}"))
           (dest (file-name-as-directory (expand-file-name "src" dir)))
           (buffer (find-file-noselect file)))
      (make-directory dest)
      (unwind-protect
          (let ((moved (my/tree-move file dest)))
            (should (file-exists-p moved))
            (should-not (file-exists-p file))
            (should (equal (buffer-file-name buffer) moved)))
        (kill-buffer buffer)))))

(ert-deftest tree/moving-a-folder-moves-buffers-inside-it ()
  (my-test-with-dir dir
    (let* ((file (my-test-write dir "lib/deep/x.rs" "x"))
           (buffer (find-file-noselect file)))
      (make-directory (expand-file-name "dest" dir))
      (unwind-protect
          (progn
            (my/tree-move (expand-file-name "lib" dir) (expand-file-name "dest" dir))
            (should (equal (buffer-file-name buffer) (expand-file-name "dest/lib/deep/x.rs" dir))))
        (kill-buffer buffer)))))

(ert-deftest tree/refuses-bad-moves ()
  (my-test-with-dir dir
    (let ((folder (expand-file-name "f" dir)))
      (make-directory (expand-file-name "f/inner" dir) t)
      (my-test-write dir "taken/a.rs" "1")
      (my-test-write dir "a.rs" "2")
      ;; into itself
      (should-error (my/tree-move folder (expand-file-name "f/inner" dir)) :type 'user-error)
      ;; onto a name that's taken
      (should-error (my/tree-move (expand-file-name "a.rs" dir) (expand-file-name "taken" dir)) :type 'user-error)
      ;; where it already is
      (should-error (my/tree-move (expand-file-name "a.rs" dir) dir) :type 'user-error))))

(ert-deftest tree/copies-get-free-names ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "main.rs" "x")))
      (should (equal (my/tree-copy-into file dir) (expand-file-name "main copy.rs" dir)))
      (should (equal (my/tree-copy-into file dir) (expand-file-name "main copy 2.rs" dir)))
      (my-test-write dir ".env" "A=1")
      (should (equal (my/tree-copy-into (expand-file-name ".env" dir) dir) (expand-file-name ".env copy" dir))))))

(ert-deftest tree/new-file-makes-its-folders ()
  (my-test-with-dir dir
    (let ((path (my/tree-new-file dir "src/bin/tool.rs")))
      (should (file-exists-p path))
      (should-error (my/tree-new-file dir "src/bin/tool.rs") :type 'user-error))))

(ert-deftest tree/rename-keeps-the-folder ()
  (my-test-with-dir dir
    (let* ((file (my-test-write dir "src/old.rs" "x"))
           (new (my/tree-rename file "new.rs")))
      (should (equal new (expand-file-name "src/new.rs" dir)))
      (should (file-exists-p new)))))

(ert-deftest tree/delete-goes-to-the-bin-and-closes-buffers ()
  (my-test-with-dir dir
    (let* ((trash (make-temp-file "trash" t))
           (trash-directory trash)
           (file (my-test-write dir "gone.rs" "x"))
           (buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (my/tree-delete file)
            (should-not (file-exists-p file))
            (should-not (buffer-live-p buffer))
            ;; it's in the bin, not gone for good
            (should (directory-files trash nil "gone")))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (delete-directory trash t)))))

(ert-deftest tree/dir-of ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "src/a.rs" "x")))
      (should (equal (my/tree--dir-of file) (expand-file-name "src/" dir)))
      (should (equal (my/tree--dir-of (expand-file-name "src" dir)) (expand-file-name "src/" dir))))))

;;; test-tree.el ends here
