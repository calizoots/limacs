;;; tests/test-java.el  -*- lexical-binding: t; -*-

;; where jdtls sees Java code from

(require 'my-java)

(ert-deftest java/jadx-sources-root-from-the-package ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "out/sources/com/example/app/Main.java"
                               "package com.example.app;\n\nclass Main {}\n")))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (should (equal (my/java-root file) (expand-file-name "out/sources/" dir)))
        (set-buffer-modified-p nil)))))

(ert-deftest java/a-loose-file-is-its-own-root ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "Hello.java" "class Hello {}\n")))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (should (equal (my/java-root file) dir))
        (set-buffer-modified-p nil)))))

(ert-deftest java/a-package-that-does-not-match-its-folders ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "misc/Odd.java" "package com.other;\nclass Odd {}\n")))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (should (equal (my/java-root file) (expand-file-name "misc/" dir)))
        (set-buffer-modified-p nil)))))

(ert-deftest java/a-maven-project-wins ()
  (my-test-with-dir dir
    (my-test-write dir "pom.xml" "<project/>")
    (let ((file (my-test-write dir "src/main/java/com/x/A.java" "package com.x;\nclass A {}\n")))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (should (equal (file-name-as-directory (my/java-root file)) dir))
        (set-buffer-modified-p nil)))))

(ert-deftest java/newest-android-version-first ()
  (should (equal (sort (list "android-34/android.jar" "android-37.0/android.jar" "android-9/android.jar")
                       (lambda (a b) (version< (my/java--api b) (my/java--api a))))
                 '("android-37.0/android.jar" "android-34/android.jar" "android-9/android.jar"))))

;;; test-java.el ends here
