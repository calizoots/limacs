;;; tests/test-projects.el  -*- lexical-binding: t; -*-

;; projects, drives, sessions, containers and search entries

(require 'projects)
(require 'my-drives)
(require 'my-session)
(require 'my-containers)
(require 'my-search)

(ert-deftest projects/create-is-idempotent ()
  (my-test-with-dir dir
    (let ((my/projects nil))
      (let ((a (my/project-create dir))
            (b (my/project-create (directory-file-name dir))))
        (should (eq a b))
        (should (equal (plist-get a :root) dir))
        (should (equal (plist-get a :name) (file-name-nondirectory (directory-file-name dir))))))))

(ert-deftest drives/symlinks-are-followed ()
  (my-test-with-dir dir
    (let* ((mount (file-name-as-directory (expand-file-name "mnt" dir)))
           (my/drives (list (list :name "t" :mount mount))))
      (make-directory (expand-file-name "code" mount) t)
      (make-symbolic-link (expand-file-name "code" mount) (expand-file-name "link" dir))
      (should (equal (plist-get (my/drive-for-path (expand-file-name "link/x.rs" dir)) :name) "t"))
      (should-not (my/drive-for-path "/etc/hosts")))))

(ert-deftest drives/mounted-reads-proc-mounts ()
  (should (my/drive-mounted-p '(:mount "/proc/")))
  (should-not (my/drive-mounted-p '(:mount "/definitely/not/mounted/"))))

(ert-deftest session/describes-buffers-it-can-bring-back ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir "a.txt" "x")))
      (let ((b (find-file-noselect file)))
        (unwind-protect
            (should (equal (my/session--describe b) (list 'file file)))
          (kill-buffer b))))
    (with-temp-buffer
      (rename-buffer "not something we restore" t)
      (should-not (my/session--describe (current-buffer))))))

(ert-deftest containers/run-args ()
  (let ((args (my/containers--run-args '(:name "pg" :image "postgres:17" :ports ("5433:5432")
                                         :env ("A=1") :command ("postgres" "-c" "fsync=off")))))
    (should (equal (seq-take args 4) '("run" "-d" "--name" "pg")))
    (should (member "5433:5432" args))
    (should (member "A=1" args))
    ;; the image comes before the command
    (should (< (seq-position args "postgres:17") (seq-position args "fsync=off")))))

(ert-deftest containers/definitions-are-read ()
  (my-test-with-dir dir
    (let ((file (my-test-write dir ".emacs-containers.eld" "((:name \"pg\" :image \"postgres:17\"))")))
      (should (equal (plist-get (car (my/containers--read-file file)) :name) "pg")))))

(ert-deftest search/quick-entries-match-plain-words ()
  (let ((labels (mapcar #'substring-no-properties
                        (funcall (plist-get my/search--source-quick :items)))))
    (dolist (word '("volume" "louder" "push" "scratch" "theme"))
      (should (seq-some (lambda (l) (string-match-p word l)) labels)))))

(ert-deftest search/volume-menu ()
  (let* ((entry (assoc "Volume" my/search-quick))
         (labels (mapcar #'car (funcall (nth 2 entry)))))
    (should (equal labels '("Up 5" "Up 10" "Down 5" "Down 10" "Set volume…" "Mute")))))

(ert-deftest search/flatten-submenus ()
  (should (equal (mapcar #'car (my/search--flatten '(("A" ignore) ("B" :sub (("C" ignore))) "--")))
                 '("A" "B › C"))))

;;; test-projects.el ends here
