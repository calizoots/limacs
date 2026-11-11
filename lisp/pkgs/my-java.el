;;; pkgs/my-java.el  -*- lexical-binding: t; -*-

;; Java: Eclipse's language server (jdtls), with or without a project

;; Real projects (Maven, Gradle, Eclipse) are picked up from their build
;; file.  Loose files, like jadx's decompiled sources, need no project:
;; the folder the packages start from (worked out from the file's
;; `package' line) becomes the root, and jdtls treats it as a plain
;; source folder.  android.jar is on the classpath, so android.* resolves
;; in decompiled APK code, along with any jars in lib/ or libs/.
;;
;; jdtls lives in ~/.emacs.d/.cache/lsp/eclipse.jdt.ls, its workspace
;; data in ~/.emacs.d/.cache/lsp/jdtls-workspace.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar my/java-build-files
  '("pom.xml" "build.gradle" "build.gradle.kts" "settings.gradle" "settings.gradle.kts" ".project")
  "Files that mark the top of a real Java project.")

(defun my/java--api (jar)
  "android-37.0/android.jar -> \"37.0\"."
  (if (string-match "android-\\([0-9.]+\\)" jar) (match-string 1 jar) "0"))

(defun my/java--newest-android-jar ()
  "The newest android.jar in the Android SDK (the one in your home), or nil."
  (let ((jars (seq-mapcat
               (lambda (sdk)
                 (file-expand-wildcards (expand-file-name "platforms/android-*/android.jar" sdk)))
               (seq-filter (lambda (d) (and d (file-directory-p d)))
                           (list (getenv "ANDROID_HOME") (getenv "ANDROID_SDK_ROOT")
                                 (expand-file-name "~/Android/Sdk"))))))
    (car (sort jars (lambda (a b) (version< (my/java--api b) (my/java--api a)))))))

;;; where the code starts
;; ---------------------------------------------------------------------

(defun my/java--package ()
  "The `package' the current file declares, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*package[ \t]+\\([A-Za-z0-9_.]+\\)[ \t]*;" nil t)
        (match-string-no-properties 1)))))

(defun my/java--source-root (file)
  "The folder FILE's packages start from (com/foo/Bar.java -> the folder above com)."
  (let ((dir (file-name-directory file))
        (package (my/java--package)))
    (if (null package)
        dir
      (let ((parts (split-string package "\\." t))
            (here (directory-file-name dir)))
        ;; only strip when the folders really match the package
        (if (string-suffix-p (string-join parts "/") here)
            (file-name-as-directory
             (substring here 0 (- (length here) (length (string-join parts "/")) 1)))
          dir)))))

(defun my/java-root (&optional file)
  "Where jdtls should see FILE from: its project, or its source folder."
  (let ((file (or file buffer-file-name)))
    (when file
      (or (locate-dominating-file
           file (lambda (dir)
                  (seq-some (lambda (f) (file-exists-p (expand-file-name f dir)))
                            my/java-build-files)))
          (my/java--source-root file)))))

(defun my/java--calculate-root (_session file-name)
  "For Java files, the root without asking (see `my/java-root')."
  (when (and file-name (derived-mode-p 'java-mode 'java-ts-mode))
    (my/java-root file-name)))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--calculate-root :before-until #'my/java--calculate-root))

;;; the server
;; ---------------------------------------------------------------------

(use-package lsp-java
  :ensure t
  :defer t
  :init
  (setq lsp-java-workspace-dir
        (expand-file-name ".cache/lsp/jdtls-workspace/" user-emacs-directory)
        lsp-java-workspace-cache-dir
        (expand-file-name ".cache/lsp/jdtls-workspace/.cache/" user-emacs-directory))
  :config
  (setq lsp-java-project-referenced-libraries
        (vconcat '("lib/**/*.jar" "libs/**/*.jar")
                 (when-let ((jar (my/java--newest-android-jar))) (list jar)))
        ;; for folders with no build file, the root itself is where the
        ;; packages start (jdtls otherwise guesses the opened file's folder)
        lsp-java-project-source-paths ["."]
        ;; quiet: no code lenses over every method, no progress spam
        lsp-java-references-code-lens-enabled nil
        lsp-java-implementations-code-lens-enabled nil
        lsp-java-format-enabled t
        lsp-java-save-actions-organize-imports nil
        lsp-java-autobuild-enabled t
        lsp-java-max-concurrent-builds 2
        lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90"
                          "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

(with-eval-after-load 'lsp-mode
  (require 'lsp-java))

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-ts-mode-hook #'lsp)

(provide 'my-java)

;;; my-java.el ends here
