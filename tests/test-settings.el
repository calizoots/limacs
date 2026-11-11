;;; tests/test-settings.el  -*- lexical-binding: t; -*-

;; the Settings page: defaults, saving, applying, stepping

(require 'tabs)
(require 'settings-page)

(defvar my-test-number 10)
(defvar my-test-flag nil)
(defvar my-test-float 0.2)

(defmacro my-test-with-settings (&rest body)
  "BODY with an empty settings file and a scratch setting registered."
  `(let* ((my/settings-file (make-temp-file "settings" nil ".eld"))
          (my/settings (copy-sequence my/settings))
          (my-test-number 10)
          (my-test-flag nil))
     (delete-file my/settings-file)
     (unwind-protect
         (progn
           (my/defsetting 'test-number :section "Test" :title "A number" :type 'number
                          :min 0 :max 20 :step 5 :var 'my-test-number)
           (my/defsetting 'test-flag :section "Test" :title "A flag" :type 'toggle :var 'my-test-flag)
           ,@body)
       (ignore-errors (delete-file my/settings-file)))))

(ert-deftest settings/defaults-are-what-it-started-with ()
  (my-test-with-settings
   (should (equal (plist-get (my/settings--find 'test-number) :default) 10))
   (should (equal (plist-get (my/settings--find 'test-flag) :default) nil))))

(ert-deftest settings/only-changes-are-saved ()
  (my-test-with-settings
   (my/settings-set 'test-flag t)
   (should (eq my-test-flag t))
   (should (equal (my/settings--saved) '((test-flag . t))))
   ;; back to the default: forgotten
   (my/settings-set 'test-flag nil)
   (should (equal (my/settings--saved) nil))))

(ert-deftest settings/saved-values-come-back ()
  (my-test-with-settings
   (my/settings-set 'test-number 15)
   (setq my-test-number 10)
   (my/settings-apply-saved)
   (should (= my-test-number 15))))

(ert-deftest settings/numbers-step-and-stay-in-range ()
  (my-test-with-settings
   (my/settings-step 'test-number 1)
   (should (= my-test-number 15))
   (my/settings-step 'test-number 1)
   (my/settings-step 'test-number 1)
   (should (= my-test-number 20))
   (dotimes (_ 10) (my/settings-step 'test-number -1))
   (should (= my-test-number 0))))

(ert-deftest settings/float-steps-stay-tidy ()
  (my-test-with-settings
   (setq my-test-float 0.2)
   (my/defsetting 'test-float :section "Test" :title "F" :type 'number :min 0.0 :max 1.0 :step 0.05
                  :var 'my-test-float)
   (dotimes (_ 3) (my/settings-step 'test-float 1))
   (should (equal my-test-float 0.35))))

(ert-deftest settings/choices-show-labels ()
  (let ((s '(:type choice :choices (("Automatic" . nil) ("podman" . "podman")))))
    (should (equal (my/settings--label s nil) "Automatic"))
    (should (equal (my/settings--label s "podman") "podman"))))

(ert-deftest settings/the-page-draws-every-section ()
  (with-temp-buffer
    (my/settings-mode)
    (my/settings--render)
    (dolist (section '("APPEARANCE" "EDITOR" "SOURCE CONTROL" "MUSIC"))
      (should (string-match-p section (buffer-string))))
    ;; the filter narrows it down
    (my/settings-filter "volume")
    (should (string-match-p "Volume step" (buffer-string)))
    (should-not (string-match-p "Font size" (buffer-string)))))

;;; test-settings.el ends here
