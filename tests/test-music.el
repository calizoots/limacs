;;; tests/test-music.el  -*- lexical-binding: t; -*-

;; the music player's output choice (mpv and the state file are stubbed)

(require 'my-music)

(defconst my-test-mpv-devices "List of detected audio devices:
  'auto' (Autoselect device)
  'pipewire' (Default (pipewire))
  'pipewire/alsa_output.hdmi' (HDMI Audio)
  'pipewire/bt_headphones' (Headphones)
  'pulse/alsa_output.hdmi' (HDMI Audio)
  'pulse/bt_headphones' (Headphones)
  'alsa' (Default (alsa))
")

(defmacro my-test-with-mpv (devices &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (&rest _) (insert ,devices) 0))
             ((symbol-function 'my/music--save-state) #'ignore)
             ((symbol-function 'my/music-page-refresh) #'ignore))
     (let ((my/music--outputs-cache nil)
           (my/music-output nil))
       ,@body)))

(ert-deftest music/outputs-listed-once-with-names ()
  (my-test-with-mpv my-test-mpv-devices
    (should (equal (my/music--outputs t)
                   '(("System default") ("HDMI Audio" . "pipewire/alsa_output.hdmi")
                     ("Headphones" . "pipewire/bt_headphones"))))))

(ert-deftest music/chosen-output-goes-to-mpv ()
  (my-test-with-mpv my-test-mpv-devices
    (my/music-set-output "pipewire/bt_headphones")
    (should (equal (my/music--output-arg) '("--audio-device=pipewire/bt_headphones")))
    (should (equal (my/music--output-label) "Headphones"))))

(ert-deftest music/a-missing-output-falls-back-to-the-default ()
  (my-test-with-mpv my-test-mpv-devices
    (setq my/music-output "pipewire/gone_speaker")
    ;; not passed to mpv, so it plays through the default instead of nothing
    (should-not (my/music--output-arg))))

(ert-deftest music/default-means-no-device-argument ()
  (my-test-with-mpv my-test-mpv-devices
    (my/music-set-output nil)
    (should-not (my/music--output-arg))
    (should (equal (my/music--output-label) "System default"))))

;;; test-music.el ends here
