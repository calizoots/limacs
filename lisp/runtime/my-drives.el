;;; runtime/my-drives.el  -*- lexical-binding: t; -*-

;; external drives: plug in, mount, unlock

;; Some things live on the USB drive: the music on its plain "pub"
;; partition, code projects on its VeraCrypt partition.  Anything that
;; needs a path on one of them asks for it with `my/drive-ensure':
;;
;;   mounted already            carry on
;;   plugged in, plain          mount it with udisksctl (no prompt)
;;   plugged in, VeraCrypt      open VeraCrypt's own password window and
;;                              wait for the volume to appear (the
;;                              password never goes through Emacs)
;;   not plugged in             "Plug in ..." and wait; it carries on by
;;                              itself once the drive shows up
;;
;; Any key while waiting skips the drive.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar my/drives
  '(( :name "pub" :title "the USB drive (pub)"
      :mount "/run/media/lim3r/pub/"
      :device "/dev/disk/by-label/pub"
      :how udisks)
    ( :name "vault" :title "the encrypted USB partition"
      :mount "/media/veracrypt1/"
      :device "/dev/disk/by-partuuid/f2e14322-72ac-419d-b059-6c84ebda2b37"
      :how veracrypt))
  "Drives that come and go.
:mount is where its files appear, :device the partition (a stable
/dev/disk/by-* link), :how is `udisks' (plain) or `veracrypt'.")

(defvar my/drive-wait-seconds 300
  "Give up waiting for a drive after this long.")

;;; looking things up
;; ---------------------------------------------------------------------

(defun my/drive-for-path (path)
  "The drive PATH lives on, or nil."
  (when path
    (let ((path (file-name-as-directory (expand-file-name path))))
      (seq-find (lambda (drive) (string-prefix-p (plist-get drive :mount) path))
                my/drives))))

(defun my/drive-mounted-p (drive)
  "Non-nil when DRIVE's files are there (it is a mount point right now)."
  (let ((mount (directory-file-name (plist-get drive :mount))))
    (with-temp-buffer
      (ignore-errors (insert-file-contents "/proc/self/mounts"))
      (re-search-forward (concat "^[^ ]+ " (regexp-quote mount) " ") nil t))))

(defun my/drive-plugged-p (drive)
  (file-exists-p (plist-get drive :device)))

(defun my/drive-path-available-p (path)
  "Non-nil if PATH isn't on a drive, or its drive is mounted."
  (let ((drive (my/drive-for-path path)))
    (or (not drive) (my/drive-mounted-p drive))))

;;; bringing a drive up
;; ---------------------------------------------------------------------

(defun my/drive--wait (message done-p)
  "Show MESSAGE until DONE-P is true.  Nil if a key was pressed or time ran out."
  (let ((end (+ (float-time) my/drive-wait-seconds))
        (result nil))
    (while (and (not result) (< (float-time) end))
      (cond ((funcall done-p) (setq result t))
            (t (message "%s  (any key skips)" message)
               (unless (sit-for 0.5)
                 ;; a key: eat it and skip
                 (read-event)
                 (setq end 0)))))
    (message nil)
    result))

(defun my/drive--mount-udisks (drive)
  (call-process "udisksctl" nil nil nil "mount" "--no-user-interaction"
                "-b" (plist-get drive :device))
  (or (my/drive-mounted-p drive)
      ;; may need a polkit password: let it ask this time
      (progn (call-process "udisksctl" nil nil nil "mount" "-b" (plist-get drive :device))
             (my/drive-mounted-p drive))))

(defun my/drive--unlock-veracrypt (drive why)
  "Ask VeraCrypt to unlock DRIVE (it shows its own password window)."
  (unless (executable-find "veracrypt")
    (user-error "VeraCrypt isn't installed"))
  (let* ((gave-up nil)
         (proc (make-process
                :name "veracrypt-mount" :noquery t
                :buffer (generate-new-buffer " *veracrypt*")
                :command (list "veracrypt" (plist-get drive :device)
                               (directory-file-name (plist-get drive :mount)))
                :sentinel (lambda (proc _event)
                            (when (memq (process-status proc) '(exit signal))
                              (unless (zerop (process-exit-status proc))
                                (setq gave-up t))
                              (kill-buffer (process-buffer proc)))))))
    (prog1 (my/drive--wait (format "Unlock %s in VeraCrypt%s…" (plist-get drive :title) why)
                           (lambda () (or (my/drive-mounted-p drive) gave-up)))
      (when (process-live-p proc) (ignore-errors (delete-process proc))))))

(defun my/drive-ensure (drive &optional purpose)
  "Make DRIVE's files available, asking for it if needed.
PURPOSE says what it's for (\"Music\", a project name...).  Returns
non-nil when the drive is mounted, nil if you skipped it."
  (let ((why (if purpose (format " (for %s)" purpose) "")))
    (cond
     ((my/drive-mounted-p drive) t)
     ((and (not (my/drive-plugged-p drive))
           (not (my/drive--wait (format "Plug in %s%s…" (plist-get drive :title) why)
                                (lambda () (my/drive-plugged-p drive)))))
      nil)
     ;; plugged in now: give the system a moment to settle, then mount
     ((my/drive-mounted-p drive) t)
     ((eq (plist-get drive :how) 'veracrypt)
      (and (my/drive--unlock-veracrypt drive why)
           (my/drive-mounted-p drive)))
     (t
      (message "Mounting %s…" (plist-get drive :title))
      (or (my/drive--mount-udisks drive)
          ;; udisks may race with the desktop's automounter: wait a little
          (my/drive--wait (format "Mounting %s…" (plist-get drive :title))
                          (lambda () (my/drive-mounted-p drive))))))))

(defun my/drive-ensure-path (path &optional purpose)
  "Make PATH available if it lives on a drive.  Nil if its drive was skipped."
  (let ((drive (my/drive-for-path path)))
    (or (not drive) (my/drive-ensure drive purpose))))

(defun my/drive-ensure-paths (paths)
  "Bring up the drives PATHS need, once each.
PATHS is a list of (PATH . PURPOSE).  Returns the drives that were
skipped."
  (let (needed skipped)
    (pcase-dolist (`(,path . ,purpose) paths)
      (when-let ((drive (my/drive-for-path path)))
        (unless (my/drive-mounted-p drive)
          (let ((entry (assq drive needed)))
            (if entry
                (unless (member purpose (cdr entry))
                  (setcdr entry (append (cdr entry) (list purpose))))
              (push (list drive purpose) needed))))))
    (pcase-dolist (`(,drive . ,purposes) (nreverse needed))
      (unless (my/drive-ensure drive (string-join (delq nil purposes) ", "))
        (push drive skipped)))
    skipped))

(provide 'my-drives)

;;; my-drives.el ends here
