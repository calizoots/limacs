;;; pkgs/my-mail.el  -*- lexical-binding: t; -*-

;; email: mu4e + mbsync + msmtp, with PGP

;; Mail is downloaded by mbsync (~/.mbsyncrc) into ~/Mail/<account>/, read
;; and searched with mu4e, and sent with msmtp (~/.config/msmtp/config).
;; Passwords live in the desktop keyring; neither config holds one.
;;
;; The accounts themselves are listed in mail-accounts.el (private, not in
;; git).  Each account becomes a mu4e context, so replying from an account
;; uses its address, folders and PGP key automatically.
;;
;; PGP goes through GnuPG.  Mail is signed with the key that belongs to the
;; From address, encrypted mail is decrypted on open, and replies to
;; encrypted mail are encrypted and signed again.  Encrypting looks up
;; recipients' keys (WKD, then keys.openpgp.org) when they aren't known yet.
;;
;; Keys anywhere (normal state)
;; ---------------------------------------------------------------------
;;   SPC i i  open the Mail tab (all inboxes)    SPC i c  write a mail
;;   SPC i s  search mail                        SPC i u  fetch mail now
;;
;; Keys in the mail list
;; ---------------------------------------------------------------------
;;   RET  open          r  reply       R  reply all     f  forward
;;   c    write         s  search      b  bookmarks     J  jump to folder
;;   a    archive       d  trash       D  delete        +  -  flag / unflag
;;   !    mark read     ?  mark unread u  unmark        x  apply marks
;;   gr   refresh       gu fetch mail  ;  switch account  q  back
;;
;; Keys when reading
;; ---------------------------------------------------------------------
;;   J K  next / previous mail   r R f  reply / all / forward
;;   a d  archive / trash        S  save attachments    q  back to list
;;
;; Keys when writing (normal state)
;; ---------------------------------------------------------------------
;;   ,s   sign                   ,e  encrypt (and sign)  ,n  no PGP
;;   ,a   attach a file          ,d  save draft
;;   ZZ   send                   ZQ  throw the draft away

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar my/mail-accounts nil
  "Mail accounts, set in `my/mail-accounts-file'.")

(defvar my/mail-accounts-file (locate-user-emacs-file "mail-accounts.el")
  "Private file listing the mail accounts.")

(defvar my/mail-mu4e-directory "/usr/share/emacs/site-lisp/mu4e"
  "Where Fedora's maildir-utils package puts mu4e.")

(defvar my/mail-keyserver "hkps://keys.openpgp.org"
  "Keyserver used to find recipients' keys.")

(when (file-exists-p my/mail-accounts-file)
  (load my/mail-accounts-file nil t))

(when (file-directory-p my/mail-mu4e-directory)
  (add-to-list 'load-path my/mail-mu4e-directory))

;;; accounts
;; ---------------------------------------------------------------------

(defun my/mail--accounts ()
  "Accounts that are set up (have :ready t)."
  (seq-filter (lambda (account) (plist-get account :ready)) my/mail-accounts))

(defun my/mail--folder (account name)
  (concat (plist-get account :maildir) "/" name))

(defun my/mail--inbox-query ()
  (mapconcat (lambda (account) (format "maildir:%s" (my/mail--folder account "inbox")))
             (my/mail--accounts) " OR "))

(defun my/mail--fetch-command ()
  "mbsync for every ready account (group names match the maildirs)."
  (concat "mbsync "
          (mapconcat (lambda (account) (substring (plist-get account :maildir) 1))
                     (my/mail--accounts) " ")))

(defun my/mail--context (account)
  "A mu4e context for ACCOUNT."
  (let ((prefix (concat (plist-get account :maildir) "/")))
    (make-mu4e-context
     :name (plist-get account :name)
     :match-func (lambda (msg)
                   (and msg (string-prefix-p prefix (or (mu4e-message-field msg :maildir) ""))))
     :vars `((user-mail-address . ,(plist-get account :address))
             (user-full-name . ,(plist-get account :full-name))
             (mu4e-sent-folder . ,(my/mail--folder account "sent"))
             (mu4e-drafts-folder . ,(my/mail--folder account "drafts"))
             (mu4e-trash-folder . ,(my/mail--folder account "trash"))
             (mu4e-refile-folder . ,(my/mail--folder account "archive"))
             ;; gmail files sent mail itself; a second copy would duplicate it
             (mu4e-sent-messages-behavior . ,(if (plist-get account :gmail) 'delete 'sent))
             (mml-secure-openpgp-signers . ,(and (plist-get account :key)
                                                 (list (plist-get account :key))))))))

;;; pgp
;; ---------------------------------------------------------------------

(defun my/mail--recipients ()
  "Addresses in To, Cc and Bcc of the mail being written."
  (let (addresses)
    (dolist (field '("To" "Cc" "Bcc"))
      (when-let ((value (message-fetch-field field)))
        (dolist (parsed (mail-extract-address-components value t))
          (when (cadr parsed) (push (downcase (cadr parsed)) addresses)))))
    (delete-dups addresses)))

(defun my/mail--have-key-p (address)
  (zerop (call-process "gpg" nil nil nil "--batch" "--list-keys" (concat "<" address ">"))))

(defun my/mail--locate-key (address)
  "Look ADDRESS's key up online (WKD, then the keyserver).  Non-nil if found."
  (zerop (call-process "gpg" nil nil nil "--batch" "--keyserver" my/mail-keyserver
                       "--auto-key-locate" "clear,wkd,keyserver" "--locate-keys" address)))

(defun my/mail-encrypt ()
  "Encrypt (and sign) the mail being written, fetching missing keys first."
  (interactive)
  (let* ((recipients (my/mail--recipients))
         (missing (seq-remove #'my/mail--have-key-p recipients)))
    (unless recipients (user-error "Add a recipient first"))
    (when missing
      (message "Looking up keys for %s…" (string-join missing ", "))
      (let ((still (seq-remove #'my/mail--locate-key missing)))
        (when still
          (user-error "No PGP key found for %s, so they can't read encrypted mail"
                      (string-join still ", ")))))
    (mml-secure-message-encrypt-pgpmime)
    (message "Will encrypt and sign for %s" (string-join recipients ", "))))

(defun my/mail-sign ()
  "Sign the mail being written."
  (interactive)
  (mml-secure-message-sign-pgpmime)
  (message "Will sign"))

(defun my/mail-no-pgp ()
  "Neither sign nor encrypt the mail being written."
  (interactive)
  (mml-unsecure-message)
  (message "No PGP"))

(defun my/mail--security ()
  "How the mail being written will go out, for its header line."
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward "<#secure[^>]*mode=signencrypt" nil t)
           (propertize "🔒 encrypted + signed" 'face 'success))
          ((re-search-forward "<#secure[^>]*mode=encrypt" nil t)
           (propertize "🔒 encrypted" 'face 'success))
          ((progn (goto-char (point-min)) (re-search-forward "<#secure[^>]*mode=sign" nil t))
           (propertize "✓ signed" 'face 'success))
          (t (propertize "not signed" 'face 'shadow)))))

(defun my/mail--compose-header ()
  (setq header-line-format
        '(:eval (concat " "
                        (propertize (or (message-fetch-field "From") "") 'face 'bold)
                        "   " (my/mail--security)
                        (propertize "    ,s sign  ,e encrypt  ,n no pgp  ,a attach  ZZ send  ZQ discard"
                                    'face 'shadow)))))

;;; mail tab
;; ---------------------------------------------------------------------

(defvar my/mail-folders '("inbox" "sent" "drafts" "trash" "spam" "archive")
  "Folders every account has locally.")

(defun my/mail--ensure-folders ()
  "Create any missing local folders, so mu4e never stops to ask.
note: mbsync only creates a folder once it has synced it"
  (let ((root (expand-file-name "~/Mail")))
    (dolist (account (my/mail--accounts))
      (dolist (folder my/mail-folders)
        (dolist (sub '("cur" "new" "tmp"))
          ;; note: maildirs start with "/", so join by hand rather than expand
          (make-directory (concat root (my/mail--folder account folder) "/" sub) t))))))

(defun my/mail ()
  "Open the Mail tab with every inbox."
  (interactive)
  (unless (my/mail--accounts)
    (user-error "No mail accounts are ready (see %s)" my/mail-accounts-file))
  (require 'mu4e)
  (my/mail--ensure-folders)
  (let ((index (seq-position (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs))
                             "Mail")))
    (if index
        (tab-bar-select-tab (1+ index))
      (tab-new)
      (tab-bar-rename-tab "Mail"))
    (delete-other-windows)
    ;; start mu4e quietly, then go straight to the inboxes
    (mu4e t)
    (mu4e-search (my/mail--inbox-query))))

(defun my/mail-compose ()
  "Write a new mail (asks which account)."
  (interactive)
  (require 'mu4e)
  (my/mail--ensure-folders)
  (mu4e t)
  (mu4e-compose-new))

(defun my/mail-search ()
  "Search all mail."
  (interactive)
  (require 'mu4e)
  (my/mail--ensure-folders)
  (mu4e t)
  (call-interactively #'mu4e-search))

(defun my/mail-fetch ()
  "Fetch mail now."
  (interactive)
  (require 'mu4e)
  (my/mail--ensure-folders)
  (mu4e t)
  (mu4e-update-mail-and-index t))

;;; mu4e setup
;; ---------------------------------------------------------------------

(with-eval-after-load 'mu4e
  (setq mu4e-get-mail-command (my/mail--fetch-command)
        mu4e-update-interval 300
        ;; mbsync needs new file names when mail moves between folders
        mu4e-change-filenames-when-moving t
        mu4e-hide-index-messages t
        mu4e-headers-auto-update t
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        mu4e-contexts (mapcar #'my/mail--context (my/mail--accounts))
        mu4e-confirm-quit nil
        mu4e-attachment-dir (expand-file-name "~/Downloads")
        mu4e-headers-date-format "%d %b %y"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-fields '((:human-date . 10) (:flags . 6) (:from-or-to . 26) (:subject))
        mu4e-bookmarks
        `((:name "All inboxes" :query ,(my/mail--inbox-query) :key ?i)
          (:name "Unread" :query "flag:unread AND NOT flag:trashed AND NOT maildir:/.*spam/"
           :key ?u)
          (:name "Today" :query "date:today..now" :key ?t)
          (:name "Flagged" :query "flag:flagged" :key ?f)
          (:name "Encrypted" :query "flag:encrypted" :key ?e))
        mu4e-maildir-shortcuts
        (cl-loop for account in (my/mail--accounts)
                 for key from ?1
                 collect (list :maildir (my/mail--folder account "inbox")
                               :name (plist-get account :name)
                               :key key)))
  ;; tell mu which addresses are you (marks your own mail correctly)
  (setq mu4e-user-mail-address-list
        (mapcar (lambda (account) (plist-get account :address)) my/mail-accounts)))

;;; sending
;; ---------------------------------------------------------------------

(setq sendmail-program (or (executable-find "msmtp") "msmtp")
      send-mail-function #'message-send-mail-with-sendmail
      message-send-mail-function #'message-send-mail-with-sendmail
      ;; msmtp picks the account from the From address
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-kill-buffer-on-exit t)

;;; reading
;; ---------------------------------------------------------------------

;; html mail: keep the theme's colours and font instead of the sender's
;; (otherwise newsletters paint white boxes over a dark theme)
(setq shr-use-colors nil
      shr-use-fonts nil
      shr-max-width 100
      ;; prefer the plain-text version when a mail has both
      mm-discouraged-alternatives '("text/html" "text/richtext"))

;;; pgp setup
;; ---------------------------------------------------------------------

(setq mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t
      mm-verify-option 'always
      mm-decrypt-option 'always
      ;; the passphrase prompt comes from gpg-agent's own window
      epg-pinentry-mode nil)

(with-eval-after-load 'mu4e-compose
  (add-hook 'mu4e-compose-mode-hook #'my/mail--compose-header))

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>ii") #'my/mail
    (kbd "<leader>ic") #'my/mail-compose
    (kbd "<leader>is") #'my/mail-search
    (kbd "<leader>iu") #'my/mail-fetch)

  (with-eval-after-load 'mu4e
    (dolist (mode '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode))
      (evil-set-initial-state mode 'normal))

    (evil-define-key 'normal mu4e-headers-mode-map
      (kbd "RET") #'mu4e-headers-view-message
      "r" #'mu4e-compose-reply
      "R" #'mu4e-compose-wide-reply
      "f" #'mu4e-compose-forward
      "c" #'mu4e-compose-new
      "s" #'mu4e-search
      "b" #'mu4e-search-bookmark
      "J" #'mu4e-search-maildir
      "a" #'mu4e-headers-mark-for-refile
      "d" #'mu4e-headers-mark-for-trash
      "D" #'mu4e-headers-mark-for-delete
      "+" #'mu4e-headers-mark-for-flag
      "-" #'mu4e-headers-mark-for-unflag
      "!" #'mu4e-headers-mark-for-read
      "?" #'mu4e-headers-mark-for-unread
      "u" #'mu4e-headers-mark-for-unmark
      "x" #'mu4e-mark-execute-all
      ";" #'mu4e-context-switch
      (kbd "g r") #'mu4e-search-rerun
      (kbd "g u") #'my/mail-fetch
      "q" #'mu4e-quit)

    (evil-define-key 'normal mu4e-view-mode-map
      "J" #'mu4e-view-headers-next
      "K" #'mu4e-view-headers-prev
      "r" #'mu4e-compose-reply
      "R" #'mu4e-compose-wide-reply
      "f" #'mu4e-compose-forward
      "a" #'mu4e-view-mark-for-refile
      "d" #'mu4e-view-mark-for-trash
      "S" #'mu4e-view-save-attachments
      "q" #'quit-window))

  (with-eval-after-load 'mu4e-compose
    (evil-define-key 'normal mu4e-compose-mode-map
      ",s" #'my/mail-sign
      ",e" #'my/mail-encrypt
      ",n" #'my/mail-no-pgp
      ",a" #'mml-attach-file
      ",d" #'message-dont-send
      "ZZ" #'message-send-and-exit
      "ZQ" #'message-kill-buffer)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC i" "mail"))

(provide 'my-mail)

;;; my-mail.el ends here
