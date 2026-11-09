;;; pkgs/my-search.el  -*- lexical-binding: t; -*-

;; Search Everywhere, like JetBrains' double-Shift

;; One prompt for everything you might be looking for: open tabs and
;; buffers, recent files, the current project's files, symbols in the
;; file you came from, projects, run configurations, commands and music.
;; Results are grouped by kind; TAB / S-TAB step through the kinds
;; (All → Tabs → Buffers → ...), or type a kind's letter and a space
;; ("f " files only, "a " actions only...).  Backspace on an empty
;; prompt goes back to All.
;;
;;   SPC SPC   search everywhere
;;   SPC m /   search music (also / on the music page): songs, artists and
;;             playlists; picking one lists what to do (RET RET plays)
;;   C-RET     pick and do the first option straight away
;;
;; Built on consult's multi-source reading, shown in the vertico popup.

(require 'cl-lib)
(require 'subr-x)
(require 'consult)
(require 'consult-imenu)
(require 'recentf)

;; set from vertico and its popup (let-bound below, so they must be special)
(defvar vertico-posframe-width)
(defvar vertico-posframe-height)
(defvar vertico-count)
(defvar my/music-mode-map)

;;; settings
;; ---------------------------------------------------------------------

(defvar my/search-file-cache-seconds 30
  "How long a project's file list is reused before listing it again.")

(defvar my/search-max-files 50000
  "Stop listing a project's files after this many.")

;; recent files feed the "Recent" group (the list is saved in recentf,
;; which is gitignored)
(setq recentf-max-saved-items 300
      recentf-auto-cleanup 'never)   ; don't touch unplugged drives at startup
(recentf-mode 1)

;;; where the search started
;; ---------------------------------------------------------------------

(defvar my/search--origin nil
  "Buffer the search was started from (symbols come from it).")

(defvar my/search--root nil
  "Project root of the tab the search was started from, or nil.")

(defun my/search--project-root ()
  (or (and (fboundp 'my/project-root) (my/project-root))
      (when-let ((project (project-current nil)))
        (project-root project))))

;;; sources
;; ---------------------------------------------------------------------

;; tabs

(defvar my/search--source-tabs
  `( :name "Tabs" :narrow ?t :category my-tab
     :items
     ,(lambda ()
        (let ((i 0))
          (mapcar (lambda (tab)
                    (cl-incf i)
                    (let ((label (if (fboundp 'my/tab--label)
                                     (my/tab--label tab (my/tab--project tab))
                                   (alist-get 'name (cdr tab)))))
                      (cons label i)))
                  (funcall tab-bar-tabs-function))))
     :annotate ,(lambda (n) (format "  tab %d" n))
     :action ,#'tab-bar-select-tab))

;; buffers: consult's own source (preview included), minus the noise

(defvar my/search--source-buffers
  (append (list :name "Buffers" :narrow ?b)
          (cl-loop for (k v) on consult-source-buffer by #'cddr
                   unless (memq k '(:name :narrow :default))
                   append (list k v))))

;; recent files

(defvar my/search--source-recent
  `( :name "Recent files" :narrow ?r :category file
     :state ,#'consult--file-state
     :items
     ,(lambda ()
        (let ((open (mapcar #'buffer-file-name (buffer-list))))
          (seq-take
           (seq-remove (lambda (file) (member (expand-file-name file) open))
                       (mapcar #'abbreviate-file-name recentf-list))
           200)))))

;; the project's files

(defvar my/search--file-cache (make-hash-table :test #'equal)
  "Project root -> (TIME . FILES).")

(defun my/search--list-files (root)
  "Files under ROOT, relative to it, ignoring what .gitignore ignores."
  (let ((default-directory root))
    (with-temp-buffer
      (cond ((executable-find "fd")
             (call-process "fd" nil t nil "--type" "f" "--hidden" "--no-require-git" "--exclude" ".git"
                           ;; emacs backups, autosaves and lock files
                           "--exclude" "*~" "--exclude" "#*#" "--exclude" ".#*"
                           "--color" "never"))
            ((executable-find "rg")
             (call-process "rg" nil t nil "--files" "--hidden" "--no-require-git" "--glob" "!.git"
                           "--glob" "!*~" "--glob" "!#*#" "--glob" "!.#*"))
            (t (insert (mapconcat (lambda (f) (file-relative-name f root))
                                  (directory-files-recursively root "" nil) "\n"))))
      (let ((files (split-string (buffer-string) "\n" t)))
        (seq-take files my/search-max-files)))))

(defun my/search--project-files (root)
  (let ((hit (gethash root my/search--file-cache)))
    (if (and hit (< (- (float-time) (car hit)) my/search-file-cache-seconds))
        (cdr hit)
      (let ((files (my/search--list-files root)))
        (puthash root (cons (float-time) files) my/search--file-cache)
        files))))

(defvar my/search--source-files
  `( :name "Project files" :narrow ?f :category file
     :enabled ,(lambda () my/search--root)
     :state
     ,(lambda ()
        ;; candidates are relative to the project root
        (let ((preview (consult--file-state)))
          (lambda (action cand)
            (funcall preview action
                     (and cand (expand-file-name cand my/search--root))))))
     :items ,(lambda () (my/search--project-files my/search--root))))

;; symbols in the buffer the search started from

(defvar my/search--source-symbols
  `( :name "Symbols in this file" :narrow ?s :category my-symbol
     :enabled ,(lambda () (and (buffer-live-p my/search--origin)
                               (with-current-buffer my/search--origin
                                 (or buffer-file-name (derived-mode-p 'prog-mode)))))
     :items
     ,(lambda ()
        (with-current-buffer my/search--origin
          (mapcar (lambda (item) (cons (car item) (cdr item)))
                  (ignore-errors (consult-imenu--items)))))
     :annotate ignore
     :action
     ,(lambda (pos)
        (when (consp pos) (setq pos (car pos)))   ; some modes give (pos . extra)
        (switch-to-buffer (if (markerp pos) (marker-buffer pos) my/search--origin))
        (goto-char pos)
        (recenter))))

;; projects

(defvar my/search--source-projects
  `( :name "Projects" :narrow ?p :category my-project
     :enabled ,(lambda () (fboundp 'my/project-known-roots))
     :items
     ,(lambda ()
        (mapcar (lambda (root)
                  (cons (concat (file-name-nondirectory (directory-file-name root))
                                "  "
                                (propertize (abbreviate-file-name root) 'face 'shadow))
                        root))
                (my/project-known-roots)))
     :annotate ignore
     :action ,(lambda (root) (my/project-switch root))))

;; run configurations of the current project

(defvar my/search--source-run
  `( :name "Run" :narrow ?x :category my-run
     :enabled ,(lambda () (and my/search--root (fboundp 'my/run-configs)))
     :items
     ,(lambda ()
        (mapcar (lambda (spec)
                  (cons (concat (plist-get spec :name)
                                "  "
                                (propertize (my/run--command-string (plist-get spec :command))
                                            'face 'shadow))
                        spec))
                (ignore-errors (my/run-configs my/search--root))))
     :annotate ,(lambda (spec) (and (eq (plist-get spec :kind) 'server)
                                    (propertize "  server" 'face 'success)))
     :action ,(lambda (spec) (my/run-spec spec my/search--root))))

;; actions: every command, the ones you've used recently first

(defvar my/search--source-actions
  `( :name "Actions" :narrow ?a :category command
     :history extended-command-history
     :items
     ,(lambda ()
        (let ((seen (make-hash-table :test #'equal))
              recent rest)
          (dolist (name extended-command-history)
            (when (and (not (gethash name seen)) (commandp (intern-soft name)))
              (puthash name t seen)
              (push name recent)))
          (mapatoms (lambda (sym)
                      (when (and (commandp sym)
                                 (not (get sym 'byte-obsolete-info))
                                 (not (gethash (symbol-name sym) seen)))
                        (push (symbol-name sym) rest))))
          (nconc (nreverse recent) rest)))
     :action
     ,(lambda (cand)
        (let ((command (intern cand)))
          (add-to-history 'extended-command-history cand)
          (setq this-command command)
          (command-execute command 'record)))))

;; music: songs, artists and playlists.  Picking one lists what you can
;; do with it (Play first, so RET RET plays); C-RET does the first thing
;; straight away.

(defvar my/search--quick nil
  "Non-nil when the result was picked with C-RET: skip the options list.")

(defun my/search-quick-exit ()
  "Pick the highlighted result and do its first option straight away."
  (interactive)
  (setq my/search--quick t)
  (vertico-exit))

(defun my/search--flatten (items &optional prefix)
  "Menu ITEMS (see `my/menu') as a list of (LABEL . COMMAND)."
  (cl-loop for item in items
           append
           (pcase item
             (`(,label :sub ,sub)
              (my/search--flatten sub (concat prefix label " › ")))
             (`(,label ,command . ,props)
              (when (or (not (plist-member props :enable))
                        (eval (plist-get props :enable) t))
                (list (cons (concat prefix label) command)))))))

(defun my/search--choose (title items)
  "Ask which of the menu ITEMS to do (the first one when C-RET was used)."
  (let* ((choices (my/search--flatten items))
         (choice (if my/search--quick
                     (car choices)
                   (assoc (completing-read (format "%s: " title)
                                           (lambda (string pred action)
                                             (if (eq action 'metadata)
                                                 '(metadata (display-sort-function . identity))
                                               (complete-with-action action choices string pred)))
                                           nil t)
                          choices))))
    (when choice
      (call-interactively (cdr choice)))))

(defun my/search--music-ready-p ()
  (and (fboundp 'my/music--library-tracks)
       (boundp 'emms-cache-db)
       (> (hash-table-count emms-cache-db) 0)))

(defmacro my/search--do (&rest body)
  `(lambda () (interactive) ,@body))

(defun my/search--track-options (track)
  (ignore-errors (my/music-ensure-mounted))
  `(("Play" ,(my/search--do (my/music--append track t)))
    ("Play next" ,(my/search--do (my/music--insert-next (list track))))
    ("Add to queue" ,(my/search--do (my/music--append track)
                                    (message "Queued %s" (my/music--title track))))
    ("Add to playlist" :sub ,(my/music--playlist-items (list track)))
    ,@(my/music--track-items track)
    ("Open the music page" my/music)))

(defun my/search--artist-tracks (artist)
  (seq-filter (lambda (track) (equal (my/music--artist track) artist))
              (my/music--library-tracks)))

(defun my/search--artist-options (artist)
  (ignore-errors (my/music-ensure-mounted))
  (let ((tracks (my/search--artist-tracks artist)))
    `(("Play all" ,(my/search--do (my/music--queue tracks)))
      ("Shuffle all" ,(my/search--do (my/music--queue tracks t)))
      ("Play all next" ,(my/search--do (my/music--insert-next tracks)))
      ("Add all to queue" ,(my/search--do (dolist (track tracks) (my/music--append track))
                                          (message "Queued %s" (my/music--plural (length tracks) "track"))))
      ("Add all to playlist" :sub ,(my/music--playlist-items tracks))
      ("Show their tracks" ,(my/search--do (my/music--open-view nil artist))))))

(defun my/search--playlist-options (file)
  (ignore-errors (my/music-ensure-mounted))
  (let ((tracks (my/music--playlist-tracks file)))
    `(("Play" ,(my/search--do (my/music-load-playlist file)) :enable ,(and tracks t))
      ("Shuffle" ,(my/search--do (my/music--queue tracks t)) :enable ,(and tracks t))
      ("Play next" ,(my/search--do (my/music--insert-next tracks)) :enable ,(and tracks t))
      ("Add to queue" ,(my/search--do (dolist (track tracks) (my/music--append track))
                                      (message "Queued %s" (my/music--plural (length tracks) "track")))
       :enable ,(and tracks t))
      ("Open (edit, reorder)" ,(my/search--do (my/music--open-view file)))
      ("Rename…" ,(my/search--do (my/music-rename-playlist file)))
      ("Duplicate…" ,(my/search--do (my/music-duplicate-playlist file)))
      ("Delete…" ,(my/search--do (my/music-delete-playlist file))))))

(defvar my/search--source-songs
  `( :name "Songs" :narrow (?m . "Music") :category my-music
     :enabled my/search--music-ready-p
     :items
     ,(lambda ()
        (mapcar (lambda (track)
                  (cons (concat (my/music--title track) "  "
                                (propertize (my/music--artist track) 'face 'shadow))
                        track))
                (sort (my/music--library-tracks)
                      (lambda (a b) (string-collate-lessp (my/music--title a)
                                                          (my/music--title b) nil t)))))
     :annotate
     ,(lambda (track)
        ;; for (DISPLAY . VALUE) items consult passes the value
        (concat "  " (propertize (string-join
                                  (delq nil (list (emms-track-get track 'info-album)
                                                  (my/music--time (emms-track-get track 'info-playing-time))))
                                  " · ")
                                 'face 'shadow)))
     :action
     ,(lambda (track)
        (my/search--choose (my/music--title track) (my/search--track-options track)))))

(defvar my/search--source-artists
  `( :name "Artists" :narrow (?m . "Music") :category my-music
     :annotate ignore
     :enabled my/search--music-ready-p
     :items
     ,(lambda ()
        (let ((counts (make-hash-table :test #'equal)))
          (dolist (track (my/music--library-tracks))
            (cl-incf (gethash (my/music--artist track) counts 0)))
          (mapcar (lambda (artist)
                    (cons (concat artist "  "
                                  (propertize (my/music--plural (gethash artist counts) "song")
                                              'face 'shadow))
                          artist))
                  (sort (hash-table-keys counts) #'string-collate-lessp))))
     :action
     ,(lambda (artist)
        (my/search--choose artist (my/search--artist-options artist)))))

(defvar my/search--source-playlists
  `( :name "Playlists" :narrow (?m . "Music") :category my-music
     :annotate ignore
     :enabled ,(lambda () (and (fboundp 'my/music--playlists) (my/music--playlists)))
     :items
     ,(lambda ()
        (mapcar (lambda (file)
                  (cons (concat "≡ " (file-name-base file) "  "
                                (propertize (my/music--plural (my/music--playlist-size file) "track")
                                            'face 'shadow))
                        file))
                (my/music--playlists)))
     :action
     ,(lambda (file)
        (my/search--choose (file-name-base file) (my/search--playlist-options file)))))

(defvar my/search-sources
  '(my/search--source-tabs
    my/search--source-buffers
    my/search--source-recent
    my/search--source-files
    my/search--source-symbols
    my/search--source-run
    my/search--source-projects
    my/search--source-actions
    my/search--source-playlists
    my/search--source-artists
    my/search--source-songs)
  "What Search Everywhere looks through, in the order the groups show.")

;;; stepping through the kinds with TAB
;; ---------------------------------------------------------------------

(defun my/search--cycle (step)
  "Narrow to the next (STEP 1) or previous (STEP -1) kind; All sits between."
  (let* ((keys (cons nil (delete-dups (mapcar #'car (plist-get consult--narrow-config :keys)))))
         (index (or (cl-position consult--narrow keys) 0))
         (next (nth (mod (+ index step) (length keys)) keys)))
    (consult-narrow next)))

(defun my/search-next-kind ()
  (interactive)
  (my/search--cycle 1))

(defun my/search-previous-kind ()
  (interactive)
  (my/search--cycle -1))

(defvar-keymap my/search-map
  "TAB" #'my/search-next-kind
  "<tab>" #'my/search-next-kind
  "<backtab>" #'my/search-previous-kind
  "C-<return>" #'my/search-quick-exit
  "C-RET" #'my/search-quick-exit)

;;; the command
;; ---------------------------------------------------------------------

(defun my/search-everywhere (&optional narrow prompt)
  "Search tabs, buffers, files, symbols, projects, run configs, actions and music.
NARROW is a kind's letter to start in (?m music, ?f files...), PROMPT
replaces \"Search everywhere: \"."
  (interactive)
  (let* ((my/search--quick nil)
         (my/search--origin (if (minibufferp) (window-buffer (minibuffer-selected-window))
                              (current-buffer)))
         (my/search--root (my/search--project-root))
         (default-directory (or my/search--root default-directory)))
    (consult--multi my/search-sources
                    :prompt (or prompt "Search everywhere: ")
                    :keymap my/search-map
                    :initial-narrow narrow
                    :require-match t
                    :sort nil)))

(defun my/search--popup (fn)
  "Run FN in a wide vertico popup."
  (let ((vertico-posframe-width 110)
        (vertico-posframe-height 22)
        (vertico-count 20))
    (if (fboundp 'my/with-vertico)
        (my/with-vertico fn)
      (call-interactively fn))))

(defun my/search-everywhere-popup ()
  "`my/search-everywhere' in the vertico popup."
  (interactive)
  (my/search--popup #'my/search-everywhere))

(defun my/search-music ()
  "Search songs, artists and playlists, each with a list of what to do."
  (interactive)
  (when (fboundp 'my/music--ensure-library)
    (my/music--ensure-library))
  (my/search--popup #'my/search--music-1))

(defun my/search--music-1 ()
  (interactive)
  (my/search-everywhere ?m "Search music: "))

(with-eval-after-load 'vertico-multiform
  ;; the popup is chosen by the command the key ran, so list each entry point
  (dolist (command '(my/search-everywhere my/search-everywhere-popup
					  my/search-music my/search--music-1))
    (add-to-list 'vertico-multiform-commands (list command 'posframe))))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
		   (kbd "<leader>SPC") #'my/search-everywhere-popup
		   (kbd "<leader>m/") #'my/search-music)
  ;; on the music page / searches music the same way
  (with-eval-after-load 'my-music
    (evil-define-key 'normal my/music-mode-map "/" #'my/search-music)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC SPC" "search everywhere")
  (which-key-add-key-based-replacements "SPC m /" "search music"))

(provide 'my-search)

;;; my-search.el ends here
