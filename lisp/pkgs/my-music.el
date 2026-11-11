;;; pkgs/my-music.el  -*- lexical-binding: t; -*-

;; music player built on emms + mpv

;; Plays the collection in `my/music-directory' through mpv, replacing
;; Elisa.  The drive is mounted on demand with udisksctl.  Track info comes
;; from the tags when they exist (emms reads mp3, flac, ogg and opus itself,
;; ffprobe covers the rest) and falls back to the folder layout
;; (Artist/Album/01 - Title.ext), so untagged rips still sort properly.
;;
;; Spotify links (track, album, playlist) can't be streamed outside the
;; Spotify app, so they open a match screen: each song is looked up on
;; YouTube and SoundCloud, and you preview, accept or decline candidates.
;;
;; Everything lives on one page, the Music tab: what's playing with a
;; progress bar and volume, the queue, and saved playlists.  Links
;; (YouTube, SoundCloud, Bandcamp, ...) play through mpv + yt-dlp, and the
;; online search finds tracks on YouTube.  Desktop media keys and the KDE
;; media widget control it through MPRIS, the same way they controlled Elisa.
;;
;; Keys anywhere (normal state)
;; ---------------------------------------------------------------------
;;   SPC m m  open the Music tab          SPC m SPC  play / pause
;;   SPC m f  find a track and play it    SPC m n / SPC m p  next / previous
;;   SPC m a  add a track to the queue    SPC m +  /  SPC m -  volume
;;   SPC m r  shuffle the whole library   SPC m u  play a link
;;   SPC m o  search online               SPC m R  rescan the library
;;   SPC m l  the library                 SPC m A  pick an artist
;;   SPC m L  show / hide lyrics           SPC m Y  download lyrics for everything
;;   SPC m e  edit lyrics (and stamp them) SPC m t  edit tags
;;
;; Keys on the Music page
;; ---------------------------------------------------------------------
;;   RET  play the track / load the playlist at point
;;   p    play / pause           ] [  next / previous track
;;   > <  seek 10s               + - =  volume
;;   /    find & play now        a    find & add to the queue
;;   u    play a link            o    search online
;;   d    remove from queue      J K  move track down / up
;;   c    clear the queue        w    save the queue as a playlist
;;   s    shuffle the queue      S    shuffle mode    r  repeat mode
;;   l    library (search, mark, build playlists)
;;   A    pick an artist         e    edit the playlist at point
;;   b    browse by artist       R    rescan         q  close
;;   L    lyrics panel (RET on a line jumps there, r looks again,
;;        P pops it out into its own window / docks it back, t pins it on top,
;;        + - = text size, e edit the lyrics)
;;   E    edit lyrics of the track at point   T  edit its tags
;;
;; Lyrics editor: type or paste lines, ,0 plays the song from the start,
;; then RET as each line begins stamps it. ,j replays from a line, ,- ,=
;; nudge a stamp by 0.1s, ,u removes one, ZZ saves into the song
;;
;; Keys on the match screen (Spotify links)
;; ---------------------------------------------------------------------
;;   p    preview a candidate    P    hear Spotify's own 30s preview
;;   RET  queue it               D    download it as an mp3 into the library
;;   x    decline it             f    find more     e  change the search
;;   s    skip the song          ] [  next / previous song   q  close
;;
;; Keys in the library and playlist views
;; ---------------------------------------------------------------------
;;   RET  play (in a playlist: play it from here)   a  add to the queue
;;   /    filter by words        A    one artist     C  clear filters
;;   m u  mark / unmark          M U  mark / unmark everything shown
;;   s    add marked (or this) to a playlist, new or existing
;;   p P  play / shuffle-play everything shown (or marked)
;;   d    remove from playlist   J K  move down / up   q  back

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'color)
(require 'xml)
(require 'url-util)
(require 'json)

;; emms variables this file let-binds before emms is loaded; declaring them
;; keeps those bindings dynamic
(defvar emms-track-initialize-functions)
(defvar emms-source-playlist-ask-before-overwrite)

(defvar my/music-directory "/run/media/lim3r/pub/Music/"
  "Root of the music collection.")

(defvar my/music-device "/dev/disk/by-label/pub"
  "Block device holding `my/music-directory', mounted on demand.")

(defvar my/music-extensions '("mp3")
  "File extensions treated as music.  The collection is kept mp3-only,
and links saved with D are converted to mp3 too.")

(defvar my/music-state-directory (locate-user-emacs-file "music/")
  "Where playlists and player state (volume) are kept.")

(defvar my/music-volume-step 5
  "How much + and - change the volume.")

;;; drive
;; ---------------------------------------------------------------------

(defun my/music-ensure-mounted ()
  "Load emms and bring up the music drive if `my/music-directory' isn't there.
Mounts it when plugged in, otherwise asks for it and waits (see my-drives)."
  (require 'emms)
  (unless (file-directory-p my/music-directory)
    (if (fboundp 'my/drive-ensure-path)
        (my/drive-ensure-path my/music-directory "Music")
      ;; may pop up a polkit password prompt, that's fine
      (call-process "udisksctl" nil nil nil "mount" "-b" my/music-device))
    (unless (file-directory-p my/music-directory)
      (user-error "Music drive isn't there (plug it in and try again)"))))

;;; track info: tags first, then ffprobe, then the folder layout
;; ---------------------------------------------------------------------

(defun my/music--ffprobe (track file)
  "Fill TRACK's missing tags from ffprobe's view of FILE."
  (with-temp-buffer
    (when (and (executable-find "ffprobe")
               (zerop (call-process "ffprobe" nil t nil "-v" "quiet"
                                    "-print_format" "json" "-show_format" file)))
      (goto-char (point-min))
      (let* ((format (alist-get 'format (ignore-errors
                                          (json-parse-buffer :object-type 'alist))))
             (tags (alist-get 'tags format))
             (tag (lambda (&rest names)
                    ;; tag names vary in case between formats
                    (seq-some (lambda (name)
                                (cdr (seq-find (lambda (kv)
                                                 (string-equal-ignore-case
                                                  (symbol-name (car kv)) name))
                                               tags)))
                              names))))
        (pcase-dolist (`(,field . ,names)
                       '((info-title "title") (info-artist "artist")
                         (info-album "album") (info-albumartist "album_artist")
                         (info-date "date" "year") (info-genre "genre")
                         (info-tracknumber "track")))
          (unless (emms-track-get track field)
            (when-let ((value (apply tag names)))
              (emms-track-set track field value))))
        (when-let ((duration (alist-get 'duration format)))
          (unless (emms-track-get track 'info-playing-time)
            (emms-track-set track 'info-playing-time
                            (round (string-to-number duration)))))))))

(defun my/music--from-path (track file)
  "Fill whatever TRACK still lacks from FILE's place in the collection."
  (let* ((rel (file-relative-name file my/music-directory))
         (parts (split-string rel "/" t))
         (base (file-name-sans-extension (car (last parts))))
         ;; "01 - Title", "01. Title", "01 Title"
         (base (replace-regexp-in-string "\\`[0-9]+\\s-*[-._)]?\\s-*" "" base))
         artist album title)
    (pcase (length parts)
      (1 (if (string-match "\\`\\(.+?\\) - \\(.+\\)\\'" base)
             (setq artist (match-string 1 base) title (match-string 2 base))
           (setq title base)))
      (2 (setq artist (nth 0 parts) title base))
      (_ (setq artist (nth 0 parts) album (nth (- (length parts) 2) parts) title base)))
    (unless (emms-track-get track 'info-title)
      (emms-track-set track 'info-title title))
    (when (and artist (not (emms-track-get track 'info-artist)))
      (emms-track-set track 'info-artist artist))
    (when (and album (not (emms-track-get track 'info-album)))
      (emms-track-set track 'info-album album))))

(defun my/music-info (track)
  "Fill TRACK's info from its tags, ffprobe and the folder layout."
  (when (eq (emms-track-type track) 'file)
    (let ((file (emms-track-name track)))
      (ignore-errors (emms-info-native track))
      (unless (and (emms-track-get track 'info-title)
                   (emms-track-get track 'info-artist))
        (ignore-errors (my/music--ffprobe track file)))
      (my/music--from-path track file))))

;;; library
;; ---------------------------------------------------------------------

(defun my/music--library-tracks ()
  "Every local track in the library."
  (let (tracks)
    (maphash (lambda (_name track)
               (when (and (eq (emms-track-type track) 'file)
                          (string-prefix-p (expand-file-name my/music-directory)
                                           (emms-track-name track)))
                 (push track tracks)))
             emms-cache-db)
    tracks))

(defun my/music--junk-p (file)
  "Non-nil for FILE that only looks like music.
That's macOS's ._ metadata files (left on exFAT drives) and other dotfiles."
  (string-prefix-p "." (file-name-nondirectory file)))

(defun my/music-scan ()
  "Scan the collection into the library.
New files are added with their tags read straight away, changed files are
re-read, and anything gone (or junk) is dropped from the cache."
  (interactive)
  (my/music-ensure-mounted)
  (let* ((root (expand-file-name my/music-directory))
         (files (seq-remove #'my/music--junk-p
                            (emms-source-file-directory-tree root (emms-source-file-regex))))
         (keep (make-hash-table :test #'equal))
         (progress (make-progress-reporter "Scanning music…" 0 (length files)))
         (count 0))
    (dolist (file files) (puthash file t keep))
    ;; drop junk and files that are gone
    (maphash (lambda (name track)
               (when (and (eq (emms-track-type track) 'file)
                          (string-prefix-p root (emms-track-name track))
                          (not (gethash (emms-track-name track) keep)))
                 (remhash name emms-cache-db)
                 (setq emms-cache-dirty t)))
             emms-cache-db)
    ;; read tags now rather than lazily, so the library is complete at once
    (let ((emms-track-initialize-functions nil))
      (dolist (file files)
        (let ((track (emms-track 'file file))
              (mtime (file-attribute-modification-time (file-attributes file))))
          (unless (and (emms-track-get track 'info-title)
                       (equal (emms-track-get track 'info-mtime) mtime))
            (my/music-info track)
            (emms-track-set track 'info-mtime mtime)))
        (progress-reporter-update progress (cl-incf count))))
    (progress-reporter-done progress)
    (emms-cache-save)
    (message "Music library: %d tracks" (hash-table-count keep))))

(defun my/music--ensure-library ()
  (my/music-ensure-mounted)
  ;; scan when empty, or when the cache holds junk / unread tracks
  (let ((tracks (my/music--library-tracks)))
    (when (or (null tracks)
              (seq-some (lambda (track)
                          (or (my/music--junk-p (emms-track-name track))
                              (not (emms-track-get track 'info-title))))
                        tracks))
      (my/music-scan))))

;;; tracks
;; ---------------------------------------------------------------------

(defun my/music--url-p (track)
  (eq (emms-track-type track) 'url))

(defun my/music--title (track)
  (or (emms-track-get track 'info-title)
      (if (my/music--url-p track)
          (emms-track-name track)
        (file-name-base (emms-track-name track)))))

(defun my/music--description (track)
  "Artist — Title  Album, for pickers and the queue."
  (let ((artist (emms-track-get track 'info-artist))
        (album (emms-track-get track 'info-album)))
    (concat (if artist (concat artist " — ") "")
            (my/music--title track)
            (cond (album (propertize (concat "  " album) 'face 'shadow))
                  ((my/music--url-p track) (propertize "  link" 'face 'shadow))
                  (t "")))))

(defun my/music--time (seconds)
  (if (and seconds (> seconds 0))
      (let ((s (round seconds)))
        (if (>= s 3600)
            (format "%d:%02d:%02d" (/ s 3600) (/ (% s 3600) 60) (% s 60))
          (format "%d:%02d" (/ s 60) (% s 60))))
    "-:--"))

(defun my/music--read-track (prompt)
  "Pick a track from the library with PROMPT."
  (my/music--ensure-library)
  (let* ((tracks (sort (my/music--library-tracks)
                       (lambda (a b) (string-lessp (my/music--description a)
                                                   (my/music--description b)))))
         (table (mapcar (lambda (track) (cons (my/music--description track) track)) tracks)))
    (cdr (assoc (completing-read prompt table nil t) table))))

;;; queue
;; ---------------------------------------------------------------------

(defun my/music--queue-buffer ()
  "The current emms playlist (the queue), created if needed."
  (with-current-emms-playlist
    (unless (derived-mode-p 'emms-playlist-mode) (emms-playlist-mode))
    (current-buffer)))

(defun my/music--queue-items ()
  "The queue as a list of (POS TRACK SELECTED)."
  (with-current-buffer (my/music--queue-buffer)
    (let ((selected (and emms-playlist-selected-marker
                         (eq (marker-buffer emms-playlist-selected-marker) (current-buffer))
                         (marker-position emms-playlist-selected-marker)))
          items)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((track (emms-playlist-track-at (point))))
            (push (list (point) track (eql (point) selected)) items))
          (forward-line 1)))
      (nreverse items))))

(defun my/music--play-at (pos)
  "Play the queue entry starting at POS."
  (when emms-player-playing-p (emms-stop))
  (with-current-buffer (my/music--queue-buffer)
    (emms-playlist-select pos))
  (emms-start))

(defun my/music--append (track &optional play)
  "Add TRACK to the end of the queue, and play it if PLAY."
  (with-current-buffer (my/music--queue-buffer)
    (let ((inhibit-read-only t)
          (pos (point-max)))
      (save-excursion
        (goto-char pos)
        (emms-playlist-insert-track track))
      (when play (my/music--play-at pos))))
  (my/music-page-refresh))

(defun my/music--queue (tracks &optional shuffle)
  "Replace the queue with TRACKS and start playing."
  (emms-stop)
  (with-current-emms-playlist
    (emms-playlist-clear)
    (dolist (track tracks) (emms-playlist-insert-track track))
    (when shuffle (emms-shuffle))
    (emms-playlist-select-first))
  (emms-start)
  (my/music-page-refresh))

;;; playback
;; ---------------------------------------------------------------------

(defun my/music-toggle ()
  "Play or pause."
  (interactive)
  (require 'emms)
  (cond (emms-player-playing-p (emms-pause))
        ((my/music--queue-items)
         (with-current-emms-playlist
           (unless (emms-playlist-selected-track) (emms-playlist-select-first)))
         (emms-start))
        (t (user-error "The queue is empty (SPC m f to find a track)"))))

(defun my/music-next ()
  (interactive)
  (emms-next))

(defun my/music-previous ()
  (interactive)
  (emms-previous))

(defun my/music-seek-forward ()
  (interactive)
  (emms-seek 10))

(defun my/music-seek-backward ()
  (interactive)
  (emms-seek -10))

(defun my/music-find ()
  "Find a track in the library and play it now."
  (interactive)
  (my/music--append (my/music--read-track "Play: ") t))

(defun my/music-add ()
  "Find a track in the library and add it to the queue."
  (interactive)
  (let ((track (my/music--read-track "Add to queue: ")))
    (my/music--append track)
    (message "Queued %s" (my/music--description track))))

(defun my/music-shuffle-all ()
  "Shuffle the whole library into the queue and play."
  (interactive)
  (my/music--ensure-library)
  (my/music--queue (my/music--library-tracks) t))

(defun my/music-shuffle-queue ()
  "Shuffle the queue once."
  (interactive)
  (with-current-emms-playlist (emms-shuffle))
  (my/music-page-refresh))

(defun my/music-toggle-shuffle ()
  "Toggle shuffle mode (play the queue in random order)."
  (interactive)
  (emms-toggle-random-playlist)
  (my/music-page-refresh))

(defun my/music-cycle-repeat ()
  "Cycle repeat: all -> one -> off."
  (interactive)
  (cond ((bound-and-true-p emms-repeat-track)
         (setq emms-repeat-track nil emms-repeat-playlist nil))
        (emms-repeat-playlist
         (setq emms-repeat-track t))
        (t (setq emms-repeat-playlist t)))
  (my/music-page-refresh))

(defun my/music-now-playing ()
  "Describe the current track, or nil when nothing is loaded."
  (when (and (boundp 'emms-player-playing-p) emms-player-playing-p)
    (when-let ((track (emms-playlist-current-selected-track)))
      (let ((artist (emms-track-get track 'info-artist)))
        (concat (if artist (concat artist " — ") "") (my/music--title track))))))

;;; volume
;; ---------------------------------------------------------------------

(defvar my/music-volume 80
  "Player volume in percent (mpv allows up to 130).")

(defun my/music--state-file ()
  (expand-file-name "state.eld" my/music-state-directory))

(defun my/music--load-state ()
  (let ((file (my/music--state-file)))
    (when (file-exists-p file)
      (let ((state (ignore-errors
                     (with-temp-buffer (insert-file-contents file) (read (current-buffer))))))
        (when (numberp (plist-get state :volume))
          (setq my/music-volume (plist-get state :volume)))
        (when (memq (plist-get state :lyrics-prefer) '(ask local online))
          (setq my/music-lyrics-prefer (plist-get state :lyrics-prefer)))))))

(defun my/music--save-state ()
  (make-directory my/music-state-directory t)
  (with-temp-file (my/music--state-file)
    (prin1 (list :volume my/music-volume :lyrics-prefer my/music-lyrics-prefer)
           (current-buffer))))

(defun my/music--apply-volume (&rest _)
  (when (and (bound-and-true-p emms-player-playing-p) (fboundp 'emms-player-mpv-cmd))
    (emms-player-mpv-cmd `(set_property volume ,my/music-volume))))

(defun my/music-set-volume (volume)
  "Set the volume to VOLUME percent."
  (interactive (list (read-number "Volume (0-130): " my/music-volume)))
  (setq my/music-volume (max 0 (min 130 (round volume))))
  (my/music--apply-volume)
  (my/music--save-state)
  (message "Volume %d%%" my/music-volume)
  (my/music-page-refresh))

(defun my/music-volume-up ()
  (interactive)
  (my/music-set-volume (+ my/music-volume my/music-volume-step)))

(defun my/music-volume-down ()
  (interactive)
  (my/music-set-volume (- my/music-volume my/music-volume-step)))

;;; playlists
;; ---------------------------------------------------------------------

(defun my/music--playlists-dir ()
  (let ((dir (expand-file-name "playlists/" my/music-state-directory)))
    (make-directory dir t)
    dir))

(defun my/music--playlists ()
  (directory-files (my/music--playlists-dir) t "\\.m3u\\'"))

(defun my/music--playlist-size (file)
  (with-temp-buffer
    (insert-file-contents file)
    (count-matches "^[^#\n]" (point-min) (point-max))))

(defun my/music-save-playlist (name)
  "Save the queue as playlist NAME."
  (interactive (list (read-string "Save queue as playlist: ")))
  (when (string-empty-p (string-trim name))
    (user-error "A playlist needs a name"))
  (unless (my/music--queue-items)
    (user-error "The queue is empty"))
  (let ((file (expand-file-name (concat (string-trim name) ".m3u") (my/music--playlists-dir))))
    (when (and (file-exists-p file)
               (not (y-or-n-p (format "Playlist %s exists, replace it? " name))))
      (user-error "Not saved"))
    (with-current-buffer (my/music--queue-buffer)
      (let ((emms-source-playlist-ask-before-overwrite nil))
        (emms-playlist-save 'm3u file)))
    (message "Saved playlist %s" name)
    (my/music-page-refresh)))

(defun my/music-load-playlist (file)
  "Replace the queue with playlist FILE and play it."
  (interactive (list (read-file-name "Playlist: " (my/music--playlists-dir) nil t)))
  ;; local tracks need the drive; links work without it
  (ignore-errors (my/music-ensure-mounted))
  (emms-stop)
  (with-current-emms-playlist (emms-playlist-clear))
  (emms-add-m3u-playlist file)
  (dolist (item (my/music--queue-items))
    (when (and (my/music--url-p (nth 1 item))
               (not (emms-track-get (nth 1 item) 'info-title)))
      (my/music--fetch-link-info (nth 1 item))))
  (with-current-emms-playlist (emms-playlist-select-first))
  (emms-start)
  (my/music-page-refresh))

(defun my/music-delete-playlist (file)
  (when (y-or-n-p (format "Delete playlist %s? " (file-name-base file)))
    (delete-file file)
    (my/music-page-refresh)))

;;; links and online search (yt-dlp)
;; ---------------------------------------------------------------------

(defun my/music--yt-dlp ()
  (or (executable-find "yt-dlp")
      (user-error "yt-dlp isn't installed (sudo dnf install yt-dlp)")))

(defun my/music--fetch-link-info (track)
  "Fill in title, uploader and length of link TRACK in the background."
  (when (and (executable-find "yt-dlp")
             (not (emms-track-get track 'my-fetching)))
    (emms-track-set track 'my-fetching t)
    (let ((out (generate-new-buffer " *yt-dlp*"))
          (err (generate-new-buffer " *yt-dlp-err*")))
      (make-process
       :name "yt-dlp-info" :buffer out :stderr err :noquery t
       :command (list "yt-dlp" "--no-playlist" "--no-warnings" "--skip-download"
                      "--print" "%(title)s\t%(uploader)s\t%(duration)s"
                      (emms-track-name track))
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (when (zerop (process-exit-status proc))
             (let ((parts (split-string (string-trim (with-current-buffer out (buffer-string)))
                                        "\t")))
               (when (and (car parts) (not (string-empty-p (car parts))))
                 (emms-track-set track 'info-title (nth 0 parts))
                 (when (and (nth 1 parts) (not (member (nth 1 parts) '("NA" ""))))
                   (emms-track-set track 'info-artist (nth 1 parts)))
                 (let ((seconds (string-to-number (or (nth 2 parts) ""))))
                   (when (> seconds 0)
                     (emms-track-set track 'info-playing-time (round seconds)))))))
           (kill-buffer out)
           (when (buffer-live-p err) (kill-buffer err))
           (my/music-page-refresh)))))))

(defun my/music--clipboard-link ()
  (let ((text (ignore-errors (current-kill 0 t))))
    (and (stringp text)
         (string-match-p "\\`\\(?:https?://\\|spotify:\\)" (string-trim text))
         (string-trim text))))

(defun my/music-play-link (url)
  "Stream URL (YouTube, SoundCloud, Bandcamp, ...) now.
Spotify links open the match screen instead."
  (interactive (list (read-string "Play link: " (my/music--clipboard-link))))
  (require 'emms)
  (my/music--yt-dlp)
  (setq url (string-trim url))
  (unless (string-match-p "\\`\\(?:https?://\\|spotify:\\)" url)
    (user-error "That doesn't look like a link"))
  (if (my/music--spotify-link-p url)
      (my/music-match-open url)
    (my/music--play-plain-link url)))

(defun my/music--play-plain-link (url)
  (let ((track (emms-track 'url url)))
    (my/music--fetch-link-info track)
    (my/music--append track t)))

(defun my/music-search-online (query)
  "Search YouTube for QUERY and play the pick."
  (interactive (list (read-string "Search online: ")))
  (require 'emms)
  (let ((yt-dlp (my/music--yt-dlp)))
    (message "Searching for %s…" query)
    (let* ((lines (apply #'process-lines-ignore-status yt-dlp
                         (list "--flat-playlist" "--no-warnings"
                               "--print" "%(id)s\t%(title)s\t%(channel)s\t%(duration)s"
                               (format "ytsearch10:%s" query))))
           (results (seq-filter (lambda (parts) (= (length parts) 4))
                                (mapcar (lambda (line) (split-string line "\t")) lines)))
           (table (mapcar (lambda (parts)
                            (cons (concat (nth 1 parts) "  "
                                          (propertize (nth 2 parts) 'face 'shadow) "  "
                                          (propertize (my/music--time
                                                       (string-to-number (nth 3 parts)))
                                                      'face 'shadow))
                                  parts))
                          results)))
      (unless table (user-error "Nothing found for %s" query))
      (let* ((parts (cdr (assoc (completing-read "Play: " table nil t) table)))
             (track (emms-track 'url (concat "https://www.youtube.com/watch?v=" (nth 0 parts))))
             (seconds (string-to-number (nth 3 parts))))
        (emms-track-set track 'info-title (nth 1 parts))
        (emms-track-set track 'info-artist (nth 2 parts))
        (when (> seconds 0) (emms-track-set track 'info-playing-time seconds))
        (my/music--append track t)))))

(defun my/music--safe-name (name)
  "NAME made safe as a file name (and as a yt-dlp output template)."
  (replace-regexp-in-string
   "%" "%%" (replace-regexp-in-string "[/\\\\:*?\"<>|]" "_" (string-trim name))))

(defun my/music--retag (file title artist album)
  "Rewrite FILE's title / artist / album tags, keeping audio and cover as they are."
  (let ((tmp (concat (file-name-sans-extension file) ".retag.mp3")))
    (when (zerop (apply #'call-process "ffmpeg" nil nil nil
                        "-v" "quiet" "-y" "-i" file "-map" "0" "-c" "copy" "-id3v2_version" "3"
                        (append (list "-metadata" (concat "title=" title)
                                      "-metadata" (concat "artist=" artist))
                                (when album (list "-metadata" (concat "album=" album)))
                                (list tmp))))
      (rename-file tmp file t))))

(defun my/music-download (track)
  "Download link TRACK into the music folder as a tagged mp3.
When TRACK already carries a title and artist (Spotify matches do), the
file is named and tagged with those instead of the uploader's."
  (my/music-ensure-mounted)
  (my/music--yt-dlp)
  (let* ((dir (expand-file-name my/music-directory))
         (title (my/music--title track))
         (artist (emms-track-get track 'info-artist))
         (album (emms-track-get track 'info-album))
         (known (and artist (emms-track-get track 'info-title)))
         (log (generate-new-buffer " *yt-dlp-download*")))
    (message "Downloading %s…" title)
    (make-process
     :name "yt-dlp-download" :buffer log :noquery t
     :command (list "yt-dlp" "--no-playlist" "--no-warnings"
                    "-x" "--audio-format" "mp3" "--audio-quality" "0"
                    "--embed-metadata" "--embed-thumbnail"
                    "--parse-metadata" "uploader:%(artist)s"
                    "-o" (expand-file-name (if known
                                               (concat (my/music--safe-name title) ".%(ext)s")
                                             "%(title)s.%(ext)s")
                                           dir)
                    "--print" "after_move:filepath"
                    (emms-track-name track))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((file (with-current-buffer log
                       (goto-char (point-max))
                       (and (re-search-backward "^/.*\\.mp3$" nil t)
                            (match-string 0)))))
           (if (and (zerop (process-exit-status proc)) file (file-exists-p file))
               (progn
                 (when known (my/music--retag file title artist album))
                 (let ((new (emms-track 'file file)))
                   ;; re-read the tags we just wrote
                   (emms-track-set new 'info-title nil)
                   (emms-track-set new 'info-artist nil)
                   (my/music-info new)
                   (emms-cache-save))
                 (message "Saved %s to your library" (file-name-nondirectory file)))
             (message "Download failed for %s (see buffer %s)" title (buffer-name log))))
         (when (zerop (process-exit-status proc)) (kill-buffer log))
         (my/music-page-refresh))))))

;;; covers
;; ---------------------------------------------------------------------
;; pulled out of the mp3 (or a youtube thumbnail for links) by ffmpeg in
;; the background, cropped square and cached under music/covers/

(defvar my/music-cover-size 112
  "Pixel size of the cover on the music page.")

(defvar my/music--cover-jobs (make-hash-table :test #'equal)
  "Cover files being extracted right now.")

(defvar my/music--cover-data (make-hash-table :test #'equal)
  "Cover file -> its base64 contents, so redraws don't reread it.")

(defun my/music--cover-source (track)
  "Where TRACK's cover comes from: its file, a youtube thumbnail or nil."
  (let ((name (emms-track-name track)))
    (if (my/music--url-p track)
        (when (string-match "\\(?:v=\\|youtu\\.be/\\)\\([A-Za-z0-9_-]\\{11\\}\\)" name)
          (format "https://i.ytimg.com/vi/%s/mqdefault.jpg" (match-string 1 name)))
      name)))

(defun my/music--cover (track)
  "Path of TRACK's cached cover, or nil (and start fetching it if needed)."
  (when-let ((source (and track (my/music--cover-source track))))
    (let* ((dir (expand-file-name "covers/" my/music-state-directory))
           (file (expand-file-name (concat (md5 source) ".jpg") dir))
           (none (concat file ".none")))
      (cond ((file-exists-p file) file)
            ((or (file-exists-p none) (gethash file my/music--cover-jobs)) nil)
            ((executable-find "ffmpeg")
             (make-directory dir t)
             (puthash file t my/music--cover-jobs)
             (make-process
              :name "music-cover" :noquery t :buffer nil
              :command (list "ffmpeg" "-v" "quiet" "-y" "-i" source "-map" "0:v:0"
                             "-frames:v" "1"
                             "-vf" "crop='min(iw,ih)':'min(iw,ih)',scale=256:256"
                             file)
              :sentinel (lambda (proc _)
                          (unless (process-live-p proc)
                            (remhash file my/music--cover-jobs)
                            (if (and (file-exists-p file)
                                     (> (file-attribute-size (file-attributes file)) 0))
                                (my/music-page-refresh)
                              ;; no art: remember that, but let links try again later
                              (ignore-errors (delete-file file))
                              (unless (string-prefix-p "http" source)
                                (write-region "" nil none nil 'quiet))))))
             nil)))))

(defun my/music--cover-base64 (file)
  (or (gethash file my/music--cover-data)
      (puthash file
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (base64-encode-region (point-min) (point-max) t)
                 (buffer-string))
               my/music--cover-data)))

(defun my/music--color (face attr fallback)
  "FACE's ATTR as #rrggbb, or FALLBACK."
  (let* ((c (face-attribute face attr nil t))
         (rgb (and (stringp c) (color-name-to-rgb c))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) fallback)))

(defun my/music--blend (a b amount)
  "Colour AMOUNT of the way from A to B (both #rrggbb)."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (x y) (+ x (* amount (- y x))))
                            (color-name-to-rgb a) (color-name-to-rgb b))
                 '(2))))

(defun my/music--fit (string px width)
  "STRING cut down to fit WIDTH pixels at font size PX."
  (let ((chars (max 4 (floor (/ width (* 0.6 px))))))
    (truncate-string-to-width string chars nil nil "…")))

(defun my/music-page--card (track)
  "The now-playing card: cover on the left, title and details beside it."
  (let* ((size my/music-cover-size)
         (win (get-buffer-window (current-buffer) t))
         (width (min 760 (- (if win (window-body-width win t) 800)
                            (* 4 (frame-char-width)))))
         (px (or (ignore-errors (aref (font-info (face-font 'default)) 2))
                 (frame-char-height)))
         (family (xml-escape-string
                  (let ((f (face-attribute 'default :family nil t)))
                    (if (stringp f) f "monospace"))))
         (fg (my/music--color 'default :foreground "#dfe1e5"))
         (bg (my/music--color 'default :background "#1e1f22"))
         (dim (my/music--color 'shadow :foreground "#9da0a8"))
         (tile (my/music--blend bg fg 0.07))
         (cover (my/music--cover track))
         (text-x (+ size 20))
         (text-w (- width text-x))
         (title (if track (my/music--title track) "Nothing playing"))
         (meta (if track
                   (string-join (delq nil (list (emms-track-get track 'info-artist)
                                                (emms-track-get track 'info-album)
                                                (emms-track-get track 'info-year)
                                                (and (my/music--url-p track) "link")))
                                "  ·  ")
                 "/ find a track   o search online   u play a link"))
         (title-px (* px 1.5))
         (svg
          (concat
           (format "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='%d' height='%d'>"
                   width size)
           (format "<clipPath id='c'><rect width='%d' height='%d' rx='8'/></clipPath>" size size)
           (format "<rect width='%d' height='%d' rx='8' fill='%s'/>" size size tile)
           (if cover
               (format "<image width='%d' height='%d' clip-path='url(#c)' preserveAspectRatio='xMidYMid slice' xlink:href='data:image/jpeg;base64,%s'/>"
                       size size (my/music--cover-base64 cover))
             (format "<text x='%d' y='%d' text-anchor='middle' font-family='%s' font-size='%dpx' fill='%s'>♪</text>"
                     (/ size 2) (+ (/ size 2) (round (* size 0.13))) family (round (* size 0.38)) dim))
           (format "<text x='%d' y='%.1f' font-family='%s' font-size='%.1fpx' font-weight='bold' fill='%s'>%s</text>"
                   text-x (- (/ size 2.0) (* px 0.4)) family title-px fg
                   (xml-escape-string (my/music--fit title title-px text-w)))
           (format "<text x='%d' y='%.1f' xml:space='preserve' font-family='%s' font-size='%.1fpx' fill='%s'>%s</text>"
                   text-x (+ (/ size 2.0) (* px 1.3)) family (float px) dim
                   (xml-escape-string (my/music--fit meta px text-w)))
           "</svg>")))
    (propertize " " 'display (create-image svg 'svg t :ascent 'center :scale 1))))

;;; the music page
;; ---------------------------------------------------------------------

(defvar my/music-page-buffer "*Music*")

(defun my/music-page--heading (title &optional count)
  (concat "\n  "
          (propertize title 'face '(:inherit font-lock-function-name-face :weight bold))
          (if count (propertize (format "  %d" count) 'face 'shadow) "")
          "\n"))

(defun my/music-page--bar (fraction width face)
  (let ((filled (max 0 (min width (round (* width fraction))))))
    (concat (propertize (make-string filled ?━) 'face face)
            (propertize (make-string (- width filled) ?─) 'face 'shadow))))

(defun my/music-page--progress ()
  "The progress line for the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (total (and track (emms-track-get track 'info-playing-time)))
         (elapsed (if (boundp 'emms-playing-time) emms-playing-time 0)))
    (propertize
     (concat "  "
             (my/music-page--bar (if (and total (> total 0)) (/ (float elapsed) total) 0)
                                 48 'success)
             "  "
             (propertize (format "%s / %s" (my/music--time elapsed) (my/music--time total))
                         'face 'shadow)
             "\n")
     'my/music-progress t)))

(defun my/music-page--status ()
  (concat "  "
          (cond ((not emms-player-playing-p) (propertize "■ stopped" 'face 'shadow))
                (emms-player-paused-p (propertize "⏸ paused" 'face 'warning))
                (t (propertize "▶ playing" 'face 'success)))
          "     "
          (propertize "vol " 'face 'shadow)
          (my/music-page--bar (/ my/music-volume 100.0) 10 'default)
          (propertize (format " %d%%" my/music-volume) 'face 'shadow)
          "     "
          (propertize (if emms-random-playlist "shuffle on" "shuffle off")
                      'face (if emms-random-playlist 'default 'shadow))
          "     "
          (propertize (cond ((bound-and-true-p emms-repeat-track) "repeat one")
                            (emms-repeat-playlist "repeat all")
                            (t "repeat off"))
                      'face (if emms-repeat-playlist 'default 'shadow))
          "\n"))

(defun my/music-page--render ()
  "Redraw the music page, keeping point on the same item."
  (let* ((inhibit-read-only t)
         (item (get-text-property (point) 'my/music-item))
         (line (line-number-at-pos))
         (starts (mapcar (lambda (w) (cons w (line-number-at-pos (window-start w))))
                         (get-buffer-window-list (current-buffer) nil t)))
         (track (and emms-player-playing-p (emms-playlist-current-selected-track)))
         (queue (my/music--queue-items))
         (win (get-buffer-window (current-buffer) t))
         ;; queue rows shrink to fit when the lyrics panel takes some room
         (desc-width (max 20 (min 72 (- (if win (window-body-width win) 100) 18))))
         (playlists (my/music--playlists)))
    (erase-buffer)
    ;; now playing
    (insert "\n  " (propertize (my/music-page--card track) 'my/music-item '(now)) "\n\n")
    (insert (if track (my/music-page--progress) "\n"))
    (insert (my/music-page--status))
    ;; queue
    (insert (my/music-page--heading "Queue" (length queue)))
    (if (null queue)
        (insert (propertize "    empty — / plays a track, a adds one\n" 'face 'shadow))
      (let ((n 0))
        (dolist (entry queue)
          (pcase-let ((`(,pos ,qtrack ,selected) entry))
            (let ((row (concat (if selected (propertize "  ▶ " 'face 'success) "    ")
                               (propertize (format "%3d  " (cl-incf n)) 'face 'shadow)
                               (truncate-string-to-width (my/music--description qtrack)
                                                         desc-width nil ?\s "…")
                               "  "
                               (propertize (my/music--time
                                            (emms-track-get qtrack 'info-playing-time))
                                           'face 'shadow)
                               "\n")))
              (when selected (add-face-text-property 0 (length row) 'bold t row))
              (insert (propertize row 'my/music-item (list 'queue pos qtrack))))))))
    ;; playlists
    (insert (my/music-page--heading "Playlists" (length playlists)))
    (if (null playlists)
        (insert (propertize "    none yet — w saves the queue as one\n" 'face 'shadow))
      (dolist (file playlists)
        (insert (propertize (concat "    ≡ "
                                    (string-pad (file-name-base file) 30)
                                    (propertize (my/music--plural (my/music--playlist-size file) "track")
                                                'face 'shadow)
                                    "\n")
                            'my/music-item (list 'playlist file)))))
    ;; put point back on the same item
    (goto-char (point-min))
    (let ((match (and item
                      (text-property-search-forward
                       'my/music-item item
                       (lambda (want have)
                         (and (eq (car want) (car-safe have))
                              (if (eq (car want) 'queue)
                                  (eq (nth 2 want) (nth 2 have))
                                (equal (nth 1 want) (nth 1 have)))))))))
      (if match
          (goto-char (prop-match-beginning match))
        (goto-char (point-min))
        (forward-line (1- line))))
    (dolist (start starts)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (cdr start)))
        (set-window-start (car start) (point) t)))
    (dolist (w (get-buffer-window-list (current-buffer) nil t))
      (set-window-point w (point)))))

(defun my/music-page-refresh (&rest _)
  "Redraw the music page if it exists."
  (when-let ((buf (get-buffer my/music-page-buffer)))
    (with-current-buffer buf
      (my/music-page--render))))

(defun my/music-page--tick ()
  "Update just the progress line, once a second while something plays."
  (when-let ((buf (get-buffer my/music-page-buffer)))
    (when (and emms-player-playing-p (not emms-player-paused-p)
               (get-buffer-window buf t))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (when-let ((match (text-property-search-forward 'my/music-progress t t)))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              (goto-char (prop-match-beginning match))
              (insert (my/music-page--progress)))))))))

(defvar my/music--timer nil)

(defun my/music-page--buffer ()
  (let ((buf (get-buffer-create my/music-page-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/music-mode) (my/music-mode))
      ;; its own folder, not whichever project's tab made it
      (setq default-directory (if (file-directory-p my/music-directory)
                                  my/music-directory
                                (expand-file-name "~/")))
      (my/music-page--render))
    buf))

(defun my/music ()
  "Open the Music tab."
  (interactive)
  (my/music--ensure-library)
  (let ((index (seq-position (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs))
                             "Music")))
    (if index
        (tab-bar-select-tab (1+ index))
      (tab-new)
      (tab-bar-rename-tab "Music"))
    (delete-other-windows)
    (switch-to-buffer (my/music-page--buffer))))

(defun my/music-page--item ()
  (or (get-text-property (point) 'my/music-item)
      (user-error "Nothing to act on here")))

(defun my/music-page-open ()
  "Play the queue entry, or load the playlist, at point."
  (interactive)
  (pcase (my/music-page--item)
    (`(queue ,pos ,_) (my/music--play-at pos))
    (`(playlist ,file) (my/music-load-playlist file))))

(defun my/music-page-remove ()
  "Remove the queue entry at point (or delete the playlist at point)."
  (interactive)
  (pcase (my/music-page--item)
    (`(queue ,pos ,_)
     (with-current-buffer (my/music--queue-buffer)
       (let ((inhibit-read-only t))
         (save-excursion (goto-char pos) (emms-playlist-delete-track))))
     (my/music-page-refresh))
    (`(playlist ,file) (my/music-delete-playlist file))))

(defun my/music-page--shift (fn)
  (pcase (my/music-page--item)
    (`(queue ,pos ,_)
     (with-current-buffer (my/music--queue-buffer)
       (let ((inhibit-read-only t))
         (goto-char pos)
         (funcall fn)))
     (my/music-page-refresh))
    (_ (user-error "Only queue entries can be moved"))))

(defun my/music-page-move-down ()
  (interactive)
  (my/music-page--shift #'emms-playlist-mode-shift-track-down))

(defun my/music-page-move-up ()
  (interactive)
  (my/music-page--shift #'emms-playlist-mode-shift-track-up))

(defun my/music-page-download ()
  "Download the link at point into the music folder as an mp3."
  (interactive)
  (pcase (my/music-page--item)
    (`(queue ,_ ,track)
     (if (my/music--url-p track)
         (my/music-download track)
       (user-error "That's already in your library")))
    (_ (user-error "Put the cursor on a link in the queue"))))

(defun my/music-clear-queue ()
  (interactive)
  (when (y-or-n-p "Clear the queue? ")
    (emms-stop)
    (with-current-emms-playlist (emms-playlist-clear))
    (my/music-page-refresh)))

(defun my/music-browse ()
  "Browse the library by artist (q comes back here)."
  (interactive)
  (my/music--ensure-library)
  (emms-browser))

(defun my/music-back-to-page ()
  (interactive)
  (switch-to-buffer (my/music-page--buffer)))

(define-derived-mode my/music-mode special-mode "Music"
  "What's playing, the queue and playlists."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (my/music-page--render)))
  (setq header-line-format
        (concat (propertize " Music" 'face 'bold)
                (propertize "    RET play   p pause   ] [ skip   / find   l library   A artist   u link   o online   + - volume   L lyrics   ? more"
                            'face 'shadow)))
  (hl-line-mode 1)
  ;; drop an image here to make it the cover of the track at point
  (setq-local dnd-protocol-alist
              (append '(("^file:" . my/music-page--drop) ("^https?://" . my/music-page--drop))
                      dnd-protocol-alist)))

(defun my/music-help ()
  "Show every key on the music page."
  (interactive)
  (message (concat "RET play/load  p pause  ] [ next/prev  > < seek  + - = volume  "
                   "/ find&play  a add  u link  o online  D download link  "
                   "d remove  J K move  c clear  w save playlist  "
                   "s shuffle queue  S shuffle mode  r repeat  l library  A artist  "
                   "e edit playlist  n new playlist  right-click a row for its menu  b browse  R rescan  L lyrics  E edit lyrics  T tags  q close")))

;;; lyrics
;; ---------------------------------------------------------------------
;; a panel beside the music page (L shows / hides it). lyrics come from
;; lrclib.net (free, no account); synced ones light up line by line and
;; RET jumps the song to a line. when lrclib has nothing, the file's own
;; lyrics tag is used. results are cached under music/lyrics/

(defvar my/music-lyrics-buffer "*Lyrics*")

(defvar my/music-lyrics-width 0.4
  "Width of the lyrics panel, as a fraction of the frame.")

(defvar my/music-lyrics-agent "limacs-music (https://github.com/calizoots/limacs)"
  "User agent sent to lrclib, which asks clients to name themselves.")

(defvar my/music--lyrics (make-hash-table :test #'equal)
  "Track name -> lyrics plist, or `pending' while it's being fetched.
The plist has :source and one of :synced ((SECONDS . LINE) ...),
:plain TEXT, :instrumental t or :none t.")

(defvar my/music-lyrics--timer nil)

(defvar-local my/music-lyrics--track nil "Track the panel is showing.")
(defvar-local my/music-lyrics--line nil "Index of the highlighted line.")
(defvar-local my/music-lyrics--overlay nil)
(defvar-local my/music-lyrics--snap t
  "Non-nil to jump to the current line even if the panel has focus.")

(defun my/music--lyrics-file (name)
  (expand-file-name (concat (md5 name) ".eld")
                    (expand-file-name "lyrics/" my/music-state-directory)))

(defun my/music--parse-lrc (text)
  "Timed lines of LRC TEXT as ((SECONDS . LINE) ...), or nil if it has none."
  (let (lines)
    (dolist (line (split-string text "\n"))
      (when (string-match "\\`\\[\\([0-9]+\\):\\([0-9.]+\\)\\]\\s-*\\(.*\\)" line)
        (push (cons (+ (* 60 (string-to-number (match-string 1 line)))
                       (string-to-number (match-string 2 line)))
                    (string-trim (match-string 3 line)))
              lines)))
    (nreverse lines)))

(defun my/music--lyrics-from (hit source)
  "Lyrics plist from lrclib HIT (or a tag), or nil if it holds none."
  (let ((synced (plist-get hit :syncedLyrics))
        (plain (plist-get hit :plainLyrics)))
    (cond ((plist-get hit :instrumental) (list :source source :instrumental t))
          ((and (stringp synced) (my/music--parse-lrc synced))
           (list :source source :synced (my/music--parse-lrc synced)))
          ((and (stringp plain) (not (string-blank-p plain)))
           (list :source source :plain (string-trim plain))))))

(defun my/music--fetch-json (url callback)
  "Fetch URL with curl and call CALLBACK with the parsed JSON and success."
  (let ((buf (generate-new-buffer " *music-lyrics*")))
    (make-process
     :name "music-lyrics" :buffer buf :noquery t
     :command (list "curl" "-s" "-m" "15" "-A" my/music-lyrics-agent url)
     :sentinel (lambda (proc _)
                 (unless (process-live-p proc)
                   (let ((ok (zerop (process-exit-status proc)))
                         (data (with-current-buffer buf
                                 (ignore-errors
                                   (json-parse-string (buffer-string)
                                                      :object-type 'plist :array-type 'list
                                                      :null-object nil :false-object nil)))))
                     (kill-buffer buf)
                     (funcall callback data ok)))))))

(defun my/music--clean-title (title)
  "TITLE without (feat. …), [edit] and other bracketed extras."
  (string-trim (replace-regexp-in-string "\\s-*[([][^])]*[])]" "" title)))

(defun my/music--tag-lyrics (track)
  "Lyrics from TRACK's own file tags, or nil."
  (unless (my/music--url-p track)
    (with-temp-buffer
      (when (zerop (call-process "ffprobe" nil t nil "-v" "quiet" "-show_entries"
                                 "format_tags" "-of" "json" (emms-track-name track)))
        (goto-char (point-min))
        (let* ((json (ignore-errors (json-parse-buffer :object-type 'alist)))
               (tags (alist-get 'tags (alist-get 'format json)))
               (text (cdr (seq-find (lambda (kv) (string-prefix-p "lyrics" (downcase (symbol-name (car kv)))))
                                    tags))))
          (when (and (stringp text) (not (string-blank-p text)))
            (if (my/music--parse-lrc text)
                (list :source "the song file" :synced (my/music--parse-lrc text))
              (list :source "the song file" :plain (string-trim text)))))))))

(defvar my/music-lyrics-prefer 'ask
  "What `r' does when a song already has lyrics in its file and lrclib has
some too: `ask', keep the `local' ones, or take the `online' ones.")

(defvar my/music-lyrics-store t
  "Non-nil to write lyrics found online into the song's own file.")

(defun my/music--lyrics-done (name result)
  "Store RESULT as the lyrics for track NAME and redraw the panel.
Songs keep their lyrics in their own file; the small cache under
music/lyrics/ only remembers links and songs nothing was found for."
  (if (null result)
      (remhash name my/music--lyrics)
    (puthash name result my/music--lyrics)
    (when (and (not (plist-get result :offline))
               (or (string-prefix-p "http" name) (plist-get result :none)))
      (let ((file (my/music--lyrics-file name)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (prin1 result (current-buffer))))))
  (my/music-lyrics--render))

(defun my/music--norm (string)
  "STRING lowercased with only letters and digits, for loose comparing."
  (replace-regexp-in-string "[^[:alnum:]]+" "" (downcase (or string ""))))

(defun my/music--lrclib-match-p (hit artist title duration)
  "Non-nil when lrclib HIT really looks like this song.
Lyrics get written into files, so a doubtful match is worse than none:
the title has to match, and the artist too when we know it (otherwise
the length has to be within a few seconds)."
  (let* ((want (my/music--norm (my/music--clean-title title)))
         (got (my/music--norm (my/music--clean-title (plist-get hit :trackName))))
         (artists (seq-remove #'string-empty-p
                              (mapcar #'my/music--norm (split-string (or artist "") "\\s-*[,&]\\s-*"))))
         (hit-artist (my/music--norm (plist-get hit :artistName)))
         (length-ok (and duration (numberp (plist-get hit :duration))
                         (< (abs (- duration (plist-get hit :duration))) 4))))
    (and (>= (length want) 2) (>= (length got) 2)
         (or (string-search want got) (string-search got want))
         (if artists
             (seq-some (lambda (a) (or (string-search a hit-artist) (string-search hit-artist a)))
                       artists)
           length-ok))))

(defun my/music--lrclib-lookup (track callback)
  "Look TRACK up on lrclib; call CALLBACK with a lyrics plist (or nil) and
whether lrclib could be reached."
  (let* ((artist (car (split-string (or (emms-track-get track 'info-artist) "") "\\s-*[,&]\\s-*")))
         (title (my/music--title track))
         (duration (emms-track-get track 'info-playing-time))
         (search
          (lambda ()
            (my/music--fetch-json
             (concat "https://lrclib.net/api/search?q="
                     (url-hexify-string (string-trim (concat artist " " (my/music--clean-title title)))))
             (lambda (hits ok)
               (let* ((hits (and (listp hits) hits))
                      (scored (mapcar
                               (lambda (hit)
                                 (cons (+ (if (plist-get hit :syncedLyrics) 0 5)
                                          (if (and duration (numberp (plist-get hit :duration)))
                                              (min 30 (abs (- duration (plist-get hit :duration))))
                                            10))
                                       hit))
                               (seq-filter (lambda (hit)
                                             (and (my/music--lyrics-from hit "lrclib")
                                                  (my/music--lrclib-match-p
                                                   hit (emms-track-get track 'info-artist) title duration)))
                                           hits)))
                      (best (cdar (sort scored (lambda (a b) (< (car a) (car b)))))))
                 (funcall callback (and best (my/music--lyrics-from best "lrclib")) ok)))))))
    (if (string-empty-p artist)
        (funcall search)
      (my/music--fetch-json
       (concat "https://lrclib.net/api/get?artist_name=" (url-hexify-string artist)
               "&track_name=" (url-hexify-string title)
               (if duration (format "&duration=%d" duration) ""))
       (lambda (hit _ok)
         (let ((result (and (plist-get hit :id) (my/music--lyrics-from hit "lrclib"))))
           (if result (funcall callback result t) (funcall search))))))))

(defun my/music--store-lyrics (track lyrics &optional callback)
  "Write LYRICS (a plist with text) into TRACK's file, in the background."
  (if (or (my/music--url-p track) (not (my/music--lyrics-text-p lyrics)))
      (when callback (funcall callback nil))
    (my/music--write-tags (emms-track-name track)
                          (list (cons 'lyrics (my/music--lyrics-text lyrics)))
                          (or callback #'ignore))))

(defun my/music--lyrics-text-p (lyrics)
  (and (consp lyrics) (or (plist-get lyrics :synced) (plist-get lyrics :plain))))

(defun my/music--lyrics (track)
  "TRACK's lyrics plist, `pending' while fetching (starts the fetch).
Looks in the song's own file first, then online (and saves what it finds
into the file, see `my/music-lyrics-store')."
  (let* ((name (emms-track-name track))
         (have (gethash name my/music--lyrics))
         (cache (my/music--lyrics-file name)))
    (cond
     (have have)
     ((when-let ((tagged (my/music--tag-lyrics track)))
        (puthash name tagged my/music--lyrics)))
     ((file-exists-p cache)
      (puthash name (with-temp-buffer (insert-file-contents cache) (read (current-buffer)))
               my/music--lyrics))
     (t
      (puthash name 'pending my/music--lyrics)
      (my/music--lrclib-lookup
       track
       (lambda (result ok)
         (when (and result my/music-lyrics-store)
           (my/music--store-lyrics track result))
         (my/music--lyrics-done
          name (or result (list :source "lrclib" :none t :offline (not ok))))))
      'pending))))

(defun my/music-lyrics--render ()
  "Redraw the lyrics panel for whatever is playing."
  (when-let ((buf (get-buffer my/music-lyrics-buffer)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (track (and emms-player-playing-p (emms-playlist-current-selected-track)))
             (lyrics (and track (my/music--lyrics track)))
             (same (equal track my/music-lyrics--track)))
        (setq my/music-lyrics--track track
              my/music-lyrics--line nil
              my/music-lyrics--snap t)
        (erase-buffer)
        (insert "\n")
        (if (not track)
            (insert (propertize "Nothing playing" 'face '(:weight bold :height 1.2)) "\n")
          (insert (propertize (my/music--title track) 'face '(:weight bold :height 1.2)) "\n"
                  (propertize (or (emms-track-get track 'info-artist) "") 'face 'shadow) "\n\n")
          (pcase lyrics
            ('pending (insert (propertize "looking for lyrics…" 'face 'shadow)))
            ((guard (plist-get lyrics :instrumental))
             (insert (propertize "♪ instrumental" 'face 'shadow)))
            ((guard (plist-get lyrics :none))
             (insert (propertize (if (plist-get lyrics :offline)
                                     "couldn't reach lrclib — r to try again"
                                   "no lyrics found — r to look again")
                                 'face 'shadow)))
            ((guard (plist-get lyrics :synced))
             (let ((i 0))
               (dolist (line (plist-get lyrics :synced))
                 (insert (propertize (concat (if (string-empty-p (cdr line)) "♪" (cdr line)) "\n")
                                     'face 'shadow 'my/lyric i 'my/lyric-time (car line)))
                 (cl-incf i))))
            (_ (insert (plist-get lyrics :plain) "\n")))
          (insert "\n" (propertize (format "lyrics from %s%s"
                                           (if (consp lyrics) (plist-get lyrics :source) "lrclib")
                                           (if (plist-get (and (consp lyrics) lyrics) :synced) " · synced" ""))
                                   'face '(:inherit shadow :height 0.85))
                  "\n"))
        (unless same
          (goto-char (point-min))
          (dolist (w (get-buffer-window-list buf nil t))
            (set-window-point w (point-min))
            (set-window-start w (point-min))))
        (my/music-lyrics--follow)))))

(defun my/music-lyrics--highlight (seconds)
  "Light up the synced line for SECONDS into the song."
  (when-let ((buf (get-buffer my/music-lyrics-buffer)))
    (with-current-buffer buf
      (let ((pos (point-min)) found)
        ;; last line whose time has come (a little early reads better)
        (while (setq pos (next-single-property-change pos 'my/lyric-time))
          (let ((time (get-text-property pos 'my/lyric-time)))
            (when (and time (<= time (+ seconds 0.3)))
              (setq found pos))))
        (unless (equal found my/music-lyrics--line)
          (unless my/music-lyrics--overlay
            (setq my/music-lyrics--overlay (make-overlay 1 1)))
          (if (not found)
              (progn (delete-overlay my/music-lyrics--overlay)
                     (setq my/music-lyrics--line nil))
            (move-overlay my/music-lyrics--overlay found
                          (next-single-property-change found 'my/lyric (current-buffer) (point-max)))
            (overlay-put my/music-lyrics--overlay 'face
                         `(:foreground ,(my/music--color 'default :foreground "#dfe1e5")
                           :weight bold))
            ;; keep it in view, unless you've moved around the panel yourself
            (let ((old my/music-lyrics--line))
              (dolist (w (get-buffer-window-list buf nil t))
                (when (or my/music-lyrics--snap
                          (not (eq w (selected-window)))
                          (eql (window-point w) old))
                  (with-selected-window w
                    (goto-char found)
                    (recenter (/ (window-body-height) 3))))))
            (setq my/music-lyrics--line found
                  my/music-lyrics--snap nil)))))))

(defun my/music-lyrics--editing-p ()
  "Non-nil when a lyrics editor is on screen."
  (seq-some (lambda (buf) (and (eq (buffer-local-value 'major-mode buf) 'my/music-lyrics-edit-mode)
                               (get-buffer-window buf t)))
            (buffer-list)))

(defun my/music-lyrics--follow ()
  "Ask the player where it is and move the highlights (panel and editor) there."
  (let* ((buf (get-buffer my/music-lyrics-buffer))
         (panel (and buf (get-buffer-window buf t)))
         (editing (my/music-lyrics--editing-p)))
    (if (not (or panel editing))
        (when my/music-lyrics--timer
          (cancel-timer my/music-lyrics--timer)
          (setq my/music-lyrics--timer nil))
      (when emms-player-playing-p
        (let ((update (lambda (seconds)
                        (when (and panel (with-current-buffer buf
                                           (text-property-any (point-min) (point-max) 'my/lyric 0)))
                          (my/music-lyrics--highlight seconds))
                        (when editing (my/music-lyrics-edit--highlight seconds)))))
          (if (and (fboundp 'emms-player-mpv-cmd) (eq (emms-player-get emms-player-playing-p 'start)
                                                      (emms-player-get emms-player-mpv 'start)))
              (emms-player-mpv-cmd '(get_property time-pos)
                                   (lambda (data &rest _) (when (numberp data) (funcall update data))))
            (funcall update (if (boundp 'emms-playing-time) emms-playing-time 0))))))))

(defun my/music-lyrics--start-timer ()
  (unless my/music-lyrics--timer
    (setq my/music-lyrics--timer (run-at-time 0.4 0.4 #'my/music-lyrics--follow))))

(define-derived-mode my/music-lyrics-mode special-mode "Lyrics"
  "Lyrics for the playing track."
  (setq-local left-margin-width 2
              right-margin-width 1)
  (visual-line-mode 1))

(defvar my/music-lyrics-popped nil
  "Non-nil when lyrics live in their own frame rather than the side panel.")

(defun my/music-lyrics--frame ()
  "The popped-out lyrics frame, if there is one."
  (seq-find (lambda (f) (frame-parameter f 'my/music-lyrics)) (frame-list)))

(defun my/music-lyrics--buffer ()
  (let ((buf (get-buffer-create my/music-lyrics-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/music-lyrics-mode) (my/music-lyrics-mode)))
    buf))

(defun my/music-lyrics--start ()
  (my/music-lyrics--render)
  (my/music-page-refresh)
  (my/music-lyrics--start-timer))

(defun my/music-lyrics--dock ()
  "Show lyrics as a side panel in the selected frame."
  (display-buffer-in-side-window
   (my/music-lyrics--buffer)
   `((side . right) (slot . 0) (window-width . ,my/music-lyrics-width)
     (preserve-size . (t . nil))
     (window-parameters . ((no-delete-other-windows . t)))))
  (my/music-lyrics--start))

(defun my/music-lyrics--pop ()
  "Show lyrics in a frame of their own, which you can put anywhere."
  (let ((frame (make-frame `((name . "Lyrics") (my/music-lyrics . t)
                             (width . 56) (height . 34)
                             (tab-bar-lines . 0) (unsplittable . t)))))
    (with-selected-frame frame
      (switch-to-buffer (my/music-lyrics--buffer))
      (set-window-dedicated-p (selected-window) t))
    (select-frame-set-input-focus frame))
  (my/music-lyrics--start))

(defun my/music-lyrics--hide ()
  "Close the lyrics, wherever they are."
  (when-let ((frame (my/music-lyrics--frame)))
    (delete-frame frame))
  (when-let ((buf (get-buffer my/music-lyrics-buffer)))
    (dolist (w (get-buffer-window-list buf nil t))
      (ignore-errors (delete-window w))))
  (my/music-page-refresh))

(defun my/music-lyrics--shown-p ()
  (when-let ((buf (get-buffer my/music-lyrics-buffer)))
    (get-buffer-window-list buf nil t)))

(defun my/music-lyrics ()
  "Show or hide the lyrics (docked or popped out, whichever you used last)."
  (interactive)
  (cond ((my/music-lyrics--shown-p) (my/music-lyrics--hide))
        (my/music-lyrics-popped (my/music-lyrics--pop))
        (t (my/music-lyrics--dock))))

(defun my/music-lyrics-pop-toggle ()
  "Pop the lyrics out into their own frame, or dock them back."
  (interactive)
  (let ((popped (my/music-lyrics--frame)))
    (my/music-lyrics--hide)
    (setq my/music-lyrics-popped (not popped))
    (if popped
        ;; back beside the music page, in the main frame
        (let ((main (seq-find (lambda (f) (and (frame-visible-p f)
                                               (not (frame-parameter f 'my/music-lyrics))))
                              (frame-list))))
          (when main (select-frame-set-input-focus main))
          (my/music-lyrics--dock))
      (my/music-lyrics--pop))))

(defun my/music-lyrics-pin ()
  "Keep the popped-out lyrics above other windows (or stop doing so)."
  (interactive)
  (let ((frame (or (my/music-lyrics--frame)
                   (user-error "Pop the lyrics out first (P)"))))
    (set-frame-parameter frame 'z-group
                         (unless (eq (frame-parameter frame 'z-group) 'above) 'above))
    (message (if (eq (frame-parameter frame 'z-group) 'above)
                 "Lyrics pinned on top"
               "Lyrics unpinned"))))

(defun my/music-lyrics-preference (choice)
  "Set what happens when a song's own lyrics and lrclib's differ."
  (interactive
   (list (intern (completing-read "When a song has lyrics and lrclib has others too: "
                                  '("ask" "local" "online") nil t))))
  (setq my/music-lyrics-prefer choice)
  (my/music--save-state)
  (message "Lyrics: %s" (pcase choice ('ask "will ask each time")
                          ('local "always keep the song's own")
                          ('online "always take lrclib's"))))

(defun my/music--lyrics-keep-which ()
  "Ask whether to keep the song's own lyrics or lrclib's; returns local or online."
  (pcase my/music-lyrics-prefer
    ('local 'local)
    ('online 'online)
    (_ (pcase (car (read-multiple-choice
                    "This song already has lyrics (lrclib's are showing now)"
                    '((?l "keep local" "keep the lyrics saved in the song")
                      (?o "use online" "save lrclib's into the song instead")
                      (?L "always local" "always keep the song's own, don't ask again")
                      (?O "always online" "always take lrclib's, don't ask again"))))
         (?l 'local)
         (?o 'online)
         (?L (my/music-lyrics-preference 'local) 'local)
         (?O (my/music-lyrics-preference 'online) 'online)))))

(defun my/music-lyrics-refetch ()
  "Look this song up on lrclib again.
If the song already has lyrics of its own, lrclib's are shown and you
choose which to keep (see `my/music-lyrics-prefer')."
  (interactive)
  (let* ((track (or my/music-lyrics--track
                    (and emms-player-playing-p (emms-playlist-current-selected-track))
                    (user-error "Nothing playing")))
         (name (emms-track-name track))
         (local (my/music--tag-lyrics track)))
    (when (and local (eq my/music-lyrics-prefer 'local))
      (user-error "Keeping the song's own lyrics (M-x my/music-lyrics-preference to change)"))
    (ignore-errors (delete-file (my/music--lyrics-file name)))
    (puthash name 'pending my/music--lyrics)
    (my/music-lyrics--render)
    (my/music--lrclib-lookup
     track
     (lambda (result ok)
       (remhash name my/music--lyrics)
       (cond
        ((not result)
         (when local (puthash name local my/music--lyrics))
         (message (if ok "lrclib has nothing for this song" "Couldn't reach lrclib")))
        ((and local (equal (my/music--lyrics-text local) (my/music--lyrics-text result)))
         (puthash name local my/music--lyrics)
         (message "lrclib has the same lyrics the song already has"))
        (t
         ;; show lrclib's while you decide
         (puthash name result my/music--lyrics)
         (my/music-lyrics--render)
         (if (and local (eq (my/music--lyrics-keep-which) 'local))
             (progn (puthash name local my/music--lyrics)
                    (message "Kept the song's own lyrics"))
           (my/music--store-lyrics track result)
           (message "Lyrics from lrclib%s"
                    (if (my/music--url-p track) "" ", saved into the song")))))
       (my/music-lyrics--render)))))

;; lyrics for the whole library, one song at a time

(defvar my/music--lyrics-batch nil "Songs still to look up.")
(defvar my/music--lyrics-batch-stats nil "(checked found total).")

(defun my/music-lyrics-fetch-all ()
  "Download lyrics into every song in the library that has none.
Run it again to stop."
  (interactive)
  (if my/music--lyrics-batch
      (when (y-or-n-p "Stop downloading lyrics? ")
        (setq my/music--lyrics-batch nil)
        (message "Stopped downloading lyrics"))
    (my/music--ensure-library)
    (let ((tracks (seq-filter (lambda (track) (string-match-p "\\.mp3\\'" (emms-track-name track)))
                              (my/music--library-tracks))))
      (setq my/music--lyrics-batch tracks
            my/music--lyrics-batch-stats (list 0 0 (length tracks)))
      (message "Downloading lyrics for %d songs in the background (SPC m Y again to stop)"
               (length tracks))
      (my/music--lyrics-batch-next))))

(defun my/music--lyrics-batch-next ()
  (pcase-let ((`(,checked ,found ,total) my/music--lyrics-batch-stats))
    (if (null my/music--lyrics-batch)
        (when (= checked total)
          (setq my/music--lyrics-batch-stats nil)
          (message "Lyrics: done, found %d for songs that had none" found))
      (let* ((track (pop my/music--lyrics-batch))
             (name (emms-track-name track))
             (next (lambda ()
                     (setf (nth 0 my/music--lyrics-batch-stats) (1+ checked))
                     (when (zerop (% (1+ checked) 10))
                       (let ((message-log-max nil))
                         (message "Lyrics: %d/%d songs checked, %d found"
                                  (1+ checked) total (nth 1 my/music--lyrics-batch-stats))))
                     (run-at-time 0.1 nil #'my/music--lyrics-batch-next))))
        (if (or (my/music--tag-lyrics track) (file-exists-p (my/music--lyrics-file name)))
            (funcall next)
          (my/music--lrclib-lookup
           track
           (lambda (result ok)
             (cond (result
                    (cl-incf (nth 1 my/music--lyrics-batch-stats))
                    (puthash name result my/music--lyrics)
                    (my/music--store-lyrics track result (lambda (_) (funcall next))))
                   (t
                    ;; remember there's nothing, so playing it later doesn't ask again
                    (when ok (my/music--lyrics-done name (list :source "lrclib" :none t)))
                    (funcall next))))))))))

(defun my/music-lyrics-seek ()
  "Jump the song to the lyric line at point."
  (interactive)
  (let ((time (get-text-property (point) 'my/lyric-time)))
    (unless time (user-error "This line has no time"))
    (setq my/music-lyrics--snap t)
    (emms-seek-to time)))

(defun my/music-lyrics--track-changed (&rest _)
  (when (get-buffer my/music-lyrics-buffer)
    (my/music-lyrics--render)))

(defun my/music-lyrics-bigger ()
  "Make the lyrics text bigger."
  (interactive)
  (text-scale-increase 1))

(defun my/music-lyrics-smaller ()
  "Make the lyrics text smaller."
  (interactive)
  (text-scale-decrease 1))

(defun my/music-lyrics-reset-size ()
  "Put the lyrics text back to its normal size."
  (interactive)
  (text-scale-set 0))

;;; editing tags and lyrics
;; ---------------------------------------------------------------------
;; tags are written with python's mutagen (ffmpeg would turn lyrics into a
;; nonstandard TXXX frame and mangle comments). lyrics go in the standard
;; USLT frame, as LRC text when they're synced, so other players see them

(defconst my/music--tag-script "
import sys, json
from mutagen.id3 import ID3, ID3NoHeaderError, TIT2, TPE1, TALB, TDRC, TCON, TRCK, USLT, APIC
req = json.load(sys.stdin)
path = req['file']
target = path
if req.get('safe'):
    # the song is playing: write a copy and swap it in, so the player
    # (which keeps the old file open) never sees it change underneath
    import shutil
    target = path + '.limacs-tmp'
    shutil.copyfile(path, target)
try:
    tags = ID3(path)
except ID3NoHeaderError:
    tags = ID3()
frames = {'title': TIT2, 'artist': TPE1, 'album': TALB, 'year': TDRC,
          'genre': TCON, 'track': TRCK}
for key, cls in frames.items():
    if key in req:
        tags.delall(cls.__name__)
        if req[key]:
            tags.add(cls(encoding=3, text=[req[key]]))
if 'lyrics' in req:
    for key in ('USLT', 'TXXX:USLT', 'TXXX:lyrics', 'TXXX:LYRICS'):
        tags.delall(key)
    if req['lyrics']:
        tags.add(USLT(encoding=3, lang='eng', desc='', text=req['lyrics']))
if req.get('cover'):
    src = req['cover']
    if src.startswith('http'):
        import urllib.request
        r = urllib.request.Request(src, headers={'User-Agent': 'limacs-music'})
        data = urllib.request.urlopen(r, timeout=20).read()
    else:
        data = open(src, 'rb').read()
    mime = 'image/png' if data[:8] == b'\\x89PNG\\r\\n\\x1a\\n' else 'image/jpeg'
    tags.delall('APIC')
    tags.add(APIC(encoding=3, mime=mime, type=3, desc='Cover', data=data))
try:
    tags.save(target, v2_version=3)
    if target != path:
        import os
        os.replace(target, path)
except BaseException:
    if target != path:
        import os
        os.path.exists(target) and os.remove(target)
    raise
"
  "Writes the tags given as JSON on stdin into an mp3 (see `my/music--write-tags').")

(defun my/music--write-tags (file fields &optional callback)
  "Write FIELDS (an alist like ((title . \"x\") (lyrics . \"…\"))) into FILE.
A field set to \"\" removes that tag; fields left out are kept.  With
CALLBACK, write in the background and call it with nil when done, or
with the error text."
  (let* ((playing (and emms-player-playing-p
                       (equal file (emms-track-name (emms-playlist-current-selected-track)))))
         (json (json-encode (append (list (cons 'file file)
                                          (cons 'safe (if playing t :json-false)))
                                    fields))))
    (if callback
        (let* ((buf (generate-new-buffer " *music-tags*"))
               (proc (make-process
                      :name "music-tags" :buffer buf :noquery t :connection-type 'pipe
                      :command (list "python3" "-I" "-c" my/music--tag-script)
                      :sentinel (lambda (proc _)
                                  (unless (process-live-p proc)
                                    (let ((err (unless (zerop (process-exit-status proc))
                                                 (with-current-buffer buf (string-trim (buffer-string))))))
                                      (kill-buffer buf)
                                      (when err (message "Couldn't save tags to %s: %s"
                                                         (file-name-nondirectory file)
                                                         (car (last (split-string err "\n" t)))))
                                      (funcall callback err)))))))
          (process-send-string proc (encode-coding-string json 'utf-8))
          (process-send-eof proc))
      (with-temp-buffer
        (insert json)
        (let* ((coding-system-for-write 'utf-8)
               (status (call-process-region (point-min) (point-max) "python3" t t nil
                                            "-I" "-c" my/music--tag-script)))
          (unless (eql status 0)
            (error "Couldn't write tags: %s"
                   (car (last (split-string (buffer-string) "\n" t))))))))))

(defun my/music--read-tags (file)
  "FILE's tags as an alist of lowercase symbol -> string (ffprobe's view)."
  (with-temp-buffer
    (when (zerop (call-process "ffprobe" nil t nil "-v" "quiet" "-show_entries"
                               "format_tags" "-of" "json" file))
      (goto-char (point-min))
      (let ((json (ignore-errors (json-parse-buffer :object-type 'alist))))
        (mapcar (lambda (kv) (cons (intern (downcase (symbol-name (car kv)))) (cdr kv)))
                (alist-get 'tags (alist-get 'format json)))))))

(defun my/music--track-at-point ()
  "Track on the row at point (queue or library), else the one playing."
  (or (pcase (get-text-property (point) 'my/music-item)
        (`(queue ,_ ,track) track))
      (and (derived-mode-p 'my/music-library-mode)
           (tabulated-list-get-id)
           (my/music-lib--track-of (tabulated-list-get-id)))
      (and (derived-mode-p 'my/music-lyrics-mode 'my/music-lyrics-edit-mode)
           (or my/music-lyrics--track my/music-lyrics-edit--track))
      (and emms-player-playing-p (emms-playlist-current-selected-track))
      (user-error "No track here, and nothing is playing")))

;; the tag form

(defvar my/music-tag-fields
  '(("Title" . title) ("Artist" . artist) ("Album" . album)
    ("Year" . date) ("Genre" . genre) ("Track" . track))
  "Labels on the tag form and the ffprobe tag each one starts from.")

(defvar-local my/music-tags--track nil "Track the tag form edits.")

(define-derived-mode my/music-tags-mode text-mode "Tags"
  "Form for editing a track's tags."
  (setq-local header-line-format
              (propertize "  ZZ save   ZQ cancel   ,c pick a cover image (or drop one here)   (clear a field to remove it)"
                          'face 'shadow))
  (setq-local dnd-protocol-alist
              (append '(("^file:" . my/music-tags--drop) ("^https?://" . my/music-tags--drop))
                      dnd-protocol-alist)))

(defun my/music-edit-tags (&optional track)
  "Edit the tags of TRACK (the row at point, or what's playing)."
  (interactive)
  (let* ((track (or track (my/music--track-at-point)))
         (file (emms-track-name track)))
    (when (my/music--url-p track)
      (user-error "That's a link, not a file (D downloads it first)"))
    (unless (string-match-p "\\.mp3\\'" file)
      (user-error "Only mp3s can be edited"))
    (let ((tags (my/music--read-tags file))
          (buf (get-buffer-create "*Track Info*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (my/music-tags-mode)
          (setq my/music-tags--track track)
          (insert (propertize (concat "\n  " (file-name-nondirectory file) "\n\n")
                              'face 'shadow 'read-only t 'rear-nonsticky t))
          (dolist (field my/music-tag-fields)
            (insert (propertize (format "  %-8s" (concat (car field) ":"))
                                'face 'font-lock-keyword-face 'read-only t
                                'front-sticky t 'rear-nonsticky t)
                    (or (alist-get (cdr field) tags)
                        (and (eq (cdr field) 'date) (alist-get 'year tags))
                        "")
                    "\n"))
          (insert (propertize (format "  %-8s" "Cover:")
                              'face 'font-lock-keyword-face 'read-only t
                              'front-sticky t 'rear-nonsticky t)
                  "\n"
                  (propertize "\n  cover: a file path or an image link, empty keeps the current one\n"
                              'face 'shadow 'read-only t 'rear-nonsticky t))
          (goto-char (point-min))
          (search-forward "Title:")
          (skip-chars-forward " ")
          (set-buffer-modified-p nil)))
      (pop-to-buffer buf '((display-buffer-below-selected) (window-height . 14))))))

(defun my/music-tags--value (label)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^  " label ":[ \t]*\\(.*\\)$") nil t)
      (string-trim (match-string-no-properties 1)))))

(defun my/music-tags--set-cover (source)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^  Cover:[ \t]*")
    (let ((inhibit-read-only t))
      (delete-region (point) (line-end-position))
      (insert source))))

(defun my/music-tags-pick-cover ()
  "Fill the cover field from a file."
  (interactive)
  (my/music-tags--set-cover (expand-file-name (read-file-name "Cover image: " nil nil t))))

(defun my/music--dropped (url)
  "A dropped URL as a local path (for file: URLs) or the link itself."
  (if (string-prefix-p "file:" url) (dnd-get-local-file-name url t) url))

(defun my/music-tags--drop (url _action)
  "An image dropped on the tag form becomes the cover."
  (when-let ((source (my/music--dropped url)))
    (my/music-tags--set-cover source)
    (message "Cover set to %s (ZZ to save)" (file-name-nondirectory source)))
  'private)

(defun my/music-page--drop (url _action)
  "An image dropped on the music page becomes the cover of the track at point."
  (let* ((source (my/music--dropped url))
         (track (ignore-errors (my/music--track-at-point))))
    (cond ((not track) (message "Play something (or drop onto a queue row) to give it a cover"))
          ((my/music--url-p track) (message "That track is a link: download it first (D) to give it a cover"))
          ((y-or-n-p (format "Use %s as the cover of %s? "
                             (file-name-nondirectory source) (my/music--title track)))
           (let ((file (emms-track-name track)))
             (my/music--write-tags file (list (cons 'cover (my/music--cover-image source))))
             (my/music--forget-cover file)
             (my/music-page-refresh)
             (message "New cover for %s" (my/music--title track))))))
  'private)

(defun my/music--forget-cover (file)
  "Drop the cached cover of FILE so the page pulls the new one."
  (let ((cached (expand-file-name (concat (md5 file) ".jpg")
                                  (expand-file-name "covers/" my/music-state-directory))))
    (remhash cached my/music--cover-data)
    (ignore-errors (delete-file cached))
    (ignore-errors (delete-file (concat cached ".none")))))

(defun my/music--cover-image (source)
  "A local jpeg or png made from SOURCE (a file or a link), for embedding.
Links are downloaded, and other formats (webp, gif, …) converted to jpeg."
  (let* ((dir (make-temp-file "music-cover" t))
         (local (if (string-match-p "\\`https?://" source)
                    (let ((out (expand-file-name "download" dir)))
                      (unless (zerop (call-process "curl" nil nil nil "-sfL" "-m" "20"
                                                   "-A" my/music-lyrics-agent "-o" out source))
                        (user-error "Couldn't download %s" source))
                      out)
                  (expand-file-name source)))
         (magic (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally local nil 0 8)
                  (buffer-string))))
    (unless (file-readable-p local) (user-error "Can't read %s" source))
    (if (or (string-prefix-p "\xff\xd8" magic) (string-prefix-p "\x89PNG" magic))
        local
      (let ((out (expand-file-name "cover.jpg" dir)))
        (unless (zerop (call-process "ffmpeg" nil nil nil "-v" "quiet" "-y" "-i" local
                                     "-frames:v" "1" "-q:v" "2" out))
          (user-error "That isn't an image"))
        out))))

(defun my/music-tags-save ()
  "Write the form back into the file."
  (interactive)
  (let* ((track my/music-tags--track)
         (file (emms-track-name track))
         (fields (mapcar (lambda (field)
                           (cons (if (eq (cdr field) 'date) 'year (cdr field))
                                 (or (my/music-tags--value (car field)) "")))
                         my/music-tag-fields))
         (cover (my/music-tags--value "Cover")))
    (when (and cover (not (string-empty-p cover)))
      (push (cons 'cover (my/music--cover-image cover)) fields))
    (my/music--write-tags file fields)
    ;; refresh what emms knows about it
    (dolist (field '((title . info-title) (artist . info-artist)
                     (album . info-album) (year . info-year)))
      (let ((value (alist-get (car field) fields)))
        (emms-track-set track (cdr field) (unless (string-empty-p value) value))))
    (when (boundp 'emms-cache-db)
      (puthash file track emms-cache-db)
      (setq emms-cache-dirty t))
    (when (alist-get 'cover fields)
      (my/music--forget-cover file))
    (set-buffer-modified-p nil)
    (quit-window t)
    (my/music-page-refresh)
    (my/music-lyrics--track-changed)
    (message "Saved tags for %s" (file-name-nondirectory file))))

(defun my/music-tags-cancel ()
  "Close the form without saving."
  (interactive)
  (set-buffer-modified-p nil)
  (quit-window t))

;; the lyrics editor: type or paste lyrics, then play the song and press
;; RET as each line starts to stamp it

(defvar-local my/music-lyrics-edit--track nil "Track whose lyrics are being edited.")
(defvar-local my/music-lyrics-edit--overlay nil)

(defconst my/music--stamp-re "^\\[\\([0-9]+\\):\\([0-9.]+\\)\\][ \t]*"
  "An LRC time stamp at the start of a line.")

(define-derived-mode my/music-lyrics-edit-mode text-mode "Lyrics Edit"
  "Edit and time-stamp lyrics."
  (setq-local header-line-format
              (propertize "  RET stamp & next   ,0 play from start   ,j play this line   ,- ,= nudge   ,u unstamp   ,p pause   ZZ save   ZQ cancel"
                          'face 'shadow))
  (setq-local left-margin-width 1))

(defun my/music--stamp (seconds)
  (format "[%02d:%05.2f] " (floor seconds 60) (- seconds (* 60 (floor seconds 60)))))

(defun my/music--lyrics-text (lyrics)
  "LYRICS (a plist) as editable text: LRC when synced, plain otherwise."
  (cond ((plist-get lyrics :synced)
         (mapconcat (lambda (line) (concat (my/music--stamp (car line)) (cdr line)))
                    (plist-get lyrics :synced) "\n"))
        ((plist-get lyrics :plain))
        (t "")))

(defun my/music-edit-lyrics (&optional track)
  "Edit (and time-stamp) the lyrics of TRACK (row at point, or what's playing)."
  (interactive)
  (let* ((track (or track (my/music--track-at-point)))
         (have (gethash (emms-track-name track) my/music--lyrics))
         (buf (get-buffer-create (format "*Lyrics Edit: %s*" (my/music--title track)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/music-lyrics-edit-mode)
        (my/music-lyrics-edit-mode)
        (setq my/music-lyrics-edit--track track)
        (insert (if (consp have) (my/music--lyrics-text have) ""))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buf)
    (my/music-lyrics--start-timer)
    (when (= (buffer-size buf) 0)
      (message "Paste or type the lyrics, one line each, then ,0 and RET along with the song"))))

(defun my/music-lyrics-edit--playing-p ()
  "Non-nil when the song being edited is the one playing."
  (and emms-player-playing-p
       (equal (emms-track-name (emms-playlist-current-selected-track))
              (emms-track-name my/music-lyrics-edit--track))))

(defun my/music-lyrics-edit--position (callback)
  "Call CALLBACK with how far into the song the player is, in seconds."
  (if (and (fboundp 'emms-player-mpv-cmd)
           (eq (emms-player-get emms-player-playing-p 'start)
               (emms-player-get emms-player-mpv 'start)))
      (emms-player-mpv-cmd '(get_property time-pos)
                           (lambda (data &rest _) (when (numberp data) (funcall callback data))))
    (funcall callback (if (boundp 'emms-playing-time) emms-playing-time 0))))

(defun my/music-lyrics-edit--set-stamp (pos seconds)
  "Put a SECONDS stamp on the line at POS (replacing any it had)."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (looking-at my/music--stamp-re)
      (delete-region (match-beginning 0) (match-end 0)))
    (insert (my/music--stamp (max 0 seconds)))))

(defun my/music-lyrics-edit--line-time ()
  "The stamp on the line at point, in seconds, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at my/music--stamp-re)
      (+ (* 60 (string-to-number (match-string 1)))
         (string-to-number (match-string 2))))))

(defun my/music-lyrics-edit-tap ()
  "Stamp the line at point with the song's current time, then go to the next line."
  (interactive)
  (unless (my/music-lyrics-edit--playing-p)
    (user-error "Play this song first (,0 plays it from the start)"))
  (let ((marker (copy-marker (line-beginning-position)))
        (buf (current-buffer)))
    (my/music-lyrics-edit--position
     (lambda (seconds)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (my/music-lyrics-edit--set-stamp marker seconds)))))
    ;; move on straight away so quick taps land on the right lines
    (forward-line 1)
    (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
      (forward-line 1))))

(defun my/music-lyrics-edit-unstamp ()
  "Remove the stamp from the line at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at my/music--stamp-re)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun my/music-lyrics-edit--nudge (delta)
  (let ((time (or (my/music-lyrics-edit--line-time) (user-error "This line has no stamp"))))
    (my/music-lyrics-edit--set-stamp (point) (+ time delta))
    (message "%s" (string-trim (my/music--stamp (max 0 (+ time delta)))))))

(defun my/music-lyrics-edit-earlier ()
  "Move this line's stamp 0.1s earlier."
  (interactive)
  (my/music-lyrics-edit--nudge -0.1))

(defun my/music-lyrics-edit-later ()
  "Move this line's stamp 0.1s later."
  (interactive)
  (my/music-lyrics-edit--nudge 0.1))

(defun my/music-lyrics-edit-play-start ()
  "Play the song from the start and go to the first line."
  (interactive)
  (let ((track my/music-lyrics-edit--track))
    (if (my/music-lyrics-edit--playing-p)
        (emms-seek-to 0)
      (my/music--append track t)))
  (goto-char (point-min)))

(defun my/music-lyrics-edit-play-line ()
  "Play the song from a moment before this line's stamp, to check it."
  (interactive)
  (let ((time (or (my/music-lyrics-edit--line-time) (user-error "This line has no stamp"))))
    (unless (my/music-lyrics-edit--playing-p)
      (my/music--append my/music-lyrics-edit--track t))
    (run-at-time (if (my/music-lyrics-edit--playing-p) 0 1) nil
                 #'emms-seek-to (max 0 (- time 1)))))

(defun my/music-lyrics-edit--highlight (seconds)
  "Light up the line playing at SECONDS in any visible lyrics editor."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'my/music-lyrics-edit-mode)
                 (get-buffer-window buf t)
                 (my/music-lyrics-edit--playing-p))
        (let (found)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward my/music--stamp-re nil t)
              (when (<= (+ (* 60 (string-to-number (match-string 1)))
                           (string-to-number (match-string 2)))
                        (+ seconds 0.3))
                (setq found (line-beginning-position)))))
          (unless my/music-lyrics-edit--overlay
            (setq my/music-lyrics-edit--overlay (make-overlay 1 1))
            (overlay-put my/music-lyrics-edit--overlay 'face 'hl-line)
            (overlay-put my/music-lyrics-edit--overlay 'before-string
                         (propertize " " 'display '(left-fringe right-triangle success))))
          (if found
              (move-overlay my/music-lyrics-edit--overlay found
                            (save-excursion (goto-char found) (line-end-position)))
            (delete-overlay my/music-lyrics-edit--overlay)))))))

(defun my/music-lyrics-edit-save ()
  "Save the lyrics into the song (and show them in the panel)."
  (interactive)
  (let* ((track my/music-lyrics-edit--track)
         (name (emms-track-name track))
         (text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
         (synced (my/music--parse-lrc text))
         (lines (seq-remove #'string-blank-p (split-string text "\n")))
         (missing (and synced (seq-count (lambda (l) (not (string-match-p my/music--stamp-re l)))
                                         lines)))
         (result (cond ((string-empty-p text) nil)
                       (synced (list :source "you" :synced synced))
                       (t (list :source "you" :plain text)))))
    (when (and missing (> missing 0)
               (not (y-or-n-p (format "%d line%s have no stamp and won't show when synced. Save anyway? "
                                      missing (if (= missing 1) "" "s")))))
      (user-error "Not saved"))
    (unless (my/music--url-p track)
      (my/music--write-tags name (list (cons 'lyrics text))))
    (if result
        (my/music--lyrics-done name result)
      ;; cleared: forget them, and look online again
      (remhash name my/music--lyrics)
      (ignore-errors (delete-file (my/music--lyrics-file name)))
      (my/music-lyrics--render))
    (set-buffer-modified-p nil)
    (quit-window t)
    (message (if (my/music--url-p track)
                 "Lyrics saved (in emacs only: links have no file to hold them)"
               "Lyrics saved into the song"))))

(defun my/music-lyrics-edit-cancel ()
  "Close the lyrics editor without saving."
  (interactive)
  (when (or (not (buffer-modified-p)) (y-or-n-p "Throw away your lyrics changes? "))
    (set-buffer-modified-p nil)
    (quit-window t)))

;;; library view
;; ---------------------------------------------------------------------

(defvar my/music-library-buffer "*Music Library*")

(defvar-local my/music-lib--playlist nil
  "Playlist file shown in this view, or nil for the whole library.")
(defvar-local my/music-lib--filter nil
  "Words every shown track must match.")
(defvar-local my/music-lib--artist nil
  "Only show this artist, when set.")
(defvar-local my/music-lib--marks nil
  "Marked tracks (a hash table keyed on the track object).")

(defun my/music--playlist-tracks (file)
  "Tracks of playlist FILE, in order."
  (with-temp-buffer
    (insert-file-contents file)
    (let (tracks)
      (dolist (line (split-string (buffer-string) "\n" t))
        (unless (string-prefix-p "#" line)
          (push (emms-track (if (string-match-p "\\`https?://" line) 'url 'file) line)
                tracks)))
      (nreverse tracks))))

(defun my/music--write-playlist (file tracks)
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (dolist (track tracks)
      (insert (emms-track-name track) "\n"))))

(defun my/music--artist (track)
  (or (emms-track-get track 'info-artist) "Unknown artist"))

(defun my/music-lib--tracks ()
  "Tracks this view shows, after the artist and word filters."
  (let ((tracks (if my/music-lib--playlist
                    (my/music--playlist-tracks my/music-lib--playlist)
                  (my/music--library-tracks)))
        (words (and my/music-lib--filter
                    (split-string (downcase my/music-lib--filter) nil t))))
    (seq-filter
     (lambda (track)
       (and (or (null my/music-lib--artist)
                (equal (my/music--artist track) my/music-lib--artist))
            (let ((text (downcase (concat (my/music--artist track) " "
                                          (my/music--title track) " "
                                          (or (emms-track-get track 'info-album) "")))))
              (seq-every-p (lambda (word) (string-search word text)) words))))
     tracks)))

(defun my/music-lib--entries ()
  (let ((n 0))
    (mapcar (lambda (track)
              ;; playlist rows also carry their position (a track can repeat)
              (list (if my/music-lib--playlist (list :row (cl-incf n) track) track)
                    (vector (if (gethash track my/music-lib--marks)
                                (propertize "●" 'face 'success)
                              " ")
                            (my/music--artist track)
                            (my/music--title track)
                            (or (emms-track-get track 'info-album) "")
                            (or (emms-track-get track 'info-year) "")
                            (my/music--time (emms-track-get track 'info-playing-time)))))
            (my/music-lib--tracks))))

(defun my/music-lib--track-of (id)
  "The track behind a row ID.
note: tracks are lists themselves, so playlist rows are tagged with :row."
  (if (eq (car-safe id) :row) (nth 2 id) id))

(defun my/music-lib--row-of (id)
  "1-based position of a playlist row ID."
  (nth 1 id))

(defun my/music-lib--header ()
  (concat
   (propertize (if my/music-lib--playlist
                   (format " Playlist · %s" (file-name-base my/music-lib--playlist))
                 " Library")
               'face 'bold)
   (propertize (format "  %d tracks" (length tabulated-list-entries)) 'face 'shadow)
   (if my/music-lib--artist
       (concat "   " (propertize (concat "artist: " my/music-lib--artist) 'face 'success))
     "")
   (if my/music-lib--filter
       (concat "   " (propertize (concat "/" my/music-lib--filter) 'face 'success))
     "")
   (let ((marked (hash-table-count my/music-lib--marks)))
     (if (> marked 0) (propertize (format "   %d marked" marked) 'face 'warning) ""))
   (propertize (if my/music-lib--playlist
                   "    RET play  a queue  m mark  s add to playlist  d remove  J K move  / filter  q back"
                 "    RET play  a queue  m mark  s add to playlist  / filter  A artist  p play all  q back")
               'face 'shadow)))

(defun my/music-lib--refresh ()
  (setq tabulated-list-entries (my/music-lib--entries))
  (tabulated-list-print t)
  (setq header-line-format (my/music-lib--header)))

(define-derived-mode my/music-library-mode tabulated-list-mode "Library"
  "Browse, search and mark local tracks; build playlists from them."
  (setq tabulated-list-format
        [("" 2 nil)
         ("Artist" 26 t)
         ("Title" 42 t)
         ("Album" 30 t)
         ("Year" 6 t)
         ("Time" 7 nil :right-align t)])
  (setq tabulated-list-padding 1)
  (setq my/music-lib--marks (make-hash-table :test #'eq))
  (setq-local truncate-lines t)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun my/music--open-view (playlist &optional artist)
  "Show the library (or PLAYLIST file), optionally only ARTIST."
  (my/music--ensure-library)
  (let ((buf (get-buffer-create (if playlist
                                    (format "*Playlist · %s*" (file-name-base playlist))
                                  my/music-library-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/music-library-mode)
        (my/music-library-mode)
        ;; library sorts by artist; playlists keep their own order
        (setq tabulated-list-sort-key (unless playlist '("Artist"))))
      (setq my/music-lib--playlist playlist
            my/music-lib--artist artist)
      (my/music-lib--refresh)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun my/music-library ()
  "Browse and search the local library."
  (interactive)
  (my/music--open-view nil))

(defun my/music--read-artist ()
  (my/music--ensure-library)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (track (my/music--library-tracks))
      (cl-incf (gethash (my/music--artist track) counts 0)))
    (let* ((table (mapcar (lambda (artist)
                            (cons (concat artist
                                          (propertize (format "  %d" (gethash artist counts))
                                                      'face 'shadow))
                                  artist))
                          (sort (hash-table-keys counts) #'string-collate-lessp))))
      (cdr (assoc (completing-read "Artist: " table nil t) table)))))

(defun my/music-artist ()
  "Pick an artist and show their tracks."
  (interactive)
  (my/music--open-view nil (my/music--read-artist)))

;; --- actions in the view ---

(defun my/music-lib--targets ()
  "Marked tracks in view order, or the track at point."
  (let ((marked (seq-filter (lambda (track) (gethash track my/music-lib--marks))
                            (mapcar (lambda (entry) (my/music-lib--track-of (car entry)))
                                    tabulated-list-entries))))
    (or marked
        (let ((id (tabulated-list-get-id)))
          (if id (list (my/music-lib--track-of id)) (user-error "No track here"))))))

(defun my/music-lib-play ()
  "Play the track at point (in a playlist: play the playlist from here)."
  (interactive)
  (let ((id (or (tabulated-list-get-id) (user-error "No track here"))))
    (if my/music-lib--playlist
        (progn
          ;; load the whole playlist, then start at this row
          ;; (read the tracks here: the playlist file is local to this view)
          (let ((tracks (my/music--playlist-tracks my/music-lib--playlist)))
            (emms-stop)
            (with-current-emms-playlist
              (emms-playlist-clear)
              (dolist (track tracks)
                (emms-playlist-insert-track track))))
          (my/music--play-at (car (nth (1- (my/music-lib--row-of id)) (my/music--queue-items))))
          (my/music-page-refresh))
      (my/music--append (my/music-lib--track-of id) t))))

(defun my/music-lib-queue ()
  "Add the marked tracks (or the one at point) to the queue."
  (interactive)
  (let ((tracks (my/music-lib--targets)))
    (dolist (track tracks) (my/music--append track))
    (message "Queued %d track%s" (length tracks) (if (cdr tracks) "s" ""))))

(defun my/music-lib-play-all (&optional shuffle)
  "Replace the queue with the marked tracks, or everything shown, and play."
  (interactive)
  (let ((tracks (or (seq-filter (lambda (track) (gethash track my/music-lib--marks))
                                (mapcar (lambda (e) (my/music-lib--track-of (car e)))
                                        tabulated-list-entries))
                    (mapcar (lambda (e) (my/music-lib--track-of (car e)))
                            tabulated-list-entries))))
    (unless tracks (user-error "Nothing to play"))
    (my/music--queue tracks shuffle)
    (message "Playing %d tracks" (length tracks))))

(defun my/music-lib-shuffle-all ()
  (interactive)
  (my/music-lib-play-all t))

(defun my/music-lib-mark ()
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (puthash (my/music-lib--track-of id) t my/music-lib--marks)
    (my/music-lib--refresh)
    (forward-line 1)))

(defun my/music-lib-unmark ()
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (remhash (my/music-lib--track-of id) my/music-lib--marks)
    (my/music-lib--refresh)
    (forward-line 1)))

(defun my/music-lib-unmark-all ()
  (interactive)
  (clrhash my/music-lib--marks)
  (my/music-lib--refresh))

(defun my/music-lib-mark-all ()
  "Mark everything shown."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (puthash (my/music-lib--track-of (car entry)) t my/music-lib--marks))
  (my/music-lib--refresh))

(defun my/music-lib-filter (words)
  "Only show tracks matching all of WORDS (empty clears the filter)."
  (interactive (list (read-string "Filter (artist, title, album): " my/music-lib--filter)))
  (setq my/music-lib--filter (unless (string-empty-p (string-trim words)) (string-trim words)))
  (my/music-lib--refresh)
  (goto-char (point-min)))

(defun my/music-lib-artist ()
  "Only show one artist (pick again with no artist to clear)."
  (interactive)
  (setq my/music-lib--artist (my/music--read-artist))
  (my/music-lib--refresh)
  (goto-char (point-min)))

(defun my/music-lib-clear ()
  "Drop the artist and word filters."
  (interactive)
  (setq my/music-lib--artist nil my/music-lib--filter nil)
  (my/music-lib--refresh))

(defun my/music--read-playlist (prompt)
  "Pick an existing playlist or type a new name; return its file."
  (let* ((existing (mapcar #'file-name-base (my/music--playlists)))
         (name (string-trim (completing-read prompt existing))))
    (when (string-empty-p name) (user-error "A playlist needs a name"))
    (expand-file-name (concat name ".m3u") (my/music--playlists-dir))))

(defun my/music--add-to-playlist (file tracks)
  "Append TRACKS to playlist FILE (created if new) and redraw what shows it."
  (let ((before (and (file-exists-p file) (my/music--playlist-tracks file))))
    (my/music--write-playlist file (append before tracks))
    ;; adding from the library uses up the marks
    (when (derived-mode-p 'my/music-library-mode)
      (clrhash my/music-lib--marks)
      (my/music-lib--refresh))
    (when-let ((view (get-buffer (format "*Playlist · %s*" (file-name-base file)))))
      (with-current-buffer view (my/music-lib--refresh)))
    (my/music-page-refresh)
    (message "%s %d track%s to %s" (if before "Added" "Created playlist with")
             (length tracks) (if (cdr tracks) "s" "") (file-name-base file))))

(defun my/music-lib-add-to-playlist ()
  "Add the marked tracks (or the one at point) to a playlist, new or existing."
  (interactive)
  (let ((tracks (my/music-lib--targets)))
    (my/music--add-to-playlist
     (my/music--read-playlist (format "Add %d track%s to playlist: "
                                      (length tracks) (if (cdr tracks) "s" "")))
     tracks)))

(defun my/music-lib--edit-playlist (fn)
  "Rewrite the shown playlist with FN applied to (TRACKS . ROW-INDEX)."
  (unless my/music-lib--playlist (user-error "This isn't a playlist"))
  (let* ((tracks (my/music--playlist-tracks my/music-lib--playlist))
         (id (tabulated-list-get-id))
         (new (funcall fn tracks (and id (1- (my/music-lib--row-of id))))))
    (my/music--write-playlist my/music-lib--playlist new)
    (my/music-lib--refresh)
    (my/music-page-refresh)))

(defun my/music-lib-remove ()
  "Remove the marked tracks (or the one at point) from this playlist."
  (interactive)
  (let ((marked (hash-table-count my/music-lib--marks)))
    (my/music-lib--edit-playlist
     (lambda (tracks index)
       (if (> marked 0)
           (seq-remove (lambda (track) (gethash track my/music-lib--marks)) tracks)
         (append (seq-take tracks index) (seq-drop tracks (1+ index))))))
    (clrhash my/music-lib--marks)
    (my/music-lib--refresh)))

(defun my/music-lib--move (delta)
  (let ((line (line-number-at-pos)))
    (my/music-lib--edit-playlist
     (lambda (tracks index)
       (let ((target (+ index delta)))
         (if (or (< target 0) (>= target (length tracks)))
             tracks
           (let ((v (vconcat tracks)))
             (cl-rotatef (aref v index) (aref v target))
             (append v nil))))))
    (goto-char (point-min))
    (forward-line (+ line delta -1))))

(defun my/music-lib-move-down () (interactive) (my/music-lib--move 1))
(defun my/music-lib-move-up () (interactive) (my/music-lib--move -1))

(defun my/music-page-edit-playlist ()
  "Open the playlist at point for editing."
  (interactive)
  (pcase (my/music-page--item)
    (`(playlist ,file) (my/music--open-view file))
    (_ (user-error "Put the cursor on a playlist"))))

;;; spotify links: match each song on youtube / soundcloud
;; ---------------------------------------------------------------------

;; Spotify audio can't be played outside the Spotify app, so a Spotify
;; link (track, album or playlist) opens the match screen instead.  Song
;; info comes from Spotify's public embed page (no account needed), then
;; each song is searched on YouTube and SoundCloud.  Candidates whose length
;; matches Spotify's within a few seconds get a ✓.  You preview, accept,
;; decline or ask for more, one song at a time.

(defvar my/music-match-buffer "*Music Match*")

(defvar my/music-match-batch 5
  "How many results each source adds per search (f asks for this many more).")

(defvar my/music-match-tolerance 4
  "Seconds of length difference still counted as a match.")

(defvar my/music-match-timeout 20
  "Seconds before a source that hasn't answered is given up on.")

(defvar my/music-preview-extra-args nil
  "Extra mpv arguments for previews (tests use \"--ao=null\").")

(defvar-local my/music-match--title nil "What the link was (album / playlist name).")
(defvar-local my/music-match--songs nil "Vector of song plists from Spotify.")
(defvar-local my/music-match--index 0 "Song being matched.")
(defvar-local my/music-match--query nil "Search text for the current song.")
(defvar-local my/music-match--limit nil "Results asked of each source so far.")
(defvar-local my/music-match--candidates nil "Candidate plists for the current song.")
(defvar-local my/music-match--declined nil "URLs declined for the current song.")
(defvar-local my/music-match--searching nil "Sources still searching.")
(defvar-local my/music-match--failed nil "Sources that errored (e.g. SoundCloud).")

(defun my/music--spotify-link-p (url)
  (string-match-p "\\(?:open\\.spotify\\.com/\\|\\`spotify:\\)" url))

(defun my/music--spotify-fetch (url)
  "Return (KIND NAME SONGS) for Spotify URL; SONGS is a list of plists."
  (unless (string-match (concat "\\(?:open\\.spotify\\.com/\\(?:intl-[a-z-]+/\\)?\\|spotify:\\)"
                                "\\(track\\|album\\|playlist\\)[/:]\\([A-Za-z0-9]+\\)")
                        url)
    (user-error "Not a Spotify track, album or playlist link"))
  (let* ((kind (match-string 1 url))
         (id (match-string 2 url))
         (html (with-temp-buffer
                 (unless (zerop (call-process "curl" nil t nil "-sL" "-A" "Mozilla/5.0"
                                              (format "https://open.spotify.com/embed/%s/%s"
                                                      kind id)))
                   (user-error "Couldn't reach Spotify"))
                 (buffer-string))))
    (unless (string-match (concat "<script id=\"__NEXT_DATA__\" type=\"application/json\">"
                                  "\\(\\(?:.\\|\n\\)*?\\)</script>")
                          html)
      (user-error "Spotify's page didn't include the track info"))
    (let* ((data (json-parse-string (match-string 1 html)
                                    :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil))
           (entity (let-alist data .props.pageProps.state.data.entity))
           (preview (lambda (item) (alist-get 'url (alist-get 'audioPreview item)))))
      (if (equal kind "track")
          (list kind (alist-get 'name entity)
                (list (list :title (alist-get 'name entity)
                            :artist (mapconcat (lambda (a) (alist-get 'name a))
                                               (alist-get 'artists entity) ", ")
                            :duration (/ (or (alist-get 'duration entity) 0) 1000)
                            :preview (funcall preview entity))))
        (list kind (alist-get 'name entity)
              (mapcar (lambda (item)
                        (list :title (alist-get 'title item)
                              :artist (alist-get 'subtitle item)
                              :album (and (equal kind "album") (alist-get 'name entity))
                              :duration (/ (or (alist-get 'duration item) 0) 1000)
                              :preview (funcall preview item)))
                      (alist-get 'trackList entity)))))))

;; --- searching ---

(defun my/music-match--song ()
  (aref my/music-match--songs my/music-match--index))

(defun my/music-match--search (source)
  "Search SOURCE (`youtube' or `soundcloud') for the current song, in the background."
  (let* ((buf (current-buffer))
         (index my/music-match--index)
         (out (generate-new-buffer " *music-search*"))
         (err (generate-new-buffer " *music-search-err*"))
         (prefix (if (eq source 'youtube) "ytsearch" "scsearch")))
    (push source my/music-match--searching)
    (let ((proc (make-process
     :name (format "music-search-%s" source) :buffer out :stderr err :noquery t
     :command (list "yt-dlp" "--flat-playlist" "--no-warnings"
                    "--print" "%(webpage_url,url)s\t%(title)s\t%(channel,uploader)s\t%(duration)s"
                    (format "%s%d:%s" prefix my/music-match--limit my/music-match--query))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((lines (split-string (with-current-buffer out (buffer-string)) "\n" t)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               ;; ignore results that arrive after moving to another song
               (when (= index my/music-match--index)
                 (setq my/music-match--searching (delq source my/music-match--searching))
                 (if (and (null lines) (not (zerop (process-exit-status proc))))
                     (cl-pushnew source my/music-match--failed)
                   (dolist (line lines)
                     (pcase (split-string line "\t")
                       (`(,url ,title ,who ,duration)
                        (unless (or (member url my/music-match--declined)
                                    (seq-find (lambda (c) (equal (plist-get c :url) url))
                                              my/music-match--candidates))
                          (setq my/music-match--candidates
                                (append my/music-match--candidates
                                        (list (list :url url :title title
                                                    :who (if (equal who "NA") "" who)
                                                    :source source
                                                    :duration (let ((s (string-to-number duration)))
                                                                (and (> s 0) (round s))))))))))))
                 (my/music-match--render)))))
         (kill-buffer out)
         (kill-buffer err))))))
      ;; a source that hangs (soundcloud does sometimes) counts as unavailable
      (run-at-time my/music-match-timeout nil
                   (lambda () (when (process-live-p proc) (delete-process proc)))))))

(defun my/music-match--start-search ()
  "Search every source for the current song from scratch."
  (setq my/music-match--candidates nil
        my/music-match--failed nil
        my/music-match--searching nil)
  (my/music-match--search 'youtube)
  (my/music-match--search 'soundcloud)
  (my/music-match--render))

(defun my/music-match--good-p (candidate)
  (let ((want (plist-get (my/music-match--song) :duration))
        (have (plist-get candidate :duration)))
    (and want have (> want 0)
         (<= (abs (- want have)) my/music-match-tolerance))))

(defun my/music-match--sorted ()
  "Candidates with length matches first, otherwise in search order."
  (append (seq-filter #'my/music-match--good-p my/music-match--candidates)
          (seq-remove #'my/music-match--good-p my/music-match--candidates)))

;; --- screen ---

(defun my/music-match--render ()
  (let* ((inhibit-read-only t)
         (line (line-number-at-pos))
         (song (my/music-match--song))
         (count (length my/music-match--songs)))
    (erase-buffer)
    (insert "\n  " (propertize (plist-get song :title) 'face '(:weight bold :height 1.4)) "\n"
            "  " (propertize (string-join (delq nil (list (plist-get song :artist)
                                                          (plist-get song :album)
                                                          (my/music--time (plist-get song :duration))))
                                          "  ·  ")
                             'face 'shadow)
            (if (plist-get song :preview)
                (propertize "     P hear Spotify's preview" 'face 'shadow)
              "")
            "\n")
    (insert (my/music-page--heading (format "Candidates for \"%s\"" my/music-match--query)))
    (let ((candidates (my/music-match--sorted)))
      (if (and (null candidates) my/music-match--searching)
          (insert (propertize "    searching…\n" 'face 'shadow))
        (when (null candidates)
          (insert (propertize "    nothing found — f for more, e to change the search\n" 'face 'shadow)))
        (dolist (c candidates)
          (insert (propertize
                   (concat "  "
                           (if (my/music-match--good-p c) (propertize "✓ " 'face 'success) "  ")
                           (truncate-string-to-width (plist-get c :title) 58 nil ?\s "…")
                           "  "
                           (propertize (truncate-string-to-width (plist-get c :who) 20 nil ?\s "…")
                                       'face 'shadow)
                           "  "
                           (propertize (string-pad (if (eq (plist-get c :source) 'youtube)
                                                       "YouTube" "SoundCloud")
                                                   10)
                                       'face 'shadow)
                           (my/music--time (plist-get c :duration))
                           "\n")
                   'my/music-candidate c))))
      (when my/music-match--searching
        (when candidates (insert (propertize "    searching for more…\n" 'face 'shadow))))
      (when (memq 'soundcloud my/music-match--failed)
        (insert (propertize "    SoundCloud unavailable right now (yt-dlp can't reach it)\n"
                            'face 'shadow))))
    (when (> count 1)
      (insert (my/music-page--heading (or my/music-match--title "Songs") count))
      (dotimes (i count)
        (let* ((s (aref my/music-match--songs i))
               (status (plist-get s :status)))
          (insert (concat "  "
                          (cond ((= i my/music-match--index) (propertize "▶ " 'face 'success))
                                ((eq status 'accepted) (propertize "✓ " 'face 'success))
                                ((eq status 'skipped) (propertize "✗ " 'face 'shadow))
                                (t "  "))
                          (propertize (format "%3d  " (1+ i)) 'face 'shadow)
                          (propertize (concat (plist-get s :artist) " — " (plist-get s :title))
                                      'face (if (eq status 'skipped) 'shadow 'default))
                          "\n")))))
    (setq header-line-format
          (concat (propertize (format " Match · %s" (or my/music-match--title "Spotify")) 'face 'bold)
                  (if (> count 1)
                      (propertize (format "  song %d of %d" (1+ my/music-match--index) count)
                                  'face 'shadow)
                    "")
                  (propertize "    p preview  RET queue  D download  x decline  f more  e search  s skip  ] [ songs  q close"
                              'face 'shadow)))
    (goto-char (point-min))
    (forward-line (1- line))
    ;; land on the first candidate when the cursor isn't on one
    (unless (get-text-property (point) 'my/music-candidate)
      (let ((match (text-property-search-forward 'my/music-candidate nil nil)))
        (when match (goto-char (prop-match-beginning match)))))))

(defun my/music-match--candidate ()
  (or (get-text-property (point) 'my/music-candidate)
      (user-error "Put the cursor on a candidate")))

(defun my/music-match--go (index)
  "Move to song INDEX and search for it."
  (my/music-preview-stop)
  (setq my/music-match--index index
        my/music-match--limit my/music-match-batch
        my/music-match--declined nil)
  (let ((song (my/music-match--song)))
    (setq my/music-match--query (concat (plist-get song :artist) " - " (plist-get song :title))))
  (goto-char (point-min))
  (my/music-match--start-search))

(defun my/music-match--advance ()
  "Go to the next song still waiting, or say we're done."
  (let ((next (seq-find (lambda (i) (null (plist-get (aref my/music-match--songs i) :status)))
                        (number-sequence (1+ my/music-match--index)
                                         (1- (length my/music-match--songs))))))
    (if next
        (my/music-match--go next)
      (my/music-preview-stop)
      (my/music-match--render)
      (message "All songs done — q closes this screen"))))

(defun my/music-match-open (url)
  "Open the match screen for Spotify URL."
  (message "Reading Spotify link…")
  (pcase-let ((`(,kind ,name ,songs) (my/music--spotify-fetch url)))
    (unless songs (user-error "No songs found in that link"))
    (let ((buf (get-buffer-create my/music-match-buffer)))
      (with-current-buffer buf
        (my/music-match-mode)
        (setq my/music-match--title (if (equal kind "track") nil name)
              my/music-match--songs (vconcat songs))
        (my/music-match--go 0))
      (switch-to-buffer buf))))

;; --- preview ---

(defvar my/music--preview-process nil)

(defun my/music-preview-stop ()
  "Stop a running preview."
  (interactive)
  (when (process-live-p my/music--preview-process)
    (delete-process my/music--preview-process))
  (setq my/music--preview-process nil))

(defun my/music--preview (url &optional from-start)
  "Play a short clip of URL (from a quarter in, unless FROM-START)."
  (my/music-preview-stop)
  ;; don't talk over the music
  (when (and (bound-and-true-p emms-player-playing-p) (not emms-player-paused-p))
    (emms-pause))
  (setq my/music--preview-process
        (make-process
         :name "music-preview" :noquery t :buffer nil
         :command (append (list "mpv" "--no-video" "--really-quiet"
                                (format "--volume=%d" my/music-volume))
                          (unless from-start (list "--start=25%" "--length=25"))
                          my/music-preview-extra-args
                          (list url))))
  (message "Previewing… (p again to stop)"))

(defun my/music-match-preview ()
  "Preview the candidate at point (again to stop)."
  (interactive)
  (if (process-live-p my/music--preview-process)
      (my/music-preview-stop)
    (my/music--preview (plist-get (my/music-match--candidate) :url))))

(defun my/music-match-preview-spotify ()
  "Hear Spotify's own 30s preview of the song, to compare."
  (interactive)
  (let ((url (plist-get (my/music-match--song) :preview)))
    (unless url (user-error "Spotify has no preview for this song"))
    (my/music--preview url t)))

;; --- decisions ---

(defun my/music-match--track (candidate)
  "An emms url track for CANDIDATE, tagged with Spotify's song info."
  (let ((song (my/music-match--song))
        (track (emms-track 'url (plist-get candidate :url))))
    (emms-track-set track 'info-title (plist-get song :title))
    (emms-track-set track 'info-artist (plist-get song :artist))
    (when (plist-get song :album) (emms-track-set track 'info-album (plist-get song :album)))
    (when (plist-get candidate :duration)
      (emms-track-set track 'info-playing-time (plist-get candidate :duration)))
    track))

(defun my/music-match--accept (download)
  (my/music-preview-stop)
  (let* ((candidate (my/music-match--candidate))
         (track (my/music-match--track candidate)))
    (if download
        (my/music-download track)
      (my/music--append track)
      (message "Queued %s" (my/music--description track)))
    (setf (plist-get (aref my/music-match--songs my/music-match--index) :status) 'accepted)
    (my/music-match--advance)))

(defun my/music-match-accept ()
  "Queue the candidate at point and go to the next song."
  (interactive)
  (my/music-match--accept nil))

(defun my/music-match-download ()
  "Download the candidate at point as an mp3 into the library, then next song."
  (interactive)
  (my/music-match--accept t))

(defun my/music-match-decline ()
  "Drop the candidate at point."
  (interactive)
  (my/music-preview-stop)
  (let ((candidate (my/music-match--candidate)))
    (push (plist-get candidate :url) my/music-match--declined)
    (setq my/music-match--candidates (delq candidate my/music-match--candidates))
    (my/music-match--render)
    (when (null my/music-match--candidates)
      (my/music-match-more))))

(defun my/music-match-more ()
  "Ask every source for more results."
  (interactive)
  (setq my/music-match--limit (+ my/music-match--limit my/music-match-batch))
  (my/music-match--search 'youtube)
  (unless (memq 'soundcloud my/music-match--failed)
    (my/music-match--search 'soundcloud))
  (my/music-match--render))

(defun my/music-match-edit-search (query)
  "Search again with QUERY."
  (interactive (list (read-string "Search for: " my/music-match--query)))
  (setq my/music-match--query query
        my/music-match--limit my/music-match-batch)
  (my/music-match--start-search))

(defun my/music-match-skip ()
  "Skip this song."
  (interactive)
  (my/music-preview-stop)
  (setf (plist-get (aref my/music-match--songs my/music-match--index) :status) 'skipped)
  (my/music-match--advance))

(defun my/music-match-next-song ()
  (interactive)
  (when (< (1+ my/music-match--index) (length my/music-match--songs))
    (my/music-match--go (1+ my/music-match--index))))

(defun my/music-match-previous-song ()
  (interactive)
  (when (> my/music-match--index 0)
    (my/music-match--go (1- my/music-match--index))))

(defun my/music-match-quit ()
  (interactive)
  (my/music-preview-stop)
  (my/music-back-to-page))

(define-derived-mode my/music-match-mode special-mode "Match"
  "Match Spotify songs to YouTube / SoundCloud uploads."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

;;; emms setup
;; ---------------------------------------------------------------------

(use-package emms
  :ensure t
  :commands (emms-browser emms-pause emms-next emms-previous emms-stop emms-show)
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        ;; a function, so mpv starts at the saved volume
        emms-player-mpv-parameters
        (lambda () (list "--quiet" "--really-quiet" "--no-audio-display" "--no-video"
                         (format "--volume=%d" my/music-volume)))
        emms-source-file-default-directory my/music-directory
        emms-info-functions '(my/music-info)
        emms-info-asynchronously t
        emms-track-description-function #'my/music--description
        emms-browser-default-browse-type 'info-artist
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-repeat-playlist t)
  ;; skip macOS ._ files and other dotfiles when adding folders
  (setq emms-source-file-exclude-regexp
        (concat emms-source-file-exclude-regexp "\\|/\\.[^/]*\\'"))
  ;; note: plain alternation on purpose, the scan goes through `find' and
  ;; its regex dialect doesn't understand regexp-opt's (?: groups
  (emms-player-set emms-player-mpv 'regex
                   (concat "\\.\\("
                           (mapconcat #'identity
                                      (append my/music-extensions
                                              (mapcar #'upcase my/music-extensions))
                                      "\\|")
                           "\\)\\'"))
  ;; our own mode line and page show the track, so turn off emms's
  (emms-mode-line-disable)
  (emms-playing-time-display-mode -1)
  ;; media keys + the desktop media widget
  (when (require 'emms-mpris nil t)
    (ignore-errors (emms-mpris-enable)))
  (my/music--load-state)
  ;; keep the queue between sessions
  (emms-history-load)
  (dolist (hook '(emms-player-started-hook emms-player-stopped-hook
                  emms-player-finished-hook emms-player-paused-hook
                  emms-playlist-selection-changed-hook))
    (add-hook hook #'my/music-page-refresh)
    (add-hook hook #'my/music-lyrics--track-changed))
  (add-hook 'emms-player-started-hook #'my/music--apply-volume)
  (unless my/music--timer
    (setq my/music--timer (run-at-time 1 1 #'my/music-page--tick))))

;;; click menus
;; ---------------------------------------------------------------------

;; Every row on the music page and in the library has a right-click menu
;; with what you can do to it.  A left click selects, a double click
;; plays, and a left click on the progress bar seeks.  The menus only
;; call the same commands the keys do.

(defmacro my/music--do (&rest body)
  "A menu action running BODY."
  `(lambda () (interactive) ,@body))

(defun my/music--menu (title items)
  "A popup menu keymap called TITLE from ITEMS, in order.
Each item is \"--\" (a separator), (LABEL :sub ITEMS) for a submenu,
\(LABEL COMMAND . PROPS) where PROPS go to `menu-item' (:enable FORM,
:button ...), or nil, which is skipped."
  (let ((map (make-sparse-keymap title))
        (n 0))
    (dolist (item items)
      (let ((key (vector (intern (format "item-%d" (cl-incf n))))))
        (pcase item
          ('nil nil)
          ((pred stringp) (define-key-after map key (list 'menu-item item)))
          (`(,label :sub ,sub)
           (define-key-after map key (list 'menu-item label (my/music--menu label sub))))
          (`(,label ,command . ,props)
           (define-key-after map key `(menu-item ,label ,command ,@props))))))
    map))

(defun my/music--plural (n word)
  (format "%d %s%s" n word (if (= n 1) "" "s")))

(defun my/music--read-new-playlist ()
  "Ask for a new playlist's name; return its file."
  (let* ((name (string-trim (read-string "New playlist name: ")))
         (file (expand-file-name (concat name ".m3u") (my/music--playlists-dir))))
    (when (string-empty-p name) (user-error "A playlist needs a name"))
    file))

(defun my/music--playlist-items (tracks)
  "Submenu items adding TRACKS to each playlist, or to a new one."
  (let ((files (my/music--playlists)))
    `(,@(mapcar (lambda (file)
                  (list (file-name-base file)
                        (my/music--do (my/music--add-to-playlist file tracks))))
                files)
      ,@(when files '("--"))
      ("New playlist…" ,(my/music--do (my/music--add-to-playlist
                                       (my/music--read-new-playlist) tracks))))))

(defun my/music--insert-next (tracks)
  "Put TRACKS in the queue right after the one playing."
  (with-current-buffer (my/music--queue-buffer)
    (let ((inhibit-read-only t)
          (selected (and emms-playlist-selected-marker
                         (eq (marker-buffer emms-playlist-selected-marker) (current-buffer))
                         (marker-position emms-playlist-selected-marker))))
      (save-excursion
        (if selected
            (progn (goto-char selected) (forward-line 1))
          (goto-char (point-max)))
        (dolist (track tracks) (emms-playlist-insert-track track)))))
  (my/music-page-refresh)
  (message "Playing next: %s" (if (cdr tracks)
                                   (my/music--plural (length tracks) "track")
                                 (my/music--title (car tracks)))))

(defun my/music--playing-p (track)
  (and emms-player-playing-p (eq track (emms-playlist-current-selected-track))))

(defun my/music--track-items (track)
  "Menu items for TRACK that fit anywhere it appears."
  (let ((local (not (my/music--url-p track))))
    `("--"
      ,(when (my/music--playing-p track) '("Show lyrics" my/music-lyrics))
      ("Edit tags…" ,(my/music--do (my/music-edit-tags track)) :enable ,local)
      ("Edit lyrics…" ,(my/music--do (my/music-edit-lyrics track)) :enable ,local)
      ,(if local
           (list (concat "Everything by " (my/music--artist track))
                 (my/music--do (my/music--open-view nil (my/music--artist track))))
         (list "Download as mp3" (my/music--do (my/music-download track)))))))

(defun my/music--general-items ()
  "Menu items for the player itself."
  `((,(if (and emms-player-playing-p (not emms-player-paused-p)) "Pause" "Play")
     my/music-toggle)
    ("Next" my/music-next)
    ("Previous" my/music-previous)
    ("Volume…" my/music-set-volume)
    "--"
    ("Find a track…" ,(my/music--vertico #'my/music-find))
    ("Library" my/music-library)
    ("Artists…" ,(my/music--vertico #'my/music-artist))
    ("Shuffle whole library" my/music-shuffle-all)
    ("Play a link…" my/music-play-link)
    ("Search online…" ,(my/music--vertico #'my/music-search-online))
    "--"
    ("Queue" :sub (("Shuffle it" my/music-shuffle-queue)
                   ("Save as playlist…" my/music-save-playlist)
                   ("Clear it…" my/music-clear-queue)))
    ("Shuffle mode" my/music-toggle-shuffle
     :button (:toggle . (bound-and-true-p emms-random-playlist)))
    (,(format "Repeat: %s (change)"
              (cond ((bound-and-true-p emms-repeat-track) "one")
                    ((bound-and-true-p emms-repeat-playlist) "all")
                    (t "off")))
     my/music-cycle-repeat)
    ("New playlist…" my/music-new-playlist)
    "--"
    ("Rescan library" my/music-scan)))

(defun my/music-page--menu (item)
  "The menu for ITEM on the music page (nil: the page itself)."
  (pcase item
    (`(queue ,_ ,track)
     (my/music--menu
      (my/music--title track)
      `(("Play" my/music-page-open)
        ("Add to playlist" :sub ,(my/music--playlist-items (list track)))
        ("Move up" my/music-page-move-up)
        ("Move down" my/music-page-move-down)
        ("Remove from queue" my/music-page-remove)
        ,@(my/music--track-items track))))
    (`(playlist ,file)
     (let ((tracks (my/music--playlist-tracks file)))
       (my/music--menu
        (file-name-base file)
        `(("Play" my/music-page-open :enable ,(and tracks t))
          ("Shuffle" ,(my/music--do (my/music--queue tracks t)) :enable ,(and tracks t))
          ("Play next" ,(my/music--do (my/music--insert-next tracks)) :enable ,(and tracks t))
          ("Add to queue" ,(my/music--do (dolist (track tracks) (my/music--append track))
                                         (message "Queued %s" (my/music--plural (length tracks) "track")))
           :enable ,(and tracks t))
          "--"
          ("Open (edit, reorder)" my/music-page-edit-playlist)
          ("Rename…" ,(my/music--do (my/music-rename-playlist file)))
          ("Duplicate…" ,(my/music--do (my/music-duplicate-playlist file)))
          ("Delete…" my/music-page-remove)
          "--"
          ("New playlist…" my/music-new-playlist)))))
    (`(now)
     (let ((track (and emms-player-playing-p (emms-playlist-current-selected-track))))
       (my/music--menu
        "Now playing"
        (append
         (when track
           `(("Add to playlist" :sub ,(my/music--playlist-items (list track)))
             ,@(cdr (my/music--track-items track))
             "--"))
         (my/music--general-items)))))
    (_ (my/music--menu "Music" (my/music--general-items)))))

(defun my/music-lib--menu ()
  "The menu for the library row at point, or for the marked tracks."
  (let* ((id (tabulated-list-get-id))
         (track (and id (my/music-lib--track-of id)))
         (marked (hash-table-count my/music-lib--marks))
         (targets (ignore-errors (my/music-lib--targets)))
         (what (if (> marked 0) (concat " " (my/music--plural marked "marked track")) "")))
    (my/music--menu
     (cond ((> marked 0) (my/music--plural marked "marked track"))
           (track (my/music--title track))
           (t "Library"))
     (append
      (when targets
        `(("Play" my/music-lib-play :enable ,(and id t))
          (,(format "Play%s next" what) ,(my/music--do (my/music--insert-next targets)))
          (,(format "Add%s to queue" what) my/music-lib-queue)
          (,(format "Add%s to playlist" what) :sub ,(my/music--playlist-items targets))
          ,@(when my/music-lib--playlist
              `((,(format "Remove%s from this playlist" what) my/music-lib-remove)
                ,(when (= marked 0) '("Move up" my/music-lib-move-up))
                ,(when (= marked 0) '("Move down" my/music-lib-move-down))))
          "--"
          ,(when track
             (if (gethash track my/music-lib--marks)
                 '("Unmark" my/music-lib-unmark)
               '("Mark" my/music-lib-mark)))))
      `(("Mark everything shown" my/music-lib-mark-all)
        ,(when (> marked 0) '("Unmark all" my/music-lib-unmark-all))
        ("Play everything shown" my/music-lib-play-all)
        ("Shuffle everything shown" my/music-lib-shuffle-all)
        "--"
        ("Filter…" my/music-lib-filter)
        ("Only one artist…" ,(my/music--vertico #'my/music-lib-artist))
        ,(when (or my/music-lib--filter my/music-lib--artist)
           '("Show everything" my/music-lib-clear)))
      (when track (my/music--track-items track))
      '("--" ("Back to the music page" my/music-back-to-page))))))

;; managing playlists

(defun my/music-new-playlist ()
  "Make a new, empty playlist."
  (interactive)
  (let ((file (my/music--read-new-playlist)))
    (when (file-exists-p file) (user-error "%s already exists" (file-name-base file)))
    (my/music--write-playlist file nil)
    (my/music-page-refresh)
    (message "Made %s — add tracks from the library (l) or any track's menu"
             (file-name-base file))))

(defun my/music-rename-playlist (file)
  (let* ((name (string-trim (read-string "Rename playlist to: " (file-name-base file))))
         (new (expand-file-name (concat name ".m3u") (my/music--playlists-dir))))
    (when (string-empty-p name) (user-error "A playlist needs a name"))
    (rename-file file new)
    ;; a view still showing the old name would write to the old file
    (when-let ((view (get-buffer (format "*Playlist · %s*" (file-name-base file)))))
      (kill-buffer view))
    (my/music-page-refresh)
    (message "Renamed to %s" name)))

(defun my/music-duplicate-playlist (file)
  (let* ((name (string-trim (read-string "Name of the copy: "
                                         (concat (file-name-base file) " copy"))))
         (new (expand-file-name (concat name ".m3u") (my/music--playlists-dir))))
    (when (string-empty-p name) (user-error "A playlist needs a name"))
    (copy-file file new)
    (my/music-page-refresh)
    (message "Copied to %s" name)))

;; the clicks

(defun my/music--click-goto (event)
  "Select the window clicked in EVENT and move point to the click."
  (let* ((start (event-start event))
         (win (posn-window start)))
    (when (window-live-p win) (select-window win))
    (when (numberp (posn-point start)) (goto-char (posn-point start)))
    ;; show the row highlighted before the menu opens
    (redisplay t)))

(defun my/music--click-seek ()
  "Seek to where the click landed on the progress bar, if it did."
  (let* ((track (emms-playlist-current-selected-track))
         (total (and track emms-player-playing-p (emms-track-get track 'info-playing-time)))
         (column (- (current-column) 2)))
    (when (and total (> total 0) (get-text-property (point) 'my/music-progress)
               (<= 0 column 47))
      (emms-seek-to (* total (/ (+ column 0.5) 48.0)))
      t)))

(defun my/music--menu-click (event menu-fn)
  (my/music--click-goto event)
  (popup-menu (funcall menu-fn) event))

(defun my/music-page-click-menu (event)
  "Open the menu for what was clicked on the music page."
  (interactive "e")
  (my/music--menu-click
   event (lambda () (my/music-page--menu (get-text-property (point) 'my/music-item)))))

(defun my/music-left-click (event)
  "Select the clicked row; on the progress bar, seek there."
  (interactive "e")
  (my/music--click-goto event)
  (my/music--click-seek))

(defun my/music-lib-click-menu (event)
  "Open the menu for the clicked library row."
  (interactive "e")
  (my/music--menu-click event #'my/music-lib--menu))

(defun my/music-page-double-click (event)
  (interactive "e")
  (my/music--click-goto event)
  (when (get-text-property (point) 'my/music-item)
    (my/music-page-open)))

(defun my/music-lib-double-click (event)
  (interactive "e")
  (my/music--click-goto event)
  (when (tabulated-list-get-id) (my/music-lib-play)))

;;; keys
;; ---------------------------------------------------------------------

(defun my/music--vertico (fn)
  (lambda ()
    (interactive)
    (if (fboundp 'my/with-vertico) (my/with-vertico fn) (call-interactively fn))))

(with-eval-after-load 'evil
  ;; everything music is in normal state, so SPC and C-h/j/k/l work as usual
  (evil-set-initial-state 'my/music-mode 'normal)
  (evil-set-initial-state 'emms-browser-mode 'normal)
  (evil-set-initial-state 'emms-playlist-mode 'normal)

  (evil-define-key 'normal my/music-mode-map
    (kbd "RET") #'my/music-page-open
    "p" #'my/music-toggle
    "]" #'my/music-next
    "[" #'my/music-previous
    ">" #'my/music-seek-forward
    "<" #'my/music-seek-backward
    "+" #'my/music-volume-up
    "=" #'my/music-volume-up
    "-" #'my/music-volume-down
    "V" #'my/music-set-volume
    "/" (my/music--vertico #'my/music-find)
    "a" (my/music--vertico #'my/music-add)
    "u" #'my/music-play-link
    "o" (my/music--vertico #'my/music-search-online)
    "D" #'my/music-page-download
    "d" #'my/music-page-remove
    "x" #'my/music-page-remove
    "J" #'my/music-page-move-down
    "K" #'my/music-page-move-up
    "c" #'my/music-clear-queue
    "w" #'my/music-save-playlist
    "s" #'my/music-shuffle-queue
    "S" #'my/music-toggle-shuffle
    "r" #'my/music-cycle-repeat
    "l" #'my/music-library
    "A" (my/music--vertico #'my/music-artist)
    "e" #'my/music-page-edit-playlist
    "b" #'my/music-browse
    "R" #'my/music-scan
    "n" #'my/music-new-playlist
    "?" #'my/music-help
    [down-mouse-1] #'my/music-left-click
    [down-mouse-3] #'my/music-page-click-menu
    [double-mouse-1] #'my/music-page-double-click
    [mouse-1] #'ignore [mouse-3] #'ignore [drag-mouse-1] #'ignore
    "L" #'my/music-lyrics
    "E" #'my/music-edit-lyrics
    "T" #'my/music-edit-tags
    (kbd "g r") #'my/music-page-refresh
    "q" #'quit-window)

  (with-eval-after-load 'emms-browser
    (evil-define-key 'normal emms-browser-mode-map
      (kbd "RET") #'emms-browser-add-tracks-and-play
      "a" #'emms-browser-add-tracks
      (kbd "TAB") #'emms-browser-toggle-subitems
      "q" #'my/music-back-to-page))

  (with-eval-after-load 'emms-playlist-mode
    (evil-define-key 'normal emms-playlist-mode-map
      (kbd "RET") #'emms-playlist-mode-play-smart
      "d" #'emms-playlist-mode-kill-track
      "q" #'my/music-back-to-page))

  (evil-define-key 'normal 'global
    (kbd "<leader>mm") #'my/music
    (kbd "<leader>mf") (my/music--vertico #'my/music-find)
    (kbd "<leader>ma") (my/music--vertico #'my/music-add)
    (kbd "<leader>mr") #'my/music-shuffle-all
    (kbd "<leader>mu") #'my/music-play-link
    (kbd "<leader>mo") (my/music--vertico #'my/music-search-online)
    (kbd "<leader>m SPC") #'my/music-toggle
    (kbd "<leader>mn") #'my/music-next
    (kbd "<leader>mp") #'my/music-previous
    (kbd "<leader>ms") #'emms-stop
    (kbd "<leader>m+") #'my/music-volume-up
    (kbd "<leader>m-") #'my/music-volume-down
    (kbd "<leader>mR") #'my/music-scan
    (kbd "<leader>ml") #'my/music-library
    (kbd "<leader>mA") (my/music--vertico #'my/music-artist)
    (kbd "<leader>mL") #'my/music-lyrics
    (kbd "<leader>me") #'my/music-edit-lyrics
    (kbd "<leader>mt") #'my/music-edit-tags
    (kbd "<leader>mY") #'my/music-lyrics-fetch-all)

  (evil-set-initial-state 'my/music-lyrics-mode 'normal)
  (evil-define-key 'normal my/music-lyrics-mode-map
    (kbd "RET") #'my/music-lyrics-seek
    "r" #'my/music-lyrics-refetch
    "P" #'my/music-lyrics-pop-toggle
    "t" #'my/music-lyrics-pin
    "e" #'my/music-edit-lyrics
    "T" #'my/music-edit-tags
    "+" #'my/music-lyrics-bigger
    "-" #'my/music-lyrics-smaller
    "=" #'my/music-lyrics-reset-size
    "L" #'my/music-lyrics
    "q" #'my/music-lyrics)

  (evil-define-key 'normal my/music-lyrics-edit-mode-map
    (kbd "RET") #'my/music-lyrics-edit-tap
    (kbd ",0") #'my/music-lyrics-edit-play-start
    (kbd ",j") #'my/music-lyrics-edit-play-line
    (kbd ",-") #'my/music-lyrics-edit-earlier
    (kbd ",=") #'my/music-lyrics-edit-later
    (kbd ",u") #'my/music-lyrics-edit-unstamp
    (kbd ",p") #'my/music-toggle
    (kbd ",s") #'my/music-lyrics-edit-save
    (kbd ",q") #'my/music-lyrics-edit-cancel
    "ZZ" #'my/music-lyrics-edit-save
    "ZQ" #'my/music-lyrics-edit-cancel)

  (evil-define-key 'normal my/music-tags-mode-map
    (kbd ",c") #'my/music-tags-pick-cover
    (kbd ",s") #'my/music-tags-save
    (kbd ",q") #'my/music-tags-cancel
    "ZZ" #'my/music-tags-save
    "ZQ" #'my/music-tags-cancel))

  (evil-set-initial-state 'my/music-match-mode 'normal)
  (evil-define-key 'normal my/music-match-mode-map
    "p" #'my/music-match-preview
    "P" #'my/music-match-preview-spotify
    (kbd "RET") #'my/music-match-accept
    "y" #'my/music-match-accept
    "D" #'my/music-match-download
    "x" #'my/music-match-decline
    "d" #'my/music-match-decline
    "f" #'my/music-match-more
    "e" #'my/music-match-edit-search
    "s" #'my/music-match-skip
    "]" #'my/music-match-next-song
    "[" #'my/music-match-previous-song
    "q" #'my/music-match-quit)

  (evil-set-initial-state 'my/music-library-mode 'normal)
  (evil-define-key 'normal my/music-library-mode-map
    (kbd "RET") #'my/music-lib-play
    "a" #'my/music-lib-queue
    "p" #'my/music-lib-play-all
    "P" #'my/music-lib-shuffle-all
    "m" #'my/music-lib-mark
    "u" #'my/music-lib-unmark
    "U" #'my/music-lib-unmark-all
    "M" #'my/music-lib-mark-all
    "s" #'my/music-lib-add-to-playlist
    "/" #'my/music-lib-filter
    "A" (my/music--vertico #'my/music-lib-artist)
    "C" #'my/music-lib-clear
    "d" #'my/music-lib-remove
    "J" #'my/music-lib-move-down
    "K" #'my/music-lib-move-up
    "E" #'my/music-edit-lyrics
    "T" #'my/music-edit-tags
    (kbd "g r") #'my/music-lib--refresh
    [down-mouse-1] #'my/music-left-click
    [down-mouse-3] #'my/music-lib-click-menu
    [double-mouse-1] #'my/music-lib-double-click
    [mouse-1] #'ignore [mouse-3] #'ignore [drag-mouse-1] #'ignore
    "q" #'my/music-back-to-page)

;; media keys while emacs has focus (mpris covers them everywhere else)
(global-set-key (kbd "<XF86AudioPlay>") #'my/music-toggle)
(global-set-key (kbd "<XF86AudioNext>") #'my/music-next)
(global-set-key (kbd "<XF86AudioPrev>") #'my/music-previous)
(global-set-key (kbd "<XF86AudioStop>") #'emms-stop)

(with-eval-after-load 'vertico-multiform
  (dolist (cmd '(my/music-find my/music-add my/music-search-online my/music-artist
                 my/music-lib-artist))
    (add-to-list 'vertico-multiform-commands (list cmd 'posframe))))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC m" "music"))

(provide 'my-music)

;;; my-music.el ends here
