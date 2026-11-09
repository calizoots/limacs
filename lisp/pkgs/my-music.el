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
  "Load emms and mount the music drive if `my/music-directory' isn't there yet."
  (require 'emms)
  (unless (file-directory-p my/music-directory)
    (message "Mounting music drive…")
    ;; may pop up a polkit password prompt, that's fine
    (call-process "udisksctl" nil nil nil "mount" "-b" my/music-device)
    (unless (file-directory-p my/music-directory)
      (user-error "Music drive isn't mounted (%s)" my/music-device))))

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
          (setq my/music-volume (plist-get state :volume)))))))

(defun my/music--save-state ()
  (make-directory my/music-state-directory t)
  (with-temp-file (my/music--state-file)
    (prin1 (list :volume my/music-volume) (current-buffer))))

(defun my/music--apply-volume (&rest _)
  (when (and emms-player-playing-p (fboundp 'emms-player-mpv-cmd))
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
         (playlists (my/music--playlists)))
    (erase-buffer)
    ;; now playing
    (insert "\n")
    (if track
        (insert "  " (propertize (my/music--title track) 'face '(:weight bold :height 1.4)) "\n"
                "  " (propertize (string-join
                                  (delq nil (list (emms-track-get track 'info-artist)
                                                  (emms-track-get track 'info-album)
                                                  (emms-track-get track 'info-year)
                                                  (and (my/music--url-p track) "link")))
                                  "  ·  ")
                                 'face 'shadow)
                "\n\n"
                (my/music-page--progress))
      (insert "  " (propertize "Nothing playing" 'face '(:weight bold :height 1.4)) "\n"
              "  " (propertize "/ find a track   o search online   u play a link" 'face 'shadow)
              "\n\n\n"))
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
                                                         72 nil ?\s "…")
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
                                    (propertize (format "%d tracks" (my/music--playlist-size file))
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
                (propertize "    RET play   p pause   ] [ skip   / find   l library   A artist   u link   o online   + - volume   ? more"
                            'face 'shadow)))
  (hl-line-mode 1))

(defun my/music-help ()
  "Show every key on the music page."
  (interactive)
  (message (concat "RET play/load  p pause  ] [ next/prev  > < seek  + - = volume  "
                   "/ find&play  a add  u link  o online  D download link  "
                   "d remove  J K move  c clear  w save playlist  "
                   "s shuffle queue  S shuffle mode  r repeat  l library  A artist  "
                   "e edit playlist  b browse  R rescan  q close")))

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

(defun my/music-lib-add-to-playlist ()
  "Add the marked tracks (or the one at point) to a playlist, new or existing."
  (interactive)
  (let* ((tracks (my/music-lib--targets))
         (file (my/music--read-playlist (format "Add %d track%s to playlist: "
                                                (length tracks) (if (cdr tracks) "s" ""))))
         (before (and (file-exists-p file) (my/music--playlist-tracks file))))
    (my/music--write-playlist file (append before tracks))
    (clrhash my/music-lib--marks)
    (my/music-lib--refresh)
    (my/music-page-refresh)
    (message "%s %d track%s to %s" (if before "Added" "Created playlist with")
             (length tracks) (if (cdr tracks) "s" "") (file-name-base file))))

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
    (add-hook hook #'my/music-page-refresh))
  (add-hook 'emms-player-started-hook #'my/music--apply-volume)
  (unless my/music--timer
    (setq my/music--timer (run-at-time 1 1 #'my/music-page--tick))))

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
    "?" #'my/music-help
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
    (kbd "<leader>mA") (my/music--vertico #'my/music-artist)))

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
    (kbd "g r") #'my/music-lib--refresh
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
