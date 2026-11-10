;;; runtime/my-containers.el  -*- lexical-binding: t; -*-

;; the Containers page: define, run and look after containers

;; Containers are written down once and then started, stopped, reset and
;; opened from one page.  A project keeps its own in .emacs-containers.eld
;; at its root; ones that belong to no project go in
;; ~/.emacs.d/containers.eld.  Both are a list of plists:
;;
;;   ((:name "luhorm-pg" :image "docker.io/library/postgres:17"
;;     :ports ("5433:5432")                     ; host:container
;;     :env ("POSTGRES_USER=dev" "POSTGRES_PASSWORD=dev" "POSTGRES_DB=luhorm")
;;     :volumes ("luhorm-pg-data:/var/lib/postgresql/data")   ; optional
;;     :args ("--shm-size=256m")                ; optional, extra run flags
;;     :command ("postgres" "-c" "fsync=off")   ; optional, after the image
;;     :shell ("psql" "-U" "dev" "-d" "luhorm") ; what `e' opens (default sh)
;;     :reset "psql -U dev -d luhorm -c 'DROP SCHEMA public CASCADE; CREATE SCHEMA public;'"
;;     :url "postgres://dev:dev@localhost:5433/luhorm"))   ; `y' copies it
;;
;; `a' adds one from a template (postgres, redis, ...) so you rarely write
;; these by hand.  Containers you didn't define (started some other way)
;; are listed at the end and can be stopped, started, opened and removed.
;;
;; Runs on podman, or docker when its daemon is up.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC d d  this project's containers in a panel on the right (from code)
;;   SPC d t  the Containers tab: every project, and containers made elsewhere
;;   SPC d u  start this project's containers   SPC d k  stop them
;;
;; On the page:
;;   RET  log of the container (follows)     s  start / stop
;;   r    restart                            R  recreate (after editing it)
;;   x    reset (runs its :reset)            e  shell inside it
;;   y    copy its url                       D  remove the container
;;   a    add one from a template            E  edit the definitions
;;   g    refresh                            q  close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function ghostel-exec "ghostel")
(declare-function my/project-known-roots "projects")
(declare-function my/project-root "projects")

(defvar my/containers-buffer "*Containers*")

(defvar my/containers-file ".emacs-containers.eld"
  "A project's container definitions, at its root.")

(defvar my/containers-global-file (locate-user-emacs-file "containers.eld")
  "Container definitions that belong to no project.")

(defvar my/containers-engine nil
  "\"podman\" or \"docker\"; nil picks docker when its daemon answers, else podman.")

(defvar my/containers-interval 3
  "Seconds between refreshes while the page is on screen.")

;;; the engine
;; ---------------------------------------------------------------------

(defvar my/containers--engine nil)

(defun my/containers-engine ()
  (or my/containers-engine my/containers--engine
      (setq my/containers--engine
            (cond ((and (executable-find "docker")
                        (zerop (call-process "docker" nil nil nil "info" "--format" "ok")))
                   "docker")
                  ((executable-find "podman") "podman")
                  ((executable-find "docker") "docker")
                  (t (user-error "Neither podman nor docker is installed"))))))

(defun my/containers--run (args &optional on-done)
  "Run the engine with ARGS in the background; ON-DONE gets (OK OUTPUT)."
  ;; stderr apart: podman prints warnings there that would spoil the json
  (let ((buffer (generate-new-buffer " *containers-cmd*"))
        (errors (generate-new-buffer " *containers-err*")))
    (make-process
     :name "containers" :buffer buffer :stderr errors :noquery t
     :command (cons (my/containers-engine) args)
     :connection-type 'pipe
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let* ((ok (zerop (process-exit-status proc)))
                          (output (with-current-buffer buffer (string-trim (buffer-string))))
                          (problem (with-current-buffer errors (string-trim (buffer-string)))))
                     (kill-buffer buffer)
                     (when (buffer-live-p errors) (kill-buffer errors))
                     (when on-done (funcall on-done ok (if ok output (concat output "\n" problem))))))))))

;;; definitions
;; ---------------------------------------------------------------------

(defvar my/containers--cache (make-hash-table :test #'equal)
  "File -> (MTIME . DEFINITIONS).")

(defun my/containers--read-file (file)
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        (hit (gethash file my/containers--cache)))
    (if (and hit (equal (car hit) mtime))
        (cdr hit)
      (let ((defs (condition-case err
                      (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))
                    (error (message "Containers: can't read %s: %s" file (error-message-string err))
                           nil))))
        (puthash file (cons mtime defs) my/containers--cache)
        defs))))

(defun my/containers--project-file (root)
  (expand-file-name my/containers-file root))

(defun my/containers--groups ()
  "((ROOT-OR-NIL . DEFINITIONS) ...): each project with containers, then global."
  (let (groups)
    (dolist (root (and (fboundp 'my/project-known-roots) (my/project-known-roots)))
      (let ((file (my/containers--project-file root)))
        (when (file-exists-p file)
          (when-let ((defs (my/containers--read-file file)))
            (push (cons root defs) groups)))))
    (setq groups (nreverse groups))
    (when (file-exists-p my/containers-global-file)
      (when-let ((defs (my/containers--read-file my/containers-global-file)))
        (setq groups (append groups (list (cons nil defs))))))
    groups))

(defun my/containers--project-defs (root)
  (let ((file (my/containers--project-file root)))
    (and (file-exists-p file) (my/containers--read-file file))))

(defun my/containers--run-args (def)
  "The `run' arguments that create DEF's container."
  (append (list "run" "-d" "--name" (plist-get def :name)
                "--label" "emacs.container=1")
          (mapcan (lambda (p) (list "-p" p)) (plist-get def :ports))
          (mapcan (lambda (e) (list "-e" e)) (plist-get def :env))
          (mapcan (lambda (v) (list "-v" v)) (plist-get def :volumes))
          (plist-get def :args)
          (list (plist-get def :image))
          (plist-get def :command)))

;;; what's running
;; ---------------------------------------------------------------------

(defvar my/containers--state (make-hash-table :test #'equal)
  "Container name -> plist (:state :status :image :ports).")

(defvar my/containers--fetching nil)

(defun my/containers--ports-string (ports)
  (cond ((stringp ports) ports)
        ((sequencep ports)
         (mapconcat (lambda (p)
                      (format "%s→%s" (alist-get 'host_port p) (alist-get 'container_port p)))
                    ports " "))
        (t "")))

(defun my/containers--parse (output)
  "Container rows from `ps -a --format json' (podman: an array; docker: lines)."
  (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol))
    (if (string-prefix-p "[" output)
        (ignore-errors (json-read-from-string output))
      (delq nil (mapcar (lambda (line) (ignore-errors (json-read-from-string line)))
                        (split-string output "\n" t))))))

(defun my/containers-fetch (&optional then)
  "Ask the engine what containers exist; THEN runs after."
  (unless my/containers--fetching
    (setq my/containers--fetching t)
    (my/containers--run
     '("ps" "-a" "--format" "json")
     (lambda (ok output)
       (setq my/containers--fetching nil)
       (when ok
         (clrhash my/containers--state)
         (dolist (row (my/containers--parse output))
           (let* ((names (alist-get 'Names row))
                  (name (if (listp names) (car names) names)))
             (puthash name (list :state (downcase (format "%s" (alist-get 'State row)))
                                 :status (alist-get 'Status row)
                                 :image (alist-get 'Image row)
                                 :ports (my/containers--ports-string (alist-get 'Ports row)))
                      my/containers--state))))
       (when then (funcall then))))))

(defun my/containers--info (name)
  (gethash name my/containers--state))

(defun my/containers-running-p (name)
  (equal (plist-get (my/containers--info name) :state) "running"))

;;; the page
;; ---------------------------------------------------------------------

(defvar-local my/containers--root 'all
  "What this buffer shows: `all' (the tab) or one project root (a panel).")

(defun my/containers--status (info)
  (let ((state (plist-get info :state)))
    (cond ((equal state "running") (or (plist-get info :status) "running"))
          (info (format "stopped (%s)" state))
          (t "not created"))))

(defun my/containers--row (name image info &optional ports url item)
  (let* ((running (equal (plist-get info :state) "running"))
         (dot (propertize (if running "●" "○") 'face (if running 'success 'shadow)))
         (status (my/containers--status info))
         (line (if (eq my/containers--root 'all)
                   (concat "  " dot " "
                           (propertize (string-pad name 22) 'face 'bold) " "
                           (string-pad (truncate-string-to-width (or image "") 26 nil nil "…") 27)
                           (propertize (string-pad (truncate-string-to-width status 24 nil nil "…") 25)
                                       'face (if running 'success 'shadow))
                           (string-pad (or ports "") 18)
                           (if url (propertize url 'face 'link) "")
                           "\n")
                 ;; the panel is narrow: name, then status and ports under it
                 (concat "  " dot " " (propertize name 'face 'bold) "\n"
                         "    " (propertize (truncate-string-to-width status 20 nil nil "…")
                                            'face (if running 'success 'shadow))
                         (if (and ports (not (string-empty-p ports)))
                             (propertize (concat "  " ports) 'face 'shadow)
                           "")
                         "\n"))))
    (propertize line 'my/containers-item item)))

(defun my/containers--render-1 ()
  "Draw the current containers buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (defined (make-hash-table :test #'equal))
        (groups (if (eq my/containers--root 'all)
                    (my/containers--groups)
                  ;; a panel: its project, and the global ones
                  (seq-filter (lambda (group) (or (null (car group))
                                                  (equal (car group) my/containers--root)))
                              (my/containers--groups)))))
    (erase-buffer)
    (insert "\n")
    (unless groups
      (insert (propertize "  No containers here yet:\n  press a to add one.\n" 'face 'shadow)))
    (pcase-dolist (`(,root . ,defs) groups)
      (insert (propertize (concat "  " (if root
                                           (file-name-nondirectory (directory-file-name root))
                                         "Global")
                                  "\n")
                          'face 'font-lock-keyword-face
                          'my/containers-item (list 'group root)))
      (dolist (def defs)
        (let ((name (plist-get def :name)))
          (puthash name t defined)
          (insert (my/containers--row name (plist-get def :image) (my/containers--info name)
                                      (string-join (plist-get def :ports) " ")
                                      (plist-get def :url)
                                      (list 'def def root)))))
      (insert "\n"))
    (when (eq my/containers--root 'all)
      (let (others)
        (maphash (lambda (name info) (unless (gethash name defined) (push (cons name info) others)))
                 my/containers--state)
        (when others
          (insert (propertize "  Other containers\n" 'face 'font-lock-keyword-face))
          (dolist (other (sort others (lambda (a b) (string< (car a) (car b)))))
            (insert (my/containers--row (car other) (plist-get (cdr other) :image) (cdr other)
                                        (plist-get (cdr other) :ports) nil
                                        (list 'other (car other))))))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun my/containers--buffers ()
  (seq-filter (lambda (buf) (eq (buffer-local-value 'major-mode buf) 'my/containers-mode))
              (buffer-list)))

(defun my/containers--render ()
  (dolist (buf (my/containers--buffers))
    (with-current-buffer buf
      (my/containers--render-1))))

(defun my/containers-refresh ()
  "Ask the engine again and redraw."
  (interactive)
  (my/containers--render)
  (my/containers-fetch #'my/containers--render))

(defvar my/containers--timer nil)

(defun my/containers--tick ()
  (if (seq-some (lambda (buf) (get-buffer-window buf t)) (my/containers--buffers))
      (my/containers-fetch #'my/containers--render)
    (cancel-timer my/containers--timer)
    (setq my/containers--timer nil)))

(define-derived-mode my/containers-mode special-mode "Containers"
  "Containers you defined, and any others the engine knows about."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (my/containers-refresh)))
  (display-line-numbers-mode -1)
  (hl-line-mode 1))

(defun my/containers--header ()
  (if (eq my/containers--root 'all)
      (concat (propertize " Containers" 'face 'bold)
              (propertize "    RET log  s start/stop  r restart  R recreate  x reset  e shell  y url  D remove  a add  E edit"
                          'face 'shadow))
    (concat (propertize " Containers" 'face 'bold)
            (propertize "  s start/stop  RET log  ? keys" 'face 'shadow))))

(defun my/containers--buffer (&optional root)
  "The Containers buffer: everything, or ROOT's containers for a panel."
  (let ((buf (get-buffer-create
              (if root
                  (format "*Containers · %s*" (file-name-nondirectory (directory-file-name root)))
                my/containers-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/containers-mode) (my/containers-mode))
      (setq my/containers--root (or root 'all)
            default-directory (or root default-directory)
            header-line-format (my/containers--header)))
    (my/containers-refresh)
    (unless my/containers--timer
      (setq my/containers--timer (run-at-time my/containers-interval my/containers-interval
                                              #'my/containers--tick)))
    buf))

;;; the side panel
;; ---------------------------------------------------------------------

(defvar my/containers-panel-width 46
  "Width of the containers panel, in columns.")

(add-to-list 'window-persistent-parameters '(my-containers-panel . writable))

(defun my/containers--panel-window ()
  (seq-find (lambda (w) (window-parameter w 'my-containers-panel))
            (window-list nil 'nomini)))

(defun my/containers-panel-toggle ()
  "Show or hide this project's containers on the right of the tab."
  (interactive)
  (let ((window (my/containers--panel-window)))
    (if (window-live-p window)
        (if (eq window (selected-window))
            (delete-window window)
          (select-window window))
      (let* ((root (or (and (fboundp 'my/project-root) (my/project-root))
                       (user-error "This tab has no project (SPC d t shows every container)")))
             (window (display-buffer-in-side-window
                      (my/containers--buffer root)
                      `((side . right) (slot . 0)
                        (window-width . ,my/containers-panel-width)
                        (preserve-size . (t . nil))))))
        (set-window-parameter window 'my-containers-panel t)
        (set-window-parameter window 'no-delete-other-windows t)
        (select-window window)))))

(defun my/containers ()
  "Open the Containers tab."
  (interactive)
  (let ((index (seq-position (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs))
                             "Containers")))
    (if index
        (tab-bar-select-tab (1+ index))
      (tab-new)
      (tab-bar-rename-tab "Containers"))
    (let ((buf (my/containers--buffer)))
      (unless (get-buffer-window buf)
        (delete-other-windows)
        (switch-to-buffer buf))
      (select-window (get-buffer-window buf)))))

;;; acting on a row
;; ---------------------------------------------------------------------

(defun my/containers--below ()
  "Where logs and shells go: under the page, or across the bottom from a panel."
  (if (window-parameter (selected-window) 'my-containers-panel)
      '((display-buffer-in-side-window) (side . bottom) (slot . 1) (window-height . 0.35))
    '((display-buffer-below-selected) (window-height . 0.45))))

;;; row commands
;; ---------------------------------------------------------------------

(defun my/containers--item ()
  (or (get-text-property (point) 'my/containers-item)
      (user-error "No container on this line")))

(defun my/containers--name (item)
  (pcase item
    (`(def ,def ,_) (plist-get def :name))
    (`(other ,name) name)
    (_ (user-error "No container on this line"))))

(defun my/containers--def (item)
  (pcase item
    (`(def ,def ,_) def)))

(defun my/containers--after (verb name)
  "A callback reporting VERB on NAME and redrawing."
  (lambda (ok output)
    (if ok
        (message "%s %s" verb name)
      (message "%s %s failed: %s" verb name
               (car (last (split-string output "\n" t)))))
    (my/containers-refresh)))

(defun my/containers-start (def)
  "Start DEF's container, creating it the first time (pulls the image if needed)."
  (let ((name (plist-get def :name)))
    (if (my/containers--info name)
        (my/containers--run (list "start" name) (my/containers--after "Started" name))
      (message "Creating %s (the first time pulls %s)…" name (plist-get def :image))
      (my/containers--run (my/containers--run-args def) (my/containers--after "Created" name)))))

(defun my/containers-stop (name)
  (my/containers--run (list "stop" name) (my/containers--after "Stopped" name)))

(defun my/containers-toggle ()
  "Start the container on this line, or stop it if it's running."
  (interactive)
  (let* ((item (my/containers--item))
         (name (my/containers--name item)))
    (cond ((my/containers-running-p name) (my/containers-stop name))
          ((my/containers--def item) (my/containers-start (my/containers--def item)))
          (t (my/containers--run (list "start" name) (my/containers--after "Started" name))))))

(defun my/containers-restart ()
  (interactive)
  (let ((name (my/containers--name (my/containers--item))))
    (my/containers--run (list "restart" name) (my/containers--after "Restarted" name))))

(defun my/containers-recreate ()
  "Remove the container and create it again from its definition."
  (interactive)
  (let* ((def (or (my/containers--def (my/containers--item))
                  (user-error "Only containers you defined can be recreated")))
         (name (plist-get def :name)))
    (message "Recreating %s…" name)
    (my/containers--run (list "rm" "-f" name)
                        (lambda (_ok _output)
                          (my/containers--run (my/containers--run-args def)
                                              (my/containers--after "Recreated" name))))))

(defun my/containers-remove ()
  (interactive)
  (let ((name (my/containers--name (my/containers--item))))
    (when (yes-or-no-p (format "Remove container %s (its data goes unless it's in a volume)? " name))
      (my/containers--run (list "rm" "-f" name) (my/containers--after "Removed" name)))))

(defun my/containers-reset ()
  "Run the container's :reset command inside it (wipe the database...)."
  (interactive)
  (let* ((def (or (my/containers--def (my/containers--item))
                  (user-error "Only containers you defined have a reset")))
         (name (plist-get def :name))
         (reset (or (plist-get def :reset)
                    (user-error "%s has no :reset (E to add one)" name))))
    (unless (my/containers-running-p name)
      (user-error "%s isn't running" name))
    (when (y-or-n-p (format "Reset %s? " name))
      (my/containers--run (list "exec" name "sh" "-c" reset)
                          (my/containers--after "Reset" name)))))

(defun my/containers-copy-url ()
  (interactive)
  (let ((url (plist-get (my/containers--def (my/containers--item)) :url)))
    (unless url (user-error "No :url for this one"))
    (kill-new url)
    (message "Copied %s" url)))

(defun my/containers-log ()
  "Follow the container's log in the bottom half."
  (interactive)
  (let* ((name (my/containers--name (my/containers--item)))
         (buffer (get-buffer-create (format "*container log: %s*" name))))
    (with-current-buffer buffer
      (unless (process-live-p (get-buffer-process buffer))
        (let ((inhibit-read-only t)) (erase-buffer))
        (special-mode)
        (display-line-numbers-mode -1)
        (setq-local truncate-lines t)
        (let ((proc (make-process :name (format "log-%s" name) :buffer buffer :noquery t
                                  :command (list (my/containers-engine) "logs" "-f" "--tail" "500" name)
                                  :connection-type 'pipe)))
          (set-process-filter proc (lambda (proc string)
                                     (when (buffer-live-p (process-buffer proc))
                                       (with-current-buffer (process-buffer proc)
                                         (let ((inhibit-read-only t)
                                               (end (= (point) (point-max))))
                                           (save-excursion (goto-char (point-max)) (insert string))
                                           (when end (goto-char (point-max)))))))))))
    (let ((window (display-buffer buffer (my/containers--below))))
      (with-selected-window window (goto-char (point-max))))))

(defun my/containers-shell ()
  "Open a terminal inside the container (its :shell, else sh)."
  (interactive)
  (let* ((item (my/containers--item))
         (name (my/containers--name item))
         (shell (or (plist-get (my/containers--def item) :shell) '("sh"))))
    (unless (my/containers-running-p name)
      (user-error "%s isn't running (s starts it)" name))
    (let ((buffer (generate-new-buffer (format "*%s shell*" name))))
      (pop-to-buffer buffer (my/containers--below))
      (if (fboundp 'ghostel-exec)
          (ghostel-exec buffer (my/containers-engine) (append (list "exec" "-it" name) shell))
        (make-term (buffer-name buffer) (my/containers-engine) nil "exec" "-it" name (car shell))))))

;;; adding and editing
;; ---------------------------------------------------------------------

(defun my/containers--free-port (port)
  "PORT, or the next one up that nothing listens on."
  (let ((used (with-temp-buffer
                (call-process "ss" nil t nil "-ltnH")
                (let (ports)
                  (goto-char (point-min))
                  (while (re-search-forward ":\\([0-9]+\\)[ \t]" nil t)
                    (push (string-to-number (match-string 1)) ports))
                  ports)))
        (taken (let (ports)
                 (pcase-dolist (`(,_ . ,defs) (my/containers--groups))
                   (dolist (def defs)
                     (dolist (p (plist-get def :ports))
                       (push (string-to-number (car (split-string p ":"))) ports))))
                 ports)))
    (while (or (memq port used) (memq port taken)) (cl-incf port))
    port))

(defun my/containers--template (kind name project)
  "A definition for KIND (\"postgres\", ...) called NAME, for PROJECT's name."
  (let ((db (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase project))))
    (pcase kind
      ("postgres"
       (let ((port (my/containers--free-port 5432)))
         (list :name name :image "docker.io/library/postgres:17"
               :ports (list (format "%d:5432" port))
               :env (list "POSTGRES_USER=dev" "POSTGRES_PASSWORD=dev" (concat "POSTGRES_DB=" db))
               :shell (list "psql" "-U" "dev" "-d" db)
               :reset (format "psql -U dev -d %s -c 'DROP SCHEMA public CASCADE; CREATE SCHEMA public;'" db)
               :url (format "postgres://dev:dev@localhost:%d/%s" port db))))
      ("redis"
       (let ((port (my/containers--free-port 6379)))
         (list :name name :image "docker.io/library/redis:7"
               :ports (list (format "%d:6379" port))
               :shell '("redis-cli")
               :reset "redis-cli FLUSHALL"
               :url (format "redis://localhost:%d" port))))
      ("mongo"
       (let ((port (my/containers--free-port 27017)))
         (list :name name :image "docker.io/library/mongo:8"
               :ports (list (format "%d:27017" port))
               :shell (list "mongosh" db)
               :reset (format "mongosh %s --eval 'db.dropDatabase()'" db)
               :url (format "mongodb://localhost:%d/%s" port db))))
      (_
       (let ((image (read-string "Image (e.g. docker.io/library/nginx:latest): ")))
         (list :name name :image image
               :ports (split-string (read-string "Ports host:container (space separated, optional): "))
               :env (split-string (read-string "Env K=V (space separated, optional): "))))))))

(defun my/containers--write (file defs)
  (with-temp-file file
    (insert ";; -*- mode: lisp-data -*-\n;; containers for the Containers page (SPC d d)\n(")
    (let ((first t))
      (dolist (def defs)
        (unless first (insert "\n "))
        (setq first nil)
        (insert "(")
        (let ((pairs (seq-partition def 2)) (first-pair t))
          (dolist (pair pairs)
            (unless first-pair (insert "\n  "))
            (setq first-pair nil)
            (insert (format "%s %S" (car pair) (cadr pair)))))
        (insert ")")))
    (insert ")\n"))
  (remhash file my/containers--cache))

(defun my/containers--target-file ()
  "Where a new definition goes: the current project's file, or the global one."
  (let ((root (and (fboundp 'my/project-root) (my/project-root))))
    (if (and root (y-or-n-p (format "Add it to project %s? (n: global) "
                                    (file-name-nondirectory (directory-file-name root)))))
        (my/containers--project-file root)
      my/containers-global-file)))

(defun my/containers-add ()
  "Define a new container from a template."
  (interactive)
  (let* ((kind (completing-read "Kind: " '("postgres" "redis" "mongo" "other") nil t))
         (file (my/containers--target-file))
         (project (file-name-nondirectory
                   (directory-file-name (if (equal file my/containers-global-file)
                                            "dev"
                                          (file-name-directory file)))))
         (name (read-string "Container name: " (format "%s-%s" project (if (equal kind "other") "app" kind))))
         (def (my/containers--template kind name project))
         (defs (and (file-exists-p file) (my/containers--read-file file))))
    (when (seq-find (lambda (d) (equal (plist-get d :name) name)) defs)
      (user-error "There's already a %s" name))
    (my/containers--write file (append defs (list def)))
    (message "Added %s to %s (s starts it, E edits it)" name (abbreviate-file-name file))
    (my/containers-refresh)))

(defun my/containers-edit ()
  "Edit the definitions of the container (or group) on this line."
  (interactive)
  (let* ((item (get-text-property (point) 'my/containers-item))
         (root (pcase item (`(def ,_ ,root) root) (`(group ,root) root)
                      (_ (and (fboundp 'my/project-root) (my/project-root)))))
         (file (if root (my/containers--project-file root) my/containers-global-file)))
    (find-file-other-window file)
    (when (and item (eq (car item) 'def))
      (goto-char (point-min))
      (search-forward (format "%S" (plist-get (cadr item) :name)) nil t)
      (beginning-of-line))))

(defun my/containers-quit ()
  "Close the panel, or leave the page."
  (interactive)
  (if (window-parameter (selected-window) 'my-containers-panel)
      (delete-window)
    (quit-window)))

(defun my/containers-keys ()
  (interactive)
  (message "RET log  s start/stop  r restart  R recreate  x reset  e shell  y url  D remove  a add  E edit  g refresh  q close"))

;;; a project's containers, from anywhere
;; ---------------------------------------------------------------------

(defun my/containers--current-defs ()
  (let ((root (and (fboundp 'my/project-root) (my/project-root))))
    (or (and root (my/containers--project-defs root))
        (user-error "This project has no containers (SPC d d, then a)"))))

(defun my/containers-up ()
  "Start every container of the current project."
  (interactive)
  (my/containers-fetch
   (lambda ()
     (dolist (def (my/containers--current-defs))
       (unless (my/containers-running-p (plist-get def :name))
         (my/containers-start def))))))

(defun my/containers-down ()
  "Stop every container of the current project."
  (interactive)
  (dolist (def (my/containers--current-defs))
    (my/containers-stop (plist-get def :name))))

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/containers-mode 'normal)
  (evil-define-key 'normal my/containers-mode-map
    (kbd "RET") #'my/containers-log
    "s" #'my/containers-toggle
    "r" #'my/containers-restart
    "R" #'my/containers-recreate
    "x" #'my/containers-reset
    "e" #'my/containers-shell
    "y" #'my/containers-copy-url
    "D" #'my/containers-remove
    "a" (lambda () (interactive)
          (if (fboundp 'my/with-vertico) (my/with-vertico #'my/containers-add)
            (call-interactively #'my/containers-add)))
    "E" #'my/containers-edit
    "g" #'my/containers-refresh
    "q" #'my/containers-quit
    "?" #'my/containers-keys)
  (evil-define-key 'normal 'global
    (kbd "<leader>dd") #'my/containers-panel-toggle
    (kbd "<leader>dt") #'my/containers
    (kbd "<leader>du") #'my/containers-up
    (kbd "<leader>dk") #'my/containers-down))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC d" "containers")
  (which-key-add-key-based-replacements "SPC d d" "containers panel")
  (which-key-add-key-based-replacements "SPC d t" "containers tab")
  (which-key-add-key-based-replacements "SPC d u" "start project's containers")
  (which-key-add-key-based-replacements "SPC d k" "stop project's containers"))

(provide 'my-containers)

;;; my-containers.el ends here
