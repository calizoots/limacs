;;; runtime/my-run.el  -*- lexical-binding: t; -*-

;; per-project run configurations and Run panel

;; Run configurations are detected from the project root:
;;   package.json scripts (npm / pnpm / yarn / bun), Cargo.toml, build.zig,
;;   go.mod, Makefile targets
;; plus your own, stored in <root>/.emacs-run.eld as a list of plists:
;;   ((:name "dev" :command "npm run dev -- --host" :kind server)
;;    (:name "deploy" :command ("./deploy.sh" "prod")))
;; A string command runs through sh -c; :kind server marks long-running
;; processes (dev servers, watchers).
;;
;; The Run panel is a bottom pane you can open in any tab.  It lists the
;; project's processes and its configurations; RET on a process shows its
;; combined stdout/stderr in the same pane, q goes back to the list.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC r r  run a configuration        SPC r l  rerun the last one
;;   SPC r b  build                      SPC r t  test
;;   SPC r s  stop a running process     SPC r p  toggle the Run panel
;;   SPC r a  add a custom configuration SPC r e  edit .emacs-run.eld
;;
;; Panel and log keys
;; ---------------------------------------------------------------------
;; In the panel:   RET open log / run config   r run or restart   s stop
;;                 o open URL in browser        x remove finished  X clear finished
;;                 A start every server         S stop everything
;;                 a add config   e edit configs   v Services page   g refresh   q close
;;
;; The panel and the Services page (my-services.el) are two views of the
;; same processes: same rows, same keys. The panel is one project at the
;; bottom of a tab, Services is every project in a tab of its own.
;; In a log:       q / DEL back to list   r restart   s stop   o open URL

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'my-task)

(defvar vertico-multiform-commands)
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function my/project-root "projects")
(declare-function my/with-vertico "my-vertico")
(declare-function my/services-show-project "my-services")
(declare-function notifications-notify "notifications")

;;; project root
;; ---------------------------------------------------------------------

(defvar-local my/run-panel--root nil
  "Project root shown by this Run panel or log buffer.")

(defun my/run--root ()
  "Root of the current project: the tab's project, else project.el, else cwd."
  (file-name-as-directory
   (expand-file-name
    (or my/run-panel--root
        (and (fboundp 'my/project-root) (my/project-root))
        (when-let ((p (project-current))) (project-root p))
        default-directory))))

(defun my/run--project-name (root)
  (file-name-nondirectory (directory-file-name root)))

(defun my/run--tasks (root)
  "Tasks of ROOT, newest first."
  (seq-filter (lambda (task) (equal (my/task-project task) root)) (my/task-all)))

(defun my/run-running-count (root)
  "Number of running tasks in project ROOT."
  (seq-count #'my/task-live-p (my/run--tasks (file-name-as-directory root))))

;;; detecting configurations
;; ---------------------------------------------------------------------

(defvar my/run-config-file ".emacs-run.eld"
  "Per-project file holding custom run configurations.")

(defvar my/run-server-name-regexp
  "\\`\\(dev\\|start\\|serve\\|server\\|preview\\|watch\\)\\([:_-]\\|\\'\\)"
  "Script names matching this are treated as long-running servers.")

(defun my/run--file (root name)
  (let ((f (expand-file-name name root)))
    (and (file-exists-p f) f)))

(defun my/run--spec (name command source &optional kind)
  (list :name name :command command :source source
        :kind (or kind (if (string-match-p my/run-server-name-regexp name) 'server 'task))))

(defun my/run--js-runner (root)
  (cond ((or (my/run--file root "bun.lockb") (my/run--file root "bun.lock")) "bun")
        ((my/run--file root "pnpm-lock.yaml") "pnpm")
        ((my/run--file root "yarn.lock") "yarn")
        (t "npm")))

(defun my/run--detect-node (root)
  (when-let* ((file (my/run--file root "package.json"))
              (json (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist)))))
    (let ((runner (my/run--js-runner root)))
      (mapcar (lambda (script)
                (let ((name (symbol-name (car script))))
                  (my/run--spec name (list runner "run" name) runner)))
              (alist-get 'scripts json)))))

(defun my/run--detect-cargo (root)
  (when (my/run--file root "Cargo.toml")
    (mapcar (lambda (cmd) (my/run--spec cmd (list "cargo" cmd) "cargo" 'task))
            '("run" "build" "test" "check" "clippy"))))

(defun my/run--detect-zig (root)
  (when (my/run--file root "build.zig")
    (list (my/run--spec "build" '("zig" "build") "zig" 'task)
          (my/run--spec "run" '("zig" "build" "run") "zig" 'task)
          (my/run--spec "test" '("zig" "build" "test") "zig" 'task))))

(defun my/run--detect-go (root)
  (when (my/run--file root "go.mod")
    (list (my/run--spec "run" '("go" "run" ".") "go" 'task)
          (my/run--spec "build" '("go" "build" "./...") "go" 'task)
          (my/run--spec "test" '("go" "test" "./...") "go" 'task))))

(defun my/run--detect-make (root)
  (when-let ((file (or (my/run--file root "Makefile") (my/run--file root "makefile"))))
    (let (targets)
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^\\([A-Za-z0-9][A-Za-z0-9_./-]*\\)[ \t]*:\\([^=]\\|$\\)" nil t)
          (push (match-string 1) targets)))
      (mapcar (lambda (target) (my/run--spec target (list "make" target) "make"))
              (delete-dups (nreverse targets))))))

(defun my/run--custom-configs (root)
  (when-let ((file (my/run--file root my/run-config-file)))
    (condition-case err
        (mapcar (lambda (c)
                  (let ((cmd (plist-get c :command)))
                    (my/run--spec (plist-get c :name)
                                  (if (stringp cmd) (list "sh" "-c" cmd) cmd)
                                  "custom"
                                  (plist-get c :kind))))
                (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer))))
      (error (message "my-run: could not read %s: %s" file (error-message-string err))
             nil))))

(defvar my/run-detectors
  '(my/run--custom-configs my/run--detect-node my/run--detect-cargo
    my/run--detect-zig my/run--detect-go my/run--detect-make)
  "Functions returning run configurations for a project root.")

(defun my/run-configs (root)
  "All run configurations for project ROOT."
  (apply #'append (mapcar (lambda (fn) (funcall fn root)) my/run-detectors)))

(defun my/run--command-string (command)
  (if (and (equal (car command) "sh") (equal (cadr command) "-c"))
      (nth 2 command)
    (mapconcat #'identity command " ")))

;;; running
;; ---------------------------------------------------------------------

(defvar my/run--last (make-hash-table :test #'equal)
  "Last configuration run in each project root.")

(defun my/run-start (spec root)
  "Start configuration SPEC in ROOT (in the background) and return the task.
One instance per configuration: an old one is stopped, old logs dropped."
  (let ((command (plist-get spec :command)))
    (dolist (task (my/run--tasks root))
      (when (equal (my/task-command task) command)
        (if (my/task-live-p task)
            (my/task-cancel task)
          (my/task-delete task))))
    (my/task-start :name (plist-get spec :name)
                   :command command
                   :directory root
                   :project root
                   :kind (plist-get spec :kind))))

(defun my/run-spec (spec root)
  "Run configuration SPEC in ROOT and show it in the Run panel."
  (puthash root spec my/run--last)
  (let ((task (my/run-start spec root)))
    (my/run-panel-show root task)
    task))

(defun my/run-servers (root)
  "ROOT's configurations marked as servers."
  (seq-filter (lambda (spec) (eq (plist-get spec :kind) 'server))
              (ignore-errors (my/run-configs root))))

(defun my/run-start-servers (root)
  "Start every server of ROOT that isn't running.  Return the new tasks."
  (let* ((live (seq-filter #'my/task-live-p (my/run--tasks root)))
         (todo (seq-remove (lambda (spec)
                             (seq-some (lambda (task) (equal (my/task-command task)
                                                             (plist-get spec :command)))
                                       live))
                           (my/run-servers root))))
    (unless todo
      (user-error (if (my/run-servers root)
                      "Every server in %s is already running"
                    "%s has no servers (mark a config :kind server, a to add one)")
                  (my/run--project-name root)))
    (prog1 (mapcar (lambda (spec) (my/run-start spec root)) todo)
      (message "Started %s" (mapconcat (lambda (s) (plist-get s :name)) todo ", ")))))

(defun my/run-stop-all (root)
  "Stop everything running in ROOT, after asking."
  (let ((live (seq-filter #'my/task-live-p (my/run--tasks root))))
    (unless live (user-error "Nothing running in %s" (my/run--project-name root)))
    (when (y-or-n-p (format "Stop %d process%s in %s? " (length live)
                            (if (= (length live) 1) "" "es") (my/run--project-name root)))
      (mapc #'my/task-cancel live))))

(defun my/run--spec-label (spec)
  (concat (string-pad (plist-get spec :name) 20)
          (propertize (format " %-7s %s%s"
                              (plist-get spec :source)
                              (my/run--command-string (plist-get spec :command))
                              (if (eq (plist-get spec :kind) 'server) "  [server]" ""))
                      'face 'shadow)))

(defun my/run--read-spec (root prompt &optional specs)
  (let* ((specs (or specs (my/run-configs root)))
         (table (mapcar (lambda (s) (cons (my/run--spec-label s) s)) specs)))
    (unless table
      (user-error "No run configurations in %s (SPC r a to add one)"
                  (abbreviate-file-name root)))
    (if (= (length table) 1)
        (cdar table)
      (cdr (assoc (completing-read prompt table nil t) table)))))

(defun my/run ()
  "Pick a run configuration of the current project and run it."
  (interactive)
  (let ((root (my/run--root)))
    (my/run-spec (my/run--read-spec root (format "Run [%s]: " (my/run--project-name root)))
                 root)))

(defun my/run-last ()
  "Rerun the last configuration of the current project."
  (interactive)
  (let* ((root (my/run--root))
         (spec (gethash root my/run--last)))
    (if spec (my/run-spec spec root) (my/run))))

(defun my/run--named (regexp label)
  (let* ((root (my/run--root))
         (all (my/run-configs root))
         (matches (seq-filter (lambda (s) (string-match-p regexp (plist-get s :name))) all)))
    (my/run-spec (my/run--read-spec root (format "%s [%s]: " label (my/run--project-name root))
                                    (or matches all))
                 root)))

(defun my/run-build ()
  "Run the project's build configuration."
  (interactive)
  (my/run--named "\\`build\\b" "Build"))

(defun my/run-test ()
  "Run the project's test configuration."
  (interactive)
  (my/run--named "\\`test\\b" "Test"))

(defun my/run-stop ()
  "Stop a running process of the current project."
  (interactive)
  (let* ((root (my/run--root))
         (live (seq-filter #'my/task-live-p (my/run--tasks root))))
    (pcase live
      ('nil (user-error "Nothing running in %s" (my/run--project-name root)))
      (`(,task) (my/task-cancel task))
      (_ (let ((table (mapcar (lambda (task)
                                (cons (format "%s  %s" (my/task-name task)
                                              (my/run--command-string (my/task-command task)))
                                      task))
                              live)))
           (my/task-cancel (cdr (assoc (completing-read "Stop: " table nil t) table))))))))

(defun my/run-add ()
  "Add a custom run configuration to the current project."
  (interactive)
  (let* ((root (my/run--root))
         (file (expand-file-name my/run-config-file root))
         (name (read-string "Name: "))
         (command (read-shell-command "Command: "))
         (server (y-or-n-p "Long-running (server/watcher)? "))
         (configs (and (file-exists-p file)
                       (with-temp-buffer (insert-file-contents file) (read (current-buffer))))))
    (setq configs (append (seq-remove (lambda (c) (equal (plist-get c :name) name)) configs)
                          (list (append (list :name name :command command)
                                        (and server (list :kind 'server))))))
    (with-temp-file file
      (insert ";; Run configurations for this project (see lisp/runtime/my-run.el)\n")
      (pp configs (current-buffer)))
    (my/run-panel--refresh-all)
    (message "Added %s to %s" name (abbreviate-file-name file))))

(defun my/run-edit ()
  "Edit the current project's custom run configurations."
  (interactive)
  (let ((file (expand-file-name my/run-config-file (my/run--root))))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert ";; Run configurations for this project.\n"
                ";; :command is a string (run with sh -c) or a list of strings.\n"
                ";; :kind server marks long-running processes.\n"
                "(\n ;; (:name \"dev\" :command \"npm run dev\" :kind server)\n)\n")))
    (when-let ((win (my/run-panel--window)))
      (when (eq win (selected-window)) (other-window -1)))
    (find-file file)))

;;; run panel
;; ---------------------------------------------------------------------

(defvar my/run-panel-height 0.32
  "Height of the Run panel, as a fraction of the frame.")

;; keep the panel marker when tabs save and restore their window layouts
(add-to-list 'window-persistent-parameters '(my-run-panel . writable))

(defun my/run-panel--buffer (root)
  (let ((buf (get-buffer-create (format "*Run · %s*" (my/run--project-name root)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/run-panel-mode)
        (my/run-panel-mode))
      (setq my/run-panel--root root
            default-directory root)
      (my/run-panel--render))
    buf))

(defun my/run-panel--window ()
  "The Run panel window in the selected frame, if any."
  (seq-find (lambda (w) (window-parameter w 'my-run-panel))
            (window-list nil 'nomini)))

(defun my/run-panel--display (buffer)
  "Show BUFFER in the Run panel window, creating it if needed."
  (let ((win (my/run-panel--window)))
    (if (window-live-p win)
        (set-window-buffer win buffer)
      (setq win (display-buffer-in-side-window
                 buffer `((side . bottom) (slot . 0)
                          (window-height . ,my/run-panel-height)
                          (preserve-size . (nil . t)))))
      (set-window-parameter win 'my-run-panel t)
      (set-window-parameter win 'no-delete-other-windows t))
    win))

(defun my/run-panel-show (root &optional task select)
  "Open ROOT's Run panel, showing TASK's log if given.  SELECT focuses it."
  (let* ((buf (if task (my/task-buffer task) (my/run-panel--buffer root)))
         (win (progn
                (when task
                  (my/run-panel--buffer root)
                  (with-current-buffer buf (setq my/run-panel--root root)))
                (my/run-panel--display buf))))
    (when task
      (my/run--show-tail win))
    (when select (select-window win))
    win))

(defun my/run--show-tail (win)
  "Put WIN at the end of its log, with the window filled by the latest output."
  (with-selected-window win
    (goto-char (point-max))
    (recenter -1)))

(defun my/run-panel-toggle ()
  "Show or hide the Run panel in this tab."
  (interactive)
  (let ((win (my/run-panel--window)))
    (if (window-live-p win)
        (delete-window win)
      (my/run-panel-show (my/run--root) nil t))))

(defun my/run-panel-close ()
  (interactive)
  (let ((win (my/run-panel--window)))
    (if (and win (eq win (selected-window)))
        (delete-window win)
      (quit-window))))

(defun my/run-panel-back ()
  "From a log, go back to the process list."
  (interactive)
  (let ((root (or my/run-panel--root (my/run--root))))
    (set-window-buffer (selected-window) (my/run-panel--buffer root))))

;; --- rendering ---

(defun my/run--status (task)
  (pcase (my/task-status task)
    ('running   (list "●" 'success "running"))
    ('succeeded (list "✓" 'success "done"))
    ('cancelled (list "■" 'shadow "stopped"))
    ('pending   (list "○" 'shadow "starting"))
    (_ (list "✗" 'error (if (my/task-exit-code task)
                            (format "exit %d" (my/task-exit-code task))
                          "failed")))))

(defun my/run--health-text (task)
  (pcase (my/task-health task)
    ('nil "")
    (`(gone) (propertize "not listening" 'face 'warning))
    (`(down) (propertize "down" 'face 'error))
    (`(,code ,ms)
     (concat (propertize (number-to-string code)
                         'face (cond ((< code 400) 'success) ((< code 500) 'warning) (t 'error)))
             (propertize (format " · %dms" ms) 'face 'shadow)))))

(defun my/run-task-row (task)
  "One line for TASK, the same in the Run panel and on the Services page:
status, name, ports, http check (or state), uptime, memory, cpu, then
the url (or the command)."
  (pcase-let* ((`(,glyph ,face ,state) (my/run--status task))
               (live (my/task-live-p task))
               (usage (my/task-usage task))
               (ports (my/task-ports task))
               (url (and live (my/task-service-url task)))
               (health (my/run--health-text task)))
    (concat (propertize glyph 'face face) " "
            (propertize (truncate-string-to-width (my/task-name task) 16 nil ?\s "…") 'face 'bold)
            "  "
            (string-pad (if ports (mapconcat (lambda (p) (format ":%d" p)) ports " ") "") 12)
            (string-pad (if (and live (not (string-empty-p health))) health
                          (propertize state 'face face))
                        14)
            (string-pad (my/task-format-duration (my/task-duration task)) 9 nil t)
            "  "
            (propertize (string-pad (if usage (my/task-format-size (car usage)) "") 8 nil t) 'face 'shadow)
            (propertize (string-pad (if usage (format "%.1f%%" (cdr usage)) "") 7 nil t) 'face 'shadow)
            "   "
            (if url
                (propertize url 'face 'link)
              (propertize (truncate-string-to-width
                           (my/run--command-string (my/task-command task)) 40 nil nil "…")
                          'face 'shadow))
            "\n")))

(defun my/run-panel--task-row (task)
  (propertize (concat "  " (my/run-task-row task)) 'my-run-item task))

(defun my/run-panel--spec-row (spec)
  (propertize
   (concat "  "
           (propertize "▶" 'face 'success) " "
           (propertize (truncate-string-to-width (plist-get spec :name) 18 nil ?\s "…")
                       'face 'bold)
           "  "
           (propertize (truncate-string-to-width
                        (my/run--command-string (plist-get spec :command)) 34 nil ?\s "…")
                       'face 'shadow)
           "  "
           (propertize (format "%-7s" (plist-get spec :source)) 'face 'shadow)
           (if (eq (plist-get spec :kind) 'server)
               (propertize "  server" 'face 'font-lock-keyword-face)
             "")
           "\n")
   'my-run-item spec))

(defun my/run-panel--heading (title &optional count)
  (concat (propertize title 'face '(:inherit font-lock-function-name-face :weight bold))
          (if count (propertize (format "  %d" count) 'face 'shadow) "")
          "\n"))

(defun my/run-panel--render ()
  "Redraw the Run panel in the current buffer, keeping point on its item."
  (let* ((root my/run-panel--root)
         (item (get-text-property (point) 'my-run-item))
         (line (line-number-at-pos))
         (tasks (my/run--tasks root))
         (running (seq-filter #'my/task-live-p tasks))
         (finished (seq-remove #'my/task-live-p tasks))
         (configs (my/run-configs root))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (my/run-panel--heading "Running" (length running)))
    (if running
        (mapc (lambda (task) (insert (my/run-panel--task-row task))) running)
      (insert (propertize "  nothing running\n" 'face 'shadow)))
    (when finished
      (insert "\n" (my/run-panel--heading "Finished" (length finished)))
      (mapc (lambda (task) (insert (my/run-panel--task-row task))) finished))
    (insert "\n" (my/run-panel--heading "Configurations" (length configs)))
    (if configs
        (mapc (lambda (spec) (insert (my/run-panel--spec-row spec))) configs)
      (insert (propertize "  none detected — press a to add one\n" 'face 'shadow)))
    ;; put point back on the same item, else the same line
    (goto-char (point-min))
    (let ((match (and item (text-property-search-forward 'my-run-item item #'eq))))
      (if match
          (goto-char (prop-match-beginning match))
        (goto-char (point-min))
        (forward-line (1- line))))
    (dolist (w (get-buffer-window-list (current-buffer) nil t))
      (set-window-point w (point)))))

(defun my/run-panel--refresh-all (&rest _)
  "Redraw every Run panel, and the tab bar (running-process dots)."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (eq (buffer-local-value 'major-mode buf) 'my/run-panel-mode))
      (with-current-buffer buf
        (my/run-panel--render))))
  (force-mode-line-update t))

(add-hook 'my/task-changed-hook #'my/run-panel--refresh-all)

(defvar my/run-notify t
  "Non-nil to pop a desktop notification when a server crashes.")

(defvar my/run--told (make-hash-table :test #'eql)
  "Tasks whose crash was already reported.")

(defun my/run--on-crash (task)
  "Tell you when a server dies on its own (stopping it yourself is quiet)."
  (when (and (eq (my/task-kind task) 'server)
             (eq (my/task-status task) 'failed)
             (not (gethash (my/task-id task) my/run--told)))
    (puthash (my/task-id task) t my/run--told)
    (let ((text (format "%s%s stopped%s"
                        (my/task-name task)
                        (if (my/task-project task)
                            (format " (%s)" (my/run--project-name (my/task-project task)))
                          "")
                        (if (my/task-exit-code task)
                            (format " with exit code %d" (my/task-exit-code task))
                          ""))))
      (message "%s" (propertize text 'face 'error))
      (when (and my/run-notify (require 'notifications nil t))
        (ignore-errors (notifications-notify :title "Server stopped" :body text :app-name "Emacs"))))))

(add-hook 'my/task-changed-hook #'my/run--on-crash)

;; tick once a second while something runs, so uptimes stay current
(defvar my/run--timer nil)

(defun my/run--views-visible-p ()
  "Non-nil when a Run panel or the Services page is on screen."
  (seq-some (lambda (buf)
              (and (memq (buffer-local-value 'major-mode buf) '(my/run-panel-mode my/services-mode))
                   (get-buffer-window buf t)))
            (buffer-list)))

(defun my/run--tick ()
  (when (and (seq-some #'my/task-live-p (my/task-all)) (my/run--views-visible-p))
    (my/task-collect-stats 1.5)
    (dolist (task (my/task-all))
      (when (my/task-live-p task) (my/task-check task))))
  (when (seq-some #'my/task-live-p (my/task-all))
    (dolist (buf (buffer-list))
      (when (and (eq (buffer-local-value 'major-mode buf) 'my/run-panel-mode)
                 (get-buffer-window buf t))
        (with-current-buffer buf (my/run-panel--render))))
    (force-mode-line-update t)))
(unless my/run--timer
  (setq my/run--timer (run-at-time 1 1 #'my/run--tick)))

;; --- panel commands ---

(defun my/run-panel--item ()
  (or (get-text-property (point) 'my-run-item)
      (user-error "Nothing on this line")))

(defun my/run-panel--task-at-point ()
  (let ((item (my/run-panel--item)))
    (if (my/task-p item) item (user-error "Not a process"))))

(defun my/run-panel-open ()
  "Open the log of the process at point, or run the configuration at point."
  (interactive)
  (let ((item (my/run-panel--item)))
    (if (my/task-p item)
        (progn
          (with-current-buffer (my/task-buffer item)
            (setq my/run-panel--root (my/task-project item)))
          (set-window-buffer (selected-window) (my/task-buffer item))
          (my/run--show-tail (selected-window)))
      (my/run-spec item my/run-panel--root))))

(defun my/run-panel-run ()
  "Run the configuration at point, or restart the process at point."
  (interactive)
  (let ((item (my/run-panel--item)))
    (if (my/task-p item)
        (my/run-panel-show (my/task-project item) (my/task-restart item))
      (my/run-spec item my/run-panel--root))))

(defun my/run-panel-stop ()
  (interactive)
  (my/task-cancel (my/run-panel--task-at-point)))

(defun my/run-panel-browse ()
  (interactive)
  (let ((url (my/task-url (my/run-panel--task-at-point))))
    (if url (browse-url url) (user-error "No URL seen in this process's output"))))

(defun my/run-panel-remove ()
  "Remove the finished process at point."
  (interactive)
  (my/task-delete (my/run-panel--task-at-point)))

(defun my/run-panel-start-servers ()
  "Start every server of this project."
  (interactive)
  (my/run-start-servers my/run-panel--root))

(defun my/run-panel-stop-all ()
  "Stop everything running in this project."
  (interactive)
  (my/run-stop-all my/run-panel--root))

(defun my/run-panel-services ()
  "Open the Services page at this project."
  (interactive)
  (my/services-show-project my/run-panel--root))

(defun my/run-panel-clear ()
  "Remove all finished processes of this project."
  (interactive)
  (mapc #'my/task-delete
        (seq-remove #'my/task-live-p (my/run--tasks my/run-panel--root))))

(define-derived-mode my/run-panel-mode special-mode "Run"
  "Processes and run configurations of a project."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (my/run-panel--render)))
  (setq header-line-format
        '(:eval (concat
                 (propertize (format " Run · %s" (my/run--project-name my/run-panel--root))
                             'face 'bold)
                 (propertize "    RET open  r run/restart  s stop  o browser  A start servers  S stop all  x remove  a add  v services  q close"
                             'face 'shadow))))
  (hl-line-mode 1))

;; --- log commands (my/task-log-mode, shown in the panel) ---

(defun my/run--log-task ()
  (or (and my/task--id (my/task-get my/task--id))
      (user-error "This log has no task")))

(defun my/run-log-restart ()
  (interactive)
  (let ((task (my/run--log-task)))
    (my/run-panel-show (my/task-project task) (my/task-restart task))))

(defun my/run-log-stop ()
  (interactive)
  (my/task-cancel (my/run--log-task)))

(defun my/run-log-browse ()
  (interactive)
  (let ((url (my/task-url (my/run--log-task))))
    (if url (browse-url url) (user-error "No URL seen in the output"))))

(defun my/run--log-header ()
  (when-let ((task (and my/task--id (my/task-get my/task--id))))
    (pcase-let ((`(,glyph ,face ,state) (my/run--status task)))
      (concat (propertize " ← q " 'face 'shadow)
              (propertize glyph 'face face) " "
              (propertize (my/task-name task) 'face 'bold) "  "
              (propertize state 'face face) "  "
              (propertize (my/task-format-duration (my/task-duration task)) 'face 'shadow)
              (if (my/task-url task) (concat "  " (propertize (my/task-url task) 'face 'link)) "")
              (propertize "    r restart  s stop  o browser" 'face 'shadow)))))

(add-hook 'my/task-log-mode-hook
          (lambda () (setq header-line-format '(:eval (my/run--log-header)))))

;;; keys
;; ---------------------------------------------------------------------

(let ((map my/run-panel-mode-map))
  (define-key map (kbd "RET") #'my/run-panel-open)
  (define-key map (kbd "r") #'my/run-panel-run)
  (define-key map (kbd "s") #'my/run-panel-stop)
  (define-key map (kbd "o") #'my/run-panel-browse)
  (define-key map (kbd "x") #'my/run-panel-remove)
  (define-key map (kbd "X") #'my/run-panel-clear)
  (define-key map (kbd "A") #'my/run-panel-start-servers)
  (define-key map (kbd "S") #'my/run-panel-stop-all)
  (define-key map (kbd "v") #'my/run-panel-services)
  (define-key map (kbd "a") #'my/run-add)
  (define-key map (kbd "e") #'my/run-edit)
  (define-key map (kbd "g") #'revert-buffer)
  (define-key map (kbd "q") #'my/run-panel-close))

(let ((map my/task-log-mode-map))
  (define-key map (kbd "q") #'my/run-panel-back)
  (define-key map (kbd "DEL") #'my/run-panel-back)
  (define-key map (kbd "r") #'my/run-log-restart)
  (define-key map (kbd "s") #'my/run-log-stop)
  (define-key map (kbd "o") #'my/run-log-browse))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/run-panel-mode 'normal)
  (evil-set-initial-state 'my/task-log-mode 'normal)

  (evil-define-key 'normal my/run-panel-mode-map
    (kbd "RET") #'my/run-panel-open
    (kbd "r") #'my/run-panel-run
    (kbd "s") #'my/run-panel-stop
    (kbd "o") #'my/run-panel-browse
    (kbd "x") #'my/run-panel-remove
    (kbd "X") #'my/run-panel-clear
    (kbd "A") #'my/run-panel-start-servers
    (kbd "S") #'my/run-panel-stop-all
    (kbd "v") #'my/run-panel-services
    (kbd "a") #'my/run-add
    (kbd "e") #'my/run-edit
    (kbd "g") #'revert-buffer
    (kbd "q") #'my/run-panel-close)

  (evil-define-key 'normal my/task-log-mode-map
    (kbd "q") #'my/run-panel-back
    (kbd "DEL") #'my/run-panel-back
    (kbd "r") #'my/run-log-restart
    (kbd "s") #'my/run-log-stop
    (kbd "o") #'my/run-log-browse)

  (evil-define-key 'normal 'global
    (kbd "<leader>rr") (lambda () (interactive)
                         (if (fboundp 'my/with-vertico) (my/with-vertico #'my/run) (my/run)))
    (kbd "<leader>rl") #'my/run-last
    (kbd "<leader>rb") #'my/run-build
    (kbd "<leader>rt") #'my/run-test
    (kbd "<leader>rs") #'my/run-stop
    (kbd "<leader>rp") #'my/run-panel-toggle
    (kbd "<leader>ra") #'my/run-add
    (kbd "<leader>re") #'my/run-edit))

(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-commands '(my/run posframe)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC r" "run"))

(provide 'my-run)

;;; my-run.el ends here
