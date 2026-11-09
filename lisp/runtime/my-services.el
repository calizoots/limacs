;;; runtime/my-services.el  -*- lexical-binding: t; -*-

;; the Services page: every project's servers in one tab

;; Where the Run panel is one project's processes, this page is all of
;; them: dev servers, APIs and watchers across every known project, with
;; what each one is doing right now (port, uptime, memory, cpu, and an
;; http check for the ones that print a url). The bottom half follows the
;; cursor and shows the log of the service under it.
;;
;; Servers are the run configurations marked :kind server (see my-run.el:
;; npm "dev"/"start"/"serve"/... scripts, or your own in .emacs-run.eld).
;; Ports opened by anything else on the machine are listed at the end.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC r S  open the Services tab
;;
;; On the page:
;;   RET  start the server / open its log / fold the project
;;   TAB  fold or unfold the project     r  start or restart
;;   s    stop                           o  open in the browser
;;   a    start every server in the project
;;   S    stop everything in the project
;;   x    remove a finished process      p  go to the project's tab
;;   e    edit the project's run configs g  refresh   q  close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'my-task)
(require 'my-run)

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function my/project-known-roots "projects")
(declare-function my/project-switch "projects")
(declare-function notifications-notify "notifications")

(defvar my/services-buffer "*Services*")

(defvar my/services-interval 2
  "Seconds between refreshes while the page is on screen.")


(defvar my/services-log-height 0.45
  "Height of the log under the list, as a fraction of the tab.")

(defvar my/services-notify t
  "Non-nil to pop a desktop notification when a server crashes.")

;;; what's running on the machine
;; ---------------------------------------------------------------------
;; one ps and one ss call per refresh. tasks lead their own process
;; group, so a task's pid is the group id of everything it started

(defvar my/services--usage (make-hash-table :test #'eql)
  "Process group -> (rss-kb . cpu-percent), summed over the group.")

(defvar my/services--ports nil
  "Listening ports: list of (port pgid name).")

(defun my/services--collect ()
  "Refresh `my/services--usage' and `my/services--ports'."
  (let ((pid-group (make-hash-table :test #'eql))
        (names (make-hash-table :test #'eql)))
    (clrhash my/services--usage)
    (with-temp-buffer
      (when (zerop (call-process "ps" nil t nil "-e" "-o" "pid=,pgid=,rss=,pcpu=,comm="))
        (goto-char (point-min))
        (while (re-search-forward "^ *\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9.]+\\) +\\(.*\\)$" nil t)
          (let ((pid (string-to-number (match-string 1)))
                (group (string-to-number (match-string 2)))
                (rss (string-to-number (match-string 3)))
                (cpu (string-to-number (match-string 4))))
            (puthash pid group pid-group)
            (puthash pid (match-string 5) names)
            (let ((sum (gethash group my/services--usage '(0 . 0.0))))
              (puthash group (cons (+ (car sum) rss) (+ (cdr sum) cpu))
                       my/services--usage))))))
    (setq my/services--ports nil)
    (with-temp-buffer
      (when (zerop (call-process "ss" nil t nil "-ltnpH"))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (when (string-match "\\S-+ +[0-9]+ +[0-9]+ +\\S-*:\\([0-9]+\\) " line)
              (let* ((port (string-to-number (match-string 1 line)))
                     (pid (and (string-match "pid=\\([0-9]+\\)" line)
                               (string-to-number (match-string 1 line))))
                     (name (and (string-match "((\"\\([^\"]+\\)\"" line) (match-string 1 line))))
                (unless (assq port my/services--ports)
                  (push (list port (and pid (gethash pid pid-group)) name)
                        my/services--ports)))))
          (forward-line 1))))
    (setq my/services--ports (sort my/services--ports (lambda (a b) (< (car a) (car b)))))))

(defun my/services--group (task)
  (when-let ((proc (my/task-process task)))
    (process-id proc)))

(defun my/services--task-ports (task)
  "Ports TASK (or anything it started) is listening on."
  (let ((group (my/services--group task)))
    (and group
         (mapcar #'car (seq-filter (lambda (p) (eql (nth 1 p) group)) my/services--ports)))))

(defun my/services--size (kb)
  (cond ((< kb 1024) (format "%d KB" kb))
        ((< kb (* 1024 1024)) (format "%d MB" (/ kb 1024)))
        (t (format "%.1f GB" (/ kb 1048576.0)))))

;;; http checks
;; ---------------------------------------------------------------------
;; one request when a server first comes up, and again on g. not on a
;; timer: every check is a line in the server's own log, and whether it's
;; still listening is already known from ss every refresh

(defvar my/services--health (make-hash-table :test #'eql)
  "Task id -> (code ms checked-at), or (down nil checked-at).")

(defvar my/services--checking (make-hash-table :test #'eql))

(defun my/services--url (task)
  "TASK's url: the one it printed, else its first port on localhost."
  (or (my/task-url task)
      (when-let ((port (car (my/services--task-ports task))))
        (format "http://localhost:%d" port))))

(defun my/services--check (task &optional again)
  "Ask TASK's url for a response, in the background.
Only once per server, unless AGAIN."
  (let ((id (my/task-id task))
        (url (my/services--url task)))
    (when (and url (not (gethash id my/services--checking))
               (or again
                   (null (gethash id my/services--health))
                   ;; said its url before it was listening: try once it is
                   (and (eq (car (gethash id my/services--health)) 'down)
                        (my/services--task-ports task))))
      (puthash id t my/services--checking)
      (let ((buf (generate-new-buffer " *services-check*")))
        (make-process
         :name "services-check" :buffer buf :noquery t
         :command (list "curl" "-s" "-o" "/dev/null" "-m" "3" "-w" "%{http_code} %{time_total}" url)
         :sentinel (lambda (proc _)
                     (unless (process-live-p proc)
                       (let ((out (with-current-buffer buf (buffer-string))))
                         (kill-buffer buf)
                         (remhash id my/services--checking)
                         (puthash id
                                  (if (string-match "\\`\\([0-9]+\\) \\([0-9.]+\\)" out)
                                      (let ((code (string-to-number (match-string 1 out))))
                                        (if (zerop code)
                                            (list 'down nil (current-time))
                                          (list code (round (* 1000 (string-to-number (match-string 2 out))))
                                                (current-time))))
                                    (list 'down nil (current-time)))
                                  my/services--health)))))))))

(defun my/services--health-text (task)
  (pcase (if (and (gethash (my/task-id task) my/services--health)
                  (null (my/services--task-ports task)))
             ;; it answered before, but nothing of it is listening now
             '(gone)
           (gethash (my/task-id task) my/services--health))
    (`(gone) (propertize "not listening" 'face 'warning))
    ('nil "")
    (`(down . ,_) (propertize "down" 'face 'error))
    (`(,code ,ms . ,_)
     (concat (propertize (number-to-string code)
                         'face (cond ((< code 400) 'success) ((< code 500) 'warning) (t 'error)))
             (propertize (format " · %dms" ms) 'face 'shadow)))))

;;; the page
;; ---------------------------------------------------------------------

(defvar my/services--folded (make-hash-table :test #'equal)
  "Project roots folded by hand: root -> folded-p.")

(defun my/services--roots ()
  "Project roots to show: known projects plus any with processes."
  (let ((roots (mapcar #'file-name-as-directory
                       (if (fboundp 'my/project-known-roots) (my/project-known-roots)
                         (ignore-errors (project-known-project-roots))))))
    (dolist (task (my/task-all))
      (when (my/task-project task)
        (cl-pushnew (file-name-as-directory (my/task-project task)) roots :test #'equal)))
    (delete-dups (mapcar #'expand-file-name roots))))

(defun my/services--servers (root)
  (seq-filter (lambda (spec) (eq (plist-get spec :kind) 'server))
              (ignore-errors (my/run-configs root))))

(defun my/services--folded-p (root tasks)
  "Whether ROOT's section is folded (by hand, else when nothing runs)."
  (let ((set (gethash root my/services--folded 'unset)))
    (if (eq set 'unset) (not (seq-some #'my/task-live-p tasks)) set)))

(defun my/services--task-row (task)
  (pcase-let* ((`(,glyph ,face ,state) (my/run--status task))
               (live (my/task-live-p task))
               (usage (and live (gethash (my/services--group task) my/services--usage)))
               (ports (and live (my/services--task-ports task))))
    (propertize
     (concat "    "
             (propertize glyph 'face face) " "
             (propertize (truncate-string-to-width (my/task-name task) 16 nil ?\s "…") 'face 'bold)
             "  "
             (string-pad (if ports (mapconcat (lambda (p) (format ":%d" p)) ports " ") "") 12)
             (string-pad (if live (my/services--health-text task) (propertize state 'face face)) 14)
             (string-pad (my/task-format-duration (my/task-duration task)) 9 nil t)
             "  "
             (propertize (string-pad (if usage (my/services--size (car usage)) "") 8 nil t) 'face 'shadow)
             (propertize (string-pad (if usage (format "%.1f%%" (cdr usage)) "") 7 nil t) 'face 'shadow)
             "   "
             (if (and live (my/services--url task))
                 (propertize (my/services--url task) 'face 'link)
               (propertize (truncate-string-to-width (my/run--command-string (my/task-command task))
                                                     40 nil nil "…")
                           'face 'shadow))
             "\n")
     'my/services-item (list 'task (my/task-id task)))))

(defun my/services--spec-row (root spec)
  (propertize
   (concat "    "
           (propertize "○" 'face 'shadow) " "
           (propertize (truncate-string-to-width (plist-get spec :name) 16 nil ?\s "…") 'face 'shadow)
           "  "
           (propertize (string-pad "stopped" 26) 'face 'shadow)
           (propertize (my/run--command-string (plist-get spec :command)) 'face 'shadow)
           "\n")
   'my/services-item (list 'spec root (plist-get spec :name))))

(defun my/services--project-section (root)
  "Text for ROOT's section, or nil when it has no servers or processes."
  (let* ((tasks (reverse (my/run--tasks root)))
         (specs (my/services--servers root))
         ;; a config that's running shows as its process instead
         (idle (seq-remove (lambda (spec)
                             (seq-some (lambda (task) (equal (my/task-command task)
                                                             (plist-get spec :command)))
                                       tasks))
                           specs)))
    (when (or tasks specs)
      (let* ((running (seq-count #'my/task-live-p tasks))
             (folded (my/services--folded-p root tasks)))
        (concat
         (propertize
          (concat "  "
                  (propertize (if folded "▸ " "▾ ") 'face 'shadow)
                  (propertize (my/run--project-name root)
                              'face '(:inherit font-lock-function-name-face :weight bold))
                  "  "
                  (propertize (cond ((> running 0) (format "%d running" running))
                                    (specs (format "%d server%s" (length specs)
                                                   (if (= (length specs) 1) "" "s")))
                                    (t ""))
                              'face (if (> running 0) 'success 'shadow))
                  "\n")
          'my/services-item (list 'project root))
         (if folded ""
           (concat (mapconcat #'my/services--task-row tasks "")
                   (mapconcat (lambda (spec) (my/services--spec-row root spec)) idle "")))
         "\n")))))

(defun my/services--ports-section ()
  "Ports something outside Emacs is listening on."
  (let* ((ours (delq nil (mapcar #'my/services--group (seq-filter #'my/task-live-p (my/task-all)))))
         (others (seq-remove (lambda (p) (memq (nth 1 p) ours)) my/services--ports)))
    (concat
     (propertize "  Other listening ports" 'face '(:inherit font-lock-function-name-face :weight bold))
     (propertize (format "  %d\n" (length others)) 'face 'shadow)
     (if (null others)
         (propertize "    none\n" 'face 'shadow)
       (mapconcat (lambda (p)
                    (propertize
                     (concat "    "
                             (propertize (string-pad (format ":%d" (car p)) 9) 'face 'bold)
                             (propertize (or (nth 2 p) "system") 'face 'shadow)
                             "\n")
                     'my/services-item (list 'port (car p))))
                  others "")))))

(defun my/services--render ()
  "Redraw the page, keeping point on the same item."
  (let* ((inhibit-read-only t)
         (item (get-text-property (point) 'my/services-item))
         (line (line-number-at-pos))
         (starts (mapcar (lambda (w) (cons w (window-start w)))
                         (get-buffer-window-list (current-buffer) nil t))))
    (erase-buffer)
    (insert "\n")
    (let ((sections (delq nil (mapcar #'my/services--project-section (my/services--roots)))))
      (if sections
          (apply #'insert sections)
        (insert (propertize "  No servers yet. Mark a run configuration :kind server, or SPC r a\n\n"
                            'face 'shadow))))
    (insert (my/services--ports-section))
    (goto-char (point-min))
    (let ((match (and item (text-property-search-forward 'my/services-item item #'equal))))
      (if match
          (goto-char (prop-match-beginning match))
        (forward-line (1- line))))
    (dolist (start starts)
      (set-window-start (car start) (min (cdr start) (point-max)) t)
      (set-window-point (car start) (point)))))

(defun my/services-refresh (&optional again)
  "Collect fresh numbers and redraw the page (if it's open).
Interactively (g), every server's http check runs again too."
  (interactive (list t))
  (when-let ((buf (get-buffer my/services-buffer)))
    (my/services--collect)
    (dolist (task (my/task-all))
      (when (my/task-live-p task) (my/services--check task again)))
    (with-current-buffer buf (my/services--render))))

(defvar my/services--timer nil)

(defun my/services--tick ()
  (let ((buf (get-buffer my/services-buffer)))
    (if (and buf (get-buffer-window buf t))
        (my/services-refresh)
      (unless buf
        (cancel-timer my/services--timer)
        (setq my/services--timer nil)))))

;;; the log under the list
;; ---------------------------------------------------------------------

(defvar-local my/services--shown nil "Item whose log the lower window shows.")

(defun my/services--log-window ()
  "The window under the list in this tab, made if needed."
  (let ((list-win (get-buffer-window my/services-buffer)))
    (when list-win
      (or (window-in-direction 'below list-win)
          (split-window list-win (- (round (* (window-total-height list-win)
                                               my/services-log-height)))
                        'below)))))

(defun my/services--task (item)
  (and (eq (car-safe item) 'task) (my/task-get (nth 1 item))))

(defun my/services--follow ()
  "Show the log of the service under the cursor in the lower window."
  (let ((item (get-text-property (point) 'my/services-item)))
    (when-let ((task (my/services--task item)))
      (unless (equal item my/services--shown)
        (setq my/services--shown item)
        (when-let ((win (my/services--log-window)))
          (set-window-buffer win (my/task-buffer task))
          (with-selected-window win
            (goto-char (point-max))
            (recenter -1)))))))

;; q in a log under the list goes back up to the list, not to the Run panel
(defun my/services--log-back ()
  (when-let ((list-win (get-buffer-window my/services-buffer)))
    (when (eq (window-in-direction 'below list-win) (selected-window))
      (select-window list-win)
      t)))

(advice-add 'my/run-panel-back :before-until #'my/services--log-back)

;;; commands
;; ---------------------------------------------------------------------

(defun my/services--item ()
  (or (get-text-property (point) 'my/services-item)
      (user-error "Nothing on this line")))

(defun my/services--spec (item)
  "The run configuration behind a (spec ROOT NAME) ITEM."
  (seq-find (lambda (spec) (equal (plist-get spec :name) (nth 2 item)))
            (my/services--servers (nth 1 item))))

(defun my/services--root-at-point ()
  "Project root of the line at point."
  (pcase (my/services--item)
    (`(project ,root) root)
    (`(spec ,root . ,_) root)
    ((and item `(task . ,_)) (my/task-project (my/services--task item)))
    (_ (user-error "Not inside a project"))))

(defun my/services--start (root spec)
  "Start SPEC in ROOT without opening the Run panel."
  (let ((command (plist-get spec :command)))
    (dolist (task (my/run--tasks root))
      (when (equal (my/task-command task) command)
        (if (my/task-live-p task) (my/task-cancel task) (my/task-delete task))))
    (my/task-start :name (plist-get spec :name) :command command
                   :directory root :project root :kind (plist-get spec :kind))))

(defun my/services-toggle-fold ()
  "Fold or unfold the project at point."
  (interactive)
  (let ((root (my/services--root-at-point)))
    (puthash root (not (my/services--folded-p root (my/run--tasks root))) my/services--folded)
    ;; land on the project line, so the fold doesn't leave point inside it
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward 'my/services-item (list 'project root) #'equal)))
      (goto-char (prop-match-beginning match)))
    (my/services--render)))

(defun my/services-open ()
  "Start the server at point, focus its log, or fold the project."
  (interactive)
  (let ((item (my/services--item)))
    (pcase (car item)
      ('project (my/services-toggle-fold))
      ('spec (my/services-run))
      ('port (my/services-browse))
      ('task (my/services--follow)
             (when-let ((win (my/services--log-window))) (select-window win))))))

(defun my/services-run ()
  "Start the server at point, or restart the process at point."
  (interactive)
  (let ((item (my/services--item)))
    (pcase (car item)
      ('spec (let ((spec (or (my/services--spec item) (user-error "That config is gone"))))
               (my/services--start (nth 1 item) spec)
               (message "Started %s" (plist-get spec :name))))
      ('task (let ((new (my/task-restart (my/services--task item))))
               (message "Restarted %s" (my/task-name new))))
      (_ (user-error "Put the cursor on a server")))
    (my/services-refresh)))

(defun my/services-stop ()
  "Stop the process at point."
  (interactive)
  (let ((task (or (my/services--task (my/services--item)) (user-error "Not a process"))))
    (my/task-cancel task)))

(defun my/services-browse ()
  "Open the server (or port) at point in the browser."
  (interactive)
  (let ((item (my/services--item)))
    (browse-url
     (pcase (car item)
       ('port (format "http://localhost:%d" (nth 1 item)))
       ('task (or (my/services--url (my/services--task item))
                  (user-error "It hasn't printed a url or opened a port yet")))
       (_ (user-error "Put the cursor on a running server"))))))

(defun my/services-remove ()
  "Forget the finished process at point."
  (interactive)
  (my/task-delete (or (my/services--task (my/services--item)) (user-error "Not a process"))))

(defun my/services-start-all ()
  "Start every server of the project at point that isn't running."
  (interactive)
  (let* ((root (my/services--root-at-point))
         (live (seq-filter #'my/task-live-p (my/run--tasks root)))
         (todo (seq-remove (lambda (spec)
                             (seq-some (lambda (task) (equal (my/task-command task) (plist-get spec :command)))
                                       live))
                           (my/services--servers root))))
    (unless todo (user-error "Every server in %s is already running" (my/run--project-name root)))
    (dolist (spec todo) (my/services--start root spec))
    (remhash root my/services--folded)
    (message "Started %s" (mapconcat (lambda (s) (plist-get s :name)) todo ", "))
    (my/services-refresh)))

(defun my/services-stop-all ()
  "Stop everything running in the project at point."
  (interactive)
  (let* ((root (my/services--root-at-point))
         (live (seq-filter #'my/task-live-p (my/run--tasks root))))
    (unless live (user-error "Nothing running in %s" (my/run--project-name root)))
    (when (y-or-n-p (format "Stop %d process%s in %s? " (length live)
                            (if (= (length live) 1) "" "es") (my/run--project-name root)))
      (mapc #'my/task-cancel live))))

(defun my/services-goto-project ()
  "Switch to the project at point."
  (interactive)
  (my/project-switch (my/services--root-at-point)))

(defun my/services-edit ()
  "Edit the run configurations of the project at point."
  (interactive)
  (let ((my/run-panel--root (my/services--root-at-point)))
    (other-window 1)
    (my/run-edit)))

;;; crashes
;; ---------------------------------------------------------------------

(defun my/services--on-change (task)
  "Tell you when a server dies on its own, and keep the page current."
  (when (and (eq (my/task-kind task) 'server)
             (eq (my/task-status task) 'failed))
    (let ((text (format "%s%s stopped%s"
                        (my/task-name task)
                        (if (my/task-project task)
                            (format " (%s)" (my/run--project-name (my/task-project task)))
                          "")
                        (if (my/task-exit-code task) (format " with exit code %d" (my/task-exit-code task)) ""))))
      (message "%s" (propertize text 'face 'error))
      (when (and my/services-notify (require 'notifications nil t))
        (ignore-errors (notifications-notify :title "Server stopped" :body text :app-name "Emacs")))))
  (when (get-buffer my/services-buffer)
    (with-current-buffer my/services-buffer (my/services--render))))

(add-hook 'my/task-changed-hook #'my/services--on-change)

;;; the tab
;; ---------------------------------------------------------------------

(define-derived-mode my/services-mode special-mode "Services"
  "Every project's servers, with a live log of the one under the cursor."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (my/services-refresh t)))
  (setq header-line-format
        (concat (propertize " Services" 'face 'bold)
                (propertize "    RET start/log   r restart   s stop   o browser   a start all   S stop all   TAB fold   p project   e configs"
                            'face 'shadow)))
  (add-hook 'post-command-hook #'my/services--follow nil t)
  (hl-line-mode 1))

(defun my/services ()
  "Open the Services tab."
  (interactive)
  (let ((index (seq-position (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs))
                             "Services")))
    (if index
        (tab-bar-select-tab (1+ index))
      (tab-new)
      (tab-bar-rename-tab "Services"))
    (let ((buf (get-buffer-create my/services-buffer)))
      (with-current-buffer buf
        (unless (derived-mode-p 'my/services-mode) (my/services-mode)))
      (unless (get-buffer-window buf)
        (delete-other-windows)
        (switch-to-buffer buf))
      (select-window (get-buffer-window buf))
      (my/services-refresh)
      ;; start on the first running service, so its log shows straight away
      (goto-char (point-min))
      (or (text-property-search-forward 'my/services-item nil
                                        (lambda (_ v) (and (my/services--task v)
                                                           (my/task-live-p (my/services--task v)))))
          (goto-char (point-min)))
      (beginning-of-line)
      (my/services--follow)
      (unless my/services--timer
        (setq my/services--timer (run-at-time my/services-interval my/services-interval
                                              #'my/services--tick))))))

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/services-mode 'normal)
  (evil-define-key 'normal my/services-mode-map
    (kbd "RET") #'my/services-open
    (kbd "TAB") #'my/services-toggle-fold
    (kbd "<tab>") #'my/services-toggle-fold
    "r" #'my/services-run
    "s" #'my/services-stop
    "o" #'my/services-browse
    "x" #'my/services-remove
    "a" #'my/services-start-all
    "S" #'my/services-stop-all
    "p" #'my/services-goto-project
    "e" #'my/services-edit
    "g" #'my/services-refresh
    "q" #'quit-window)
  (evil-define-key 'normal 'global
    (kbd "<leader>rS") #'my/services))

(provide 'my-services)

;;; my-services.el ends here
