;;; runtime/my-services.el  -*- lexical-binding: t; -*-

;; the Services page: every project's processes in one tab

;; The Run panel (my-run.el) shows one project at the bottom of its tab;
;; this page shows every project, with the log of whatever is under the
;; cursor in the bottom half. Both are views of the same tasks, drawn with
;; the same rows (`my/run-task-row') and driven by the same keys, so a
;; dev server looks and behaves the same wherever you meet it. The numbers
;; (ports, memory, cpu, http checks) come from my-task.el.
;;
;; Each project lists its processes (servers, builds, tests: anything run
;; from it) and the servers that aren't running yet. Servers are run
;; configurations marked :kind server (npm "dev"/"start"/"serve"/...
;; scripts, or your own in .emacs-run.eld). Ports opened by anything else
;; on the machine are listed at the end.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC r S  open the Services tab      v  (in a Run panel) the same
;;
;; On the page, the same as in the Run panel:
;;   RET  start the server / open its log / fold the project
;;   r    start or restart               s  stop
;;   o    open in the browser            x  remove a finished process
;;   A    start every server             S  stop everything
;;   X    clear finished processes       a  add a run configuration
;;   e    edit run configurations        g  refresh (and check urls again)
;; and its own:
;;   TAB  fold or unfold the project     p  go to the project (with its panel)
;;   q    close

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'my-task)
(require 'my-run)

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function my/project-known-roots "projects")
(declare-function my/project-switch "projects")

(defvar my/services-buffer "*Services*")

(defvar my/services-interval 2
  "Seconds between refreshes while the page is on screen.")

(defvar my/services-log-height 0.45
  "Height of the log under the list, as a fraction of the tab.")

;;; the page
;; ---------------------------------------------------------------------

(defvar my/services--folded (make-hash-table :test #'equal)
  "Project roots folded or unfolded by hand: root -> folded-p.")

(defun my/services--roots ()
  "Project roots to show: known projects plus any with processes."
  (let ((roots (if (fboundp 'my/project-known-roots) (my/project-known-roots)
                 (ignore-errors (project-known-project-roots)))))
    (dolist (task (my/task-all))
      (when (my/task-project task) (push (my/task-project task) roots)))
    (delete-dups (mapcar (lambda (r) (file-name-as-directory (expand-file-name r))) roots))))

(defun my/services--folded-p (root tasks)
  "Whether ROOT's section is folded (by hand, else when nothing runs)."
  (let ((set (gethash root my/services--folded 'unset)))
    (if (eq set 'unset) (not (seq-some #'my/task-live-p tasks)) set)))

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
         (specs (my/run-servers root))
         ;; a server that's running shows as its process instead
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
           (concat (mapconcat (lambda (task)
                                (propertize (concat "    " (my/run-task-row task))
                                            'my/services-item (list 'task (my/task-id task))))
                              tasks "")
                   (mapconcat (lambda (spec) (my/services--spec-row root spec)) idle "")))
         "\n")))))

(defun my/services--ports-section ()
  "Ports something outside Emacs is listening on."
  (let* ((ours (delq nil (mapcar #'my/task-group (my/task-all))))
         (others (seq-remove (lambda (p) (memq (nth 1 p) ours)) my/task-listening)))
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
Interactively (g), every server's url is checked again too."
  (interactive (list t))
  (when-let ((buf (get-buffer my/services-buffer)))
    (my/task-collect-stats (if again nil 1.5))
    (dolist (task (my/task-all))
      (when (my/task-live-p task) (my/task-check task again)))
    (with-current-buffer buf (my/services--render))))

(defun my/services--on-change (&rest _)
  (when-let ((buf (get-buffer my/services-buffer)))
    (with-current-buffer buf (my/services--render))))

(add-hook 'my/task-changed-hook #'my/services--on-change)

(defvar my/services--timer nil)

(defun my/services--tick ()
  (let ((buf (get-buffer my/services-buffer)))
    (cond ((null buf)
           (cancel-timer my/services--timer)
           (setq my/services--timer nil))
          ((get-buffer-window buf t) (my/services-refresh)))))

;;; the log under the list
;; ---------------------------------------------------------------------

(defvar-local my/services--shown nil "Item whose log the lower window shows.")

(defun my/services--log-window ()
  "The window under the list in this tab, made if needed."
  (when-let ((list-win (get-buffer-window my/services-buffer)))
    (or (window-in-direction 'below list-win)
        (split-window list-win (- (round (* (window-total-height list-win)
                                             my/services-log-height)))
                      'below))))

(defun my/services--task (item)
  (and (eq (car-safe item) 'task) (my/task-get (nth 1 item))))

(defun my/services--follow ()
  "Show the log of the process under the cursor in the lower window."
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
            (my/run-servers (nth 1 item))))

(defun my/services--root-at-point ()
  "Project root of the line at point."
  (pcase (my/services--item)
    (`(project ,root) root)
    (`(spec ,root . ,_) root)
    ((and item `(task . ,_)) (or (my/task-project (my/services--task item))
                                 (user-error "That process has no project")))
    (_ (user-error "Not inside a project"))))

(defmacro my/services--in-project (&rest body)
  "Run BODY as if in the Run panel of the project at point."
  `(let ((my/run-panel--root (my/services--root-at-point)))
     ,@body))

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
               (my/run-start spec (nth 1 item))
               (message "Started %s" (plist-get spec :name))))
      ('task (message "Restarted %s" (my/task-name (my/task-restart (my/services--task item)))))
      (_ (user-error "Put the cursor on a server or process")))
    (my/services-refresh)))

(defun my/services-stop ()
  "Stop the process at point."
  (interactive)
  (my/task-cancel (or (my/services--task (my/services--item)) (user-error "Not a process"))))

(defun my/services-browse ()
  "Open the server (or port) at point in the browser."
  (interactive)
  (let ((item (my/services--item)))
    (browse-url
     (pcase (car item)
       ('port (format "http://localhost:%d" (nth 1 item)))
       ('task (or (my/task-service-url (my/services--task item))
                  (user-error "It hasn't printed a url or opened a port yet")))
       (_ (user-error "Put the cursor on a running server"))))))

(defun my/services-remove ()
  "Forget the finished process at point."
  (interactive)
  (my/task-delete (or (my/services--task (my/services--item)) (user-error "Not a process"))))

(defun my/services-clear ()
  "Forget the finished processes of the project at point."
  (interactive)
  (my/services--in-project (my/run-panel-clear)))

(defun my/services-start-all ()
  "Start every server of the project at point."
  (interactive)
  (let ((root (my/services--root-at-point)))
    (my/run-start-servers root)
    (remhash root my/services--folded)
    (my/services-refresh)))

(defun my/services-stop-all ()
  "Stop everything running in the project at point."
  (interactive)
  (my/run-stop-all (my/services--root-at-point)))

(defun my/services-add ()
  "Add a run configuration to the project at point."
  (interactive)
  (my/services--in-project (my/run-add))
  (my/services-refresh))

(defun my/services-edit ()
  "Edit the run configurations of the project at point."
  (interactive)
  (let ((root (my/services--root-at-point)))
    (other-window 1)
    (let ((my/run-panel--root root)) (my/run-edit))))

(defun my/services-goto-project ()
  "Go to the project at point, with its Run panel open."
  (interactive)
  (let ((root (my/services--root-at-point)))
    (my/project-switch root)
    (my/run-panel-show root nil t)))

;;; the tab
;; ---------------------------------------------------------------------

(define-derived-mode my/services-mode special-mode "Services"
  "Every project's processes, with a live log of the one under the cursor."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (my/services-refresh t)))
  (setq header-line-format
        (concat (propertize " Services" 'face 'bold)
                (propertize "    RET start/log  r restart  s stop  o browser  A start servers  S stop all  a add  TAB fold  p project"
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
      (unless my/services--timer
        (setq my/services--timer (run-at-time my/services-interval my/services-interval
                                              #'my/services--tick))))))

(defun my/services-show-project (root)
  "Open the Services tab with ROOT unfolded and the cursor on it."
  (my/services)
  (puthash root nil my/services--folded)
  (my/services--render)
  (goto-char (point-min))
  (when-let ((match (text-property-search-forward 'my/services-item (list 'project root) #'equal)))
    (goto-char (prop-match-beginning match))
    ;; onto its first process, so its log shows
    (forward-line 1)
    (unless (my/services--task (get-text-property (point) 'my/services-item))
      (forward-line -1)))
  (my/services--follow))

(defun my/services-open-tab ()
  "Open the Services tab, on the first running process."
  (interactive)
  (my/services)
  (goto-char (point-min))
  (or (text-property-search-forward 'my/services-item nil
                                    (lambda (_ v) (when-let ((task (my/services--task v)))
                                                    (my/task-live-p task))))
      (goto-char (point-min)))
  (beginning-of-line)
  (my/services--follow))

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
    "X" #'my/services-clear
    "A" #'my/services-start-all
    "S" #'my/services-stop-all
    "a" #'my/services-add
    "e" #'my/services-edit
    "p" #'my/services-goto-project
    "g" #'my/services-refresh
    "q" #'quit-window)
  (evil-define-key 'normal 'global
    (kbd "<leader>rS") #'my/services-open-tab))

(provide 'my-services)

;;; my-services.el ends here
