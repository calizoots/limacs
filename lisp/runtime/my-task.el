;;; runtime/my-task.el  -*- lexical-binding: t; -*-

;; background processes with a combined log

;; A task is one execution of a command.  Its stdout and stderr are written
;; to a single log buffer (stderr in red); stderr also goes to its own
;; buffer.  Tasks run in their own process group, so stopping one also
;; stops whatever it spawned (npm -> node, cargo -> binary, ...).

(require 'cl-lib)
(require 'seq)
(require 'compile)
(require 'ansi-color)

;;; State

(cl-defstruct (my/task (:constructor my/task--create))
  id
  name
  command
  directory
  process
  buffer          ; combined stdout + stderr log
  error-buffer    ; stderr only
  status          ; pending running succeeded failed cancelled
  exit-code
  started-at
  finished-at
  on-success
  on-failure
  project         ; project root this task belongs to, or nil
  kind            ; `task' (runs and exits) or `server' (long-running)
  url)            ; first local URL seen in the output

(defvar my/task--next-id 0
  "Next task execution ID.")

(defvar my/task--registry (make-hash-table :test #'eql)
  "Registry of task executions, indexed by ID.")

(defvar my/task-changed-hook nil
  "Hook run with one argument, the task, whenever a task's status changes.")

(defvar my/task-stop-timeout 3
  "Seconds to wait after SIGTERM before killing a task with SIGKILL.")

(defvar my/task-environment
  '("FORCE_COLOR=1" "CLICOLOR_FORCE=1" "CARGO_TERM_COLOR=always" "TERM=xterm-256color")
  "Extra environment for tasks, so tools keep their colors.")

(defface my/task-stderr '((t :inherit error :weight normal))
  "Face for stderr lines in a task log.")

(defface my/task-meta '((t :inherit shadow :slant italic))
  "Face for the header and footer lines of a task log.")

;;; Internal helpers

(defun my/task--changed (task)
  (run-hook-with-args 'my/task-changed-hook task))

(defun my/task--set-status (task status)
  "Set TASK's STATUS."
  (setf (my/task-status task) status)
  (my/task--changed task))

(defun my/task-duration (task)
  "Seconds TASK has been running, or ran for."
  (when (my/task-started-at task)
    (float-time (time-subtract (or (my/task-finished-at task) (current-time))
                               (my/task-started-at task)))))

(defun my/task-format-duration (seconds)
  (cond ((null seconds) "")
        ((< seconds 10) (format "%.1fs" seconds))
        ((< seconds 60) (format "%ds" seconds))
        ((< seconds 3600) (format "%dm %02ds" (/ seconds 60) (mod (round seconds) 60)))
        (t (format "%dh %02dm" (/ seconds 3600) (/ (mod (round seconds) 3600) 60)))))

(defvar-local my/task--stderr-ansi nil
  "ANSI color state for the stderr stream of this log buffer.")

(defconst my/task--url-regexp
  "https?://\\(?:localhost\\|127\\.0\\.0\\.1\\|0\\.0\\.0\\.0\\|\\[::1?\\]\\|[a-z0-9.-]+\\.local\\)\\(?::[0-9]+\\)?[^][ \t\n\"'<>]*")

(defun my/task--insert (task string &optional face)
  "Append STRING to TASK's log, optionally with FACE.
Windows (and the buffer's own point) that were at the end keep following."
  (let ((buf (my/task-buffer task)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (string (replace-regexp-in-string "\r\n" "\n" string))
               (string (replace-regexp-in-string "\r" "\n" string))
               (text (if (eq face 'my/task-stderr)
                         (let ((ansi-color-context my/task--stderr-ansi))
                           (prog1 (ansi-color-apply string)
                             (setq my/task--stderr-ansi ansi-color-context)))
                       (ansi-color-apply string)))
               (at-end (= (point) (point-max)))
               (following (seq-filter (lambda (w) (= (window-point w) (point-max)))
                                      (get-buffer-window-list buf nil t))))
          (when face
            (add-face-text-property 0 (length text) face t text))
          (save-excursion
            (goto-char (point-max))
            (insert text))
          (when at-end (goto-char (point-max)))
          (dolist (w following) (set-window-point w (point-max)))
          (unless (or face (my/task-url task))
            (let ((plain (substring-no-properties text)))
              (when (string-match my/task--url-regexp plain)
                (setf (my/task-url task)
                      (replace-regexp-in-string "[.,;:)]+\\'" "" (match-string 0 plain)))
                (my/task--changed task)))))))))

(defun my/task--finish (task status exit-code)
  "Finalize TASK with STATUS and EXIT-CODE."
  (unless (memq (my/task-status task)
                '(succeeded failed cancelled))
    (setf (my/task-status task) status
          (my/task-exit-code task) exit-code
          (my/task-finished-at task) (current-time))

    (my/task--insert
     task
     (format "\n%s · %s\n"
             (pcase status
               ('succeeded "✓ finished")
               ('cancelled "■ stopped")
               (_ (if exit-code (format "✗ exited with code %d" exit-code) "✗ failed")))
             (my/task-format-duration (my/task-duration task)))
     'my/task-meta)
    (my/task--changed task)

    (let ((callback
           (if (eq status 'succeeded)
               (my/task-on-success task)
             (my/task-on-failure task))))
      (when callback
        (condition-case err
            (funcall callback task)
          (error
           (message "Task callback error: %S" err)))))))

(defun my/task--sentinel (process _event)
  "Handle PROCESS completion."
  (when (memq (process-status process) '(exit signal))
    ;; drain stderr before writing the footer
    (when-let ((err-proc (process-get process 'my/task-stderr)))
      (while (and (process-live-p err-proc)
                  (accept-process-output err-proc 0.05)))
      (delete-process err-proc))
    (when-let ((task (process-get process 'my/task)))
      (let* ((code (process-exit-status process))
             (status (if (and (eq (process-status process) 'exit)
                              (zerop code))
                         'succeeded
                       'failed)))
        (my/task--finish task status code)))))

;;; Public API

(cl-defun my/task-start (&key name command directory
                              on-success on-failure
                              project kind)
  "Start an asynchronous task.

COMMAND must be a list of strings.
DIRECTORY defaults to `default-directory'.
PROJECT is the project root the task belongs to; KIND is `task' or `server'.

Return the task object."
  (unless (and (listp command)
               (stringp (car command))
               (cl-every #'stringp command))
    (user-error "COMMAND must be a nonempty list of strings"))

  (let* ((id (cl-incf my/task--next-id))
         (name (or name (car command)))
         (directory (expand-file-name
                     (or directory default-directory)))
         (buffer (generate-new-buffer
                  (format "*task:%s:%d*" name id)))
         (error-buffer (generate-new-buffer
                        (format " *task:%s:%d:stderr*" name id)))
         (task (my/task--create
                :id id
                :name name
                :command command
                :directory directory
                :buffer buffer
                :error-buffer error-buffer
                :status 'pending
                :started-at (current-time)
                :on-success on-success
                :on-failure on-failure
                :project project
                :kind (or kind 'task))))

    (puthash id task my/task--registry)

    (with-current-buffer buffer
      (when (fboundp 'my/task-log-mode) (my/task-log-mode))
      (setq-local my/task--id id)
      (let ((inhibit-read-only t))
        (insert (propertize (format "$ %s\n  in %s\n\n"
                                    (mapconcat #'shell-quote-argument command " ")
                                    (abbreviate-file-name directory))
                            'face 'my/task-meta))))

    (condition-case err
        (let* ((default-directory (file-name-as-directory directory))
               (process-environment (append my/task-environment process-environment))
               (err-proc
                (make-pipe-process
                 :name (format "task-%d-stderr" id)
                 :noquery t
                 :filter (lambda (_proc string)
                           (when (buffer-live-p error-buffer)
                             (with-current-buffer error-buffer
                               (save-excursion
                                 (goto-char (point-max))
                                 (insert string))))
                           (my/task--insert task string 'my/task-stderr))
                 :sentinel #'ignore))
               (process
                (make-process
                 :name (format "task-%d" id)
                 :buffer buffer
                 :stderr err-proc
                 :command command
                 :noquery t
                 :connection-type 'pipe
                 :filter (lambda (_proc string) (my/task--insert task string))
                 :sentinel #'my/task--sentinel)))
          (setf (my/task-process task) process)
          (process-put process 'my/task task)
          (process-put process 'my/task-stderr err-proc)
          (my/task--set-status task 'running))

      (error
       (my/task--insert task (format "Failed to start: %s\n" (error-message-string err))
                        'my/task-stderr)
       (my/task--finish task 'failed nil)))

    task))

(defun my/task-live-p (task)
  (eq (my/task-status task) 'running))

(defun my/task-cancel (task)
  "Stop TASK and everything it started."
  (interactive
   (list (my/task-read "Cancel task: ")))

  (unless (eq (my/task-status task) 'running)
    (user-error "Task is not running"))

  ;; mark before terminating, so the sentinel cannot
  ;; accidentally classify cancellation as failure
  (my/task--finish task 'cancelled nil)

  (when-let* ((process (my/task-process task))
              (pid (process-id process)))
    (when (process-live-p process)
      ;; the task is a process-group leader: signal the whole group
      (ignore-errors (signal-process (- pid) 'SIGTERM))
      (run-at-time my/task-stop-timeout nil
                   (lambda ()
                     (when (process-live-p process)
                       (ignore-errors (signal-process (- pid) 'SIGKILL))
                       (delete-process process)))))))

(defun my/task-restart (task)
  "Stop TASK if running and start a fresh copy.  Return the new task."
  (when (my/task-live-p task)
    (my/task-cancel task))
  (my/task-start :name (my/task-name task)
                 :command (my/task-command task)
                 :directory (my/task-directory task)
                 :project (my/task-project task)
                 :kind (my/task-kind task)
                 :on-success (my/task-on-success task)
                 :on-failure (my/task-on-failure task)))

(defvar my/task--health)               ; http checks, further down

(defun my/task-delete (task)
  "Forget a finished TASK and kill its buffers."
  (when (my/task-live-p task)
    (user-error "Stop the task first"))
  (remhash (my/task-id task) my/task--registry)
  (remhash (my/task-id task) my/task--health)
  (dolist (b (list (my/task-buffer task) (my/task-error-buffer task)))
    (when (buffer-live-p b) (kill-buffer b)))
  (my/task--changed task))

(defun my/task-get (id)
  "Return the task with ID."
  (gethash id my/task--registry))

(defun my/task-open-output (task)
  "Display the log for TASK."
  (interactive
   (list (my/task-read "Task output: ")))
  (pop-to-buffer (my/task-buffer task)))

(defun my/task-open-errors (task)
  "Display standard error for TASK."
  (interactive
   (list (my/task-read "Task errors: ")))
  (let ((buffer (my/task-error-buffer task)))
    (unless (buffer-live-p buffer)
      (user-error "No error buffer for this task"))
    (pop-to-buffer buffer)))

(defun my/task-all ()
  "Return all task executions, newest first."
  (let (tasks)
    (maphash (lambda (_id task)
               (push task tasks))
             my/task--registry)
    (sort tasks
          (lambda (a b)
            (> (my/task-id a)
               (my/task-id b))))))

(defun my/task-read (prompt)
  "Read a task from the registry using PROMPT."
  (let* ((tasks (my/task-all))
         (choices
          (mapcar
           (lambda (task)
             (cons
              (format "#%d %s [%s]"
                      (my/task-id task)
                      (my/task-name task)
                      (my/task-status task))
              task))
           tasks)))

    (unless choices
      (user-error "No tasks available"))

    (cdr
     (assoc
      (completing-read prompt choices nil t)
      choices))))

(defun my/task-open (task)
  "Display TASK's log buffer."
  (interactive
   (list (my/task-read "Open task: ")))
  (pop-to-buffer (my/task-buffer task)))

;;; What a task is doing: ports, memory, cpu, http
;; one ps and one ss call per refresh, shared by every view. tasks lead
;; their own process group, so a task's pid is the group id of everything
;; it started (npm -> sh -> node all count towards the same task)

(defvar my/task--usage (make-hash-table :test #'eql)
  "Process group -> (rss-kb . cpu-percent), summed over the group.")

(defvar my/task-listening nil
  "Ports listening on this machine, as (port process-group name).")

(defvar my/task--stats-at nil
  "When the stats were last collected.")

(defun my/task-collect-stats (&optional max-age)
  "Refresh memory, cpu and listening ports, unless done in the last MAX-AGE seconds."
  (unless (and max-age my/task--stats-at
               (< (float-time (time-subtract nil my/task--stats-at)) max-age))
    (setq my/task--stats-at (current-time))
    (let ((pid-group (make-hash-table :test #'eql)))
      (clrhash my/task--usage)
      (with-temp-buffer
        (when (zerop (call-process "ps" nil t nil "-e" "-o" "pid=,pgid=,rss=,pcpu="))
          (goto-char (point-min))
          (while (re-search-forward "^ *\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9.]+\\)" nil t)
            (let ((group (string-to-number (match-string 2)))
                  (rss (string-to-number (match-string 3)))
                  (cpu (string-to-number (match-string 4))))
              (puthash (string-to-number (match-string 1)) group pid-group)
              (let ((sum (gethash group my/task--usage '(0 . 0.0))))
                (puthash group (cons (+ (car sum) rss) (+ (cdr sum) cpu)) my/task--usage))))))
      (setq my/task-listening nil)
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
                  (unless (assq port my/task-listening)
                    (push (list port (and pid (gethash pid pid-group)) name) my/task-listening)))))
            (forward-line 1))))
      (setq my/task-listening (sort my/task-listening (lambda (a b) (< (car a) (car b))))))))

(defun my/task-group (task)
  "Process group of TASK while it runs."
  (when-let ((proc (my/task-process task)))
    (and (process-live-p proc) (process-id proc))))

(defun my/task-ports (task)
  "Ports TASK (or anything it started) is listening on."
  (when-let ((group (my/task-group task)))
    (mapcar #'car (seq-filter (lambda (p) (eql (nth 1 p) group)) my/task-listening))))

(defun my/task-usage (task)
  "(rss-kb . cpu-percent) of TASK and everything it started, or nil."
  (when-let ((group (my/task-group task)))
    (gethash group my/task--usage)))

(defun my/task-format-size (kb)
  (cond ((< kb 1024) (format "%d KB" kb))
        ((< kb (* 1024 1024)) (format "%d MB" (/ kb 1024)))
        (t (format "%.1f GB" (/ kb 1048576.0)))))

(defun my/task-service-url (task)
  "TASK's url: the one it printed, else its first port on localhost."
  (or (my/task-url task)
      (when-let ((port (car (my/task-ports task))))
        (format "http://localhost:%d" port))))

;; http checks: one request when a server comes up (and on demand), not
;; on a timer. every check is a line in the server's own log, and whether
;; it's still listening is already known from ss

(defvar my/task--health (make-hash-table :test #'eql)
  "Task id -> (code ms) or (down).")

(defvar my/task--checking (make-hash-table :test #'eql))

(defun my/task-check (task &optional again)
  "Ask TASK's url for a response, in the background.
Only once per task, unless AGAIN."
  (let* ((id (my/task-id task))
         (url (my/task-service-url task))
         (last (gethash id my/task--health)))
    (when (and url (my/task-live-p task) (not (gethash id my/task--checking))
               (or again (null last)
                   ;; printed its url before it was listening: try once it is
                   (and (eq (car last) 'down) (my/task-ports task))))
      (puthash id t my/task--checking)
      (let ((buf (generate-new-buffer " *task-check*")))
        (make-process
         :name "task-check" :buffer buf :noquery t
         :command (list "curl" "-s" "-o" "/dev/null" "-m" "3" "-w" "%{http_code} %{time_total}" url)
         :sentinel (lambda (proc _)
                     (unless (process-live-p proc)
                       (let ((out (with-current-buffer buf (buffer-string))))
                         (kill-buffer buf)
                         (remhash id my/task--checking)
                         (puthash id
                                  (if (and (string-match "\\`\\([0-9]+\\) \\([0-9.]+\\)" out)
                                           (> (string-to-number (match-string 1 out)) 0))
                                      (list (string-to-number (match-string 1 out))
                                            (round (* 1000 (string-to-number (match-string 2 out)))))
                                    (list 'down))
                                  my/task--health)
                         (my/task--changed task)))))))))

(defun my/task-health (task)
  "TASK's last http check: (code ms), (down), (gone) once it stopped
listening, or nil if never checked."
  (let ((health (gethash (my/task-id task) my/task--health)))
    (if (and health (not (eq (car health) 'down)) (null (my/task-ports task)))
        '(gone)
      health)))

;;; Log buffers

(defvar-local my/task--id nil
  "ID of the task whose log is in this buffer.")

(define-derived-mode my/task-log-mode special-mode "Task Log"
  "Read-only log of a task's stdout and stderr."
  (setq-local truncate-lines nil)
  (setq-local buffer-read-only t)
  ;; file:line references in compiler output become clickable
  (compilation-minor-mode 1))

(provide 'my-task)

;;; my-task.el ends here
