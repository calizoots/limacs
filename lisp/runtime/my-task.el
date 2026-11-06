;;; my-task.el

(require 'cl-lib)

;;; State

(cl-defstruct (my/task (:constructor my/task--create))
  id
  name
  command
  directory
  process
  buffer
  error-buffer
  status
  exit-code
  started-at
  finished-at
  on-success
  on-failure)

(defvar my/task--next-id 0
  "Next task execution ID.")

(defvar my/task--registry (make-hash-table :test #'eql)
  "Registry of task executions, indexed by ID.")

;;; Internal helpers

(defun my/task--set-status (task status)
  "Set TASK's STATUS."
  (setf (my/task-status task) status))

(defun my/task--finish (task status exit-code)
  "Finalize TASK with STATUS and EXIT-CODE."
  (unless (memq (my/task-status task)
                '(succeeded failed cancelled))
    (setf (my/task-status task) status
          (my/task-exit-code task) exit-code
          (my/task-finished-at task) (current-time))

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
    (when-let ((task (process-get process 'my/task)))
      (let* ((code (process-exit-status process))
             (status (if (and (eq (process-status process) 'exit)
                              (zerop code))
                         'succeeded
                       'failed)))
        (my/task--finish task status code)))))

;;; Public API

(cl-defun my/task-start (&key name command directory
                              on-success on-failure)
  "Start an asynchronous task.

COMMAND must be a list of strings.
DIRECTORY defaults to `default-directory'.

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
	   (format "*task:%s:%d:stderr*" name id)))
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
                :on-failure on-failure)))

    (puthash id task my/task--registry)

    (condition-case err
        (let ((default-directory
               (file-name-as-directory directory)))
          (let ((process
                 (make-process
                  :name (format "task-%d" id)
                  :buffer buffer
		  :stderr error-buffer
                  :command command
                  :noquery t
                  :connection-type 'pipe
                  :sentinel #'my/task--sentinel)))

            (setf (my/task-process task) process)
            (process-put process 'my/task task)
            (my/task--set-status task 'running)))

      (error
       (my/task--finish task 'failed nil)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (format "\nFailed to start: %s\n"
                         (error-message-string err))))))

    task))

(defun my/task-cancel (task)
  "Cancel TASK if it is running."
  (interactive
   (list (my/task-read "Cancel task: ")))

  (unless (eq (my/task-status task) 'running)
    (user-error "Task is not running"))

  ;; Mark before terminating, so the sentinel cannot
  ;; accidentally classify cancellation as failure.
  (my/task--finish task 'cancelled nil)

  (when-let ((process (my/task-process task)))
    (when (process-live-p process)
      (delete-process process))))

(defun my/task-get (id)
  "Return the task with ID."
  (gethash id my/task--registry))

(defun my/task-open-output (task)
  "Display standard output for TASK."
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
  "Display TASK's output buffer."
  (interactive
   (list (my/task-read "Open task: ")))
  (pop-to-buffer (my/task-buffer task)))

(provide 'my-task)

;;; my-task.el ends here
