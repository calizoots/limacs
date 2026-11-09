;;; editor/buffers.el

(defun kill-buffer-and-window ()
  "Kill the current buffer, then close its window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(global-set-key (kbd "C-c C-k") #'kill-buffer-and-window)

(defun my/find-buffer ()
  "Select a buffer to open in current window."
  (interactive)
  (let* ((all-buffers (mapcar #'buffer-name (buffer-list)))
         (choice (completing-read "find buffer: " all-buffers)))
    (when (and choice (not (string-empty-p choice)))
      (switch-to-buffer choice))))

(defun my/find-buffer-floating ()
  "Select a buffer using a beautifully custom-rendered floating childframe."
  (interactive)
  (let* ((all-buffers (mapcar #'buffer-name (buffer-list)))
         (choice (completing-read "Find Buffer: " all-buffers)))
    
    (when (and choice (not (string-empty-p choice)))
      (switch-to-buffer choice))))

(provide 'buffers)

;;; buffers.el ends here
