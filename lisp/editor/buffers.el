;;; editor/buffers.el

(require 'cl-lib)
(require 'subr-x)

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

;;; scratch buffers
;; ---------------------------------------------------------------------
;; SPC b s  the *scratch* buffer (made again if it was killed)
;; SPC b n  a new scratch buffer in the language you're in

(defvar my/scratch-modes
  '(("Rust" . rust-mode) ("TypeScript" . typescript-ts-mode) ("JavaScript" . js-ts-mode)
    ("Java" . java-mode) ("Python" . python-mode) ("JSON" . json-ts-mode)
    ("HTML" . web-mode) ("CSS" . css-ts-mode) ("Markdown" . markdown-mode)
    ("Emacs Lisp" . lisp-interaction-mode) ("Plain text" . text-mode))
  "Languages offered by `my/scratch-new-in'.")

(defun my/scratch ()
  "Go to the *scratch* buffer."
  (interactive)
  (scratch-buffer))

(defun my/scratch--name (mode)
  (let* ((lang (downcase (string-remove-suffix "-mode" (string-remove-suffix "-ts-mode" (symbol-name mode)))))
         (base (format "*scratch: %s" lang)))
    (if (get-buffer (concat base "*"))
        (let ((n 2))
          (while (get-buffer (format "%s %d*" base n)) (setq n (1+ n)))
          (format "%s %d*" base n))
      (concat base "*"))))

(defun my/scratch-new-in (mode)
  "A new scratch buffer in MODE (highlighting, no language server: there's no file)."
  (interactive (list (cdr (assoc (completing-read "Scratch in: " my/scratch-modes nil t)
                                 my/scratch-modes))))
  (let ((buffer (generate-new-buffer (my/scratch--name mode))))
    (switch-to-buffer buffer)
    ;; language servers need a file; keep them from trying
    (cl-letf (((symbol-function 'lsp) #'ignore)
              ((symbol-function 'lsp-deferred) #'ignore))
      (funcall mode))
    (when (fboundp 'evil-insert-state) (evil-insert-state))
    buffer))

(defun my/scratch-new ()
  "A new scratch buffer in the same language as this one."
  (interactive)
  (my/scratch-new-in (if (and (derived-mode-p 'prog-mode 'text-mode)
                              (not (derived-mode-p 'lisp-interaction-mode)))
                         major-mode
                       'lisp-interaction-mode)))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>bs") #'my/scratch
    (kbd "<leader>bn") #'my/scratch-new))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC b s" "scratch")
  (which-key-add-key-based-replacements "SPC b n" "new scratch (this language)"))

(provide 'buffers)

;;; buffers.el ends here
