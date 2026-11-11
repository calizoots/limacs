;;; editor/my-code.el  -*- lexical-binding: t; -*-

;; code actions within reach, and a right-click menu for code

;; Code actions are the language server's fixes and refactors for the
;; spot you're on: import this, fill in the match arms, extract a
;; function, add a missing semicolon...  They open in a small list at
;; the cursor, the fixes first.
;;
;; Right-click in code puts the actions for that spot straight in the
;; menu (no extra "Code actions…" step), then go to, references,
;; rename and format, then cut / copy / paste.
;;
;; Keys
;; ---------------------------------------------------------------------
;;   M-RET  or  C-.  or  SPC c a   code actions here (or on the selection)
;;   SPC c r  rename the symbol      SPC c f  format (the selection or the file)
;;   SPC c i  organise imports       SPC c R  references
;;   right-click                      the menu

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function lsp-request "lsp-mode")
(declare-function lsp-feature? "lsp-mode")
(declare-function lsp-execute-code-action "lsp-mode")
(declare-function lsp--text-document-code-action-params "lsp-mode")
(declare-function lsp-get "lsp-protocol")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function my/menu "tabs")
(declare-function evil-define-key "evil-core")
(defvar lsp-response-timeout)
(defvar vertico-posframe-poshandler)
(defvar vertico-posframe-width)
(defvar vertico-posframe-height)
(defvar vertico-count)

(defvar my/code-action-timeout 2
  "Seconds to wait for the server's code actions before giving up.")

(defvar my/code-menu-max-actions 10
  "Actions shown straight in the right-click menu; the rest go in More.")

;;; asking for actions
;; ---------------------------------------------------------------------

(defun my/code--lsp-p ()
  (and (bound-and-true-p lsp-mode) (fboundp 'lsp-feature?)))

(defun my/code--clean (actions)
  "ACTIONS (from the server) as a list, disabled ones out, the most useful first."
  (my/code--sort (seq-remove (lambda (a) (lsp-get a :disabled))
                             (seq-into (or actions []) 'list))))

(defun my/code--actions ()
  "The code actions for the selection or point, the most useful first.
Instant when they were fetched ahead (see the cache below)."
  (when (and (my/code--lsp-p) (lsp-feature? "textDocument/codeAction"))
    (let ((cached (and (not (use-region-p)) (my/code--cache-get (point)))))
      (if cached
          (cdr cached)
        (let* ((lsp-response-timeout my/code-action-timeout)
               (actions (my/code--clean
                         (condition-case nil
                             (lsp-request "textDocument/codeAction"
                                          (lsp--text-document-code-action-params))
                           (error nil)))))
          (unless (use-region-p) (my/code--cache-put (buffer-chars-modified-tick) (point) actions))
          actions)))))

;;; asking ahead, so the menu opens at once
;; ---------------------------------------------------------------------
;; When the cursor or the mouse rests somewhere in code for a moment, the
;; actions for that spot are fetched in the background and kept, keyed by
;; the buffer's edit count and the position.  Typing (a new edit count)
;; or new diagnostics make them stale.

(defvar my/code-prefetch-delay 0.2 "Idle seconds before asking ahead.")

(defvar-local my/code--cache nil "((TICK . POS) . ACTIONS), newest first.")

(defun my/code--cache-get (pos)
  "(t . ACTIONS) for POS if they're known for this version of the buffer."
  (when-let ((hit (assoc (cons (buffer-chars-modified-tick) pos) my/code--cache)))
    (cons t (cdr hit))))

(defun my/code--cache-put (tick pos actions)
  (setq my/code--cache (seq-take (cons (cons (cons tick pos) actions)
                                       (seq-remove (lambda (e) (equal (car e) (cons tick pos)))
                                                   my/code--cache))
                                 16)))

(defun my/code--forget (&rest _)
  "Drop every buffer's cached actions (diagnostics changed)."
  (dolist (b (buffer-list))
    (when (buffer-local-value 'my/code--cache b)
      (with-current-buffer b (setq my/code--cache nil)))))

(defun my/code--prefetch (window pos slot)
  "Fetch the actions at POS in WINDOW's buffer in the background.
SLOT (point or mouse): a newer fetch in the same slot cancels the older one."
  (with-current-buffer (window-buffer window)
    (when (and pos (my/code--lsp-p)
               (not (use-region-p))
               (lsp-feature? "textDocument/codeAction")
               (not (my/code--cache-get pos)))
      (let ((buffer (current-buffer))
            (tick (buffer-chars-modified-tick)))
        (save-excursion
          (goto-char pos)
          (lsp-request-async
           "textDocument/codeAction" (lsp--text-document-code-action-params)
           (lambda (actions)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (my/code--cache-put tick pos (my/code--clean actions)))))
           :mode 'alive
           :error-handler #'ignore
           :cancel-token (intern (format "my/code-prefetch-%s-%s" slot (buffer-name buffer)))))))))

(defun my/code--mouse-spot ()
  "(WINDOW . POS) of the text under the mouse, or nil."
  (let* ((mouse (mouse-pixel-position))
         (frame (car mouse)) (x (cadr mouse)) (y (cddr mouse)))
    (when (and (frame-live-p frame) x y)
      (let ((posn (posn-at-x-y x y frame)))
        (when (and posn (null (posn-area posn)) (window-live-p (posn-window posn))
                   (integerp (posn-point posn)))
          (cons (posn-window posn) (posn-point posn)))))))

(defvar my/code--last-spot nil)

(defun my/code--idle ()
  "When Emacs is idle: ask ahead for where the cursor is and where the mouse rests."
  (when-let ((idle (current-idle-time)))
    (when (>= (float-time idle) my/code-prefetch-delay)
      (ignore-errors
        (my/code--prefetch (selected-window) (window-point (selected-window)) 'point)
        (let ((spot (my/code--mouse-spot)))
          ;; only once the mouse has stopped on a spot
          (when (and spot (equal spot my/code--last-spot))
            (my/code--prefetch (car spot) (cdr spot) 'mouse))
          (setq my/code--last-spot spot))))))

;; a cheap check (no server traffic unless something new needs asking)
(defvar my/code--idle-timer nil)
(when (timerp my/code--idle-timer) (cancel-timer my/code--idle-timer))
(setq my/code--idle-timer (run-at-time 0.25 0.25 #'my/code--idle))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-diagnostics-updated-hook #'my/code--forget))

(defun my/code--kind (action)
  "quickfix, refactor, source, or nil."
  (let ((kind (lsp-get action :kind)))
    (cond ((null kind) nil)
          ((string-prefix-p "quickfix" kind) 'quickfix)
          ((string-prefix-p "refactor" kind) 'refactor)
          ((string-prefix-p "source" kind) 'source))))

(defun my/code--rank (action)
  (+ (if (lsp-get action :isPreferred) 0 10)
     (pcase (my/code--kind action) ('quickfix 0) ('nil 1) ('refactor 2) ('source 3))))

(defun my/code--sort (actions)
  (sort actions (lambda (a b) (< (my/code--rank a) (my/code--rank b)))))

(defun my/code--icon (action)
  (let ((name (pcase (my/code--kind action)
                ('quickfix "nf-cod-lightbulb")
                ('refactor "nf-cod-wrench")
                ('source "nf-cod-file_code")
                (_ "nf-cod-lightbulb"))))
    (or (ignore-errors
          (let ((icon (nerd-icons-codicon name)))
            (propertize (substring-no-properties icon)
                        'face `(:family ,(plist-get (get-text-property 0 'face icon) :family)
                                :inherit ,(if (eq (my/code--kind action) 'quickfix)
                                              'warning 'shadow)))))
        "•")))

(defun my/code--title (action)
  (or (lsp-get action :title) "?"))

(defun my/code--run (action)
  "Do ACTION from the menu or list."
  (lsp-execute-code-action action))

;;; the list at the cursor
;; ---------------------------------------------------------------------

(defun my/code-actions ()
  "Pick one of the code actions for here (or the selection) from a list at the cursor."
  (interactive)
  (unless (my/code--lsp-p)
    (user-error "No language server in this buffer"))
  (let ((actions (my/code--actions)))
    (cond
     ((null actions) (message "No code actions here"))
     (t
      (let* ((seen (make-hash-table :test #'equal))
             (choices (mapcar (lambda (a)
                                (let* ((title (my/code--title a))
                                       (n (puthash title (1+ (gethash title seen 0)) seen))
                                       (label (concat (my/code--icon a) "  " title
                                                      (if (> n 1) (format " (%d)" n) ""))))
                                  (cons label a)))
                              actions))
             (pick (my/code--read "Code action: " choices)))
        (when-let ((action (cdr (assoc pick choices)))) (my/code--run action)))))))

(defun my/code--read (prompt choices)
  "PROMPT for one of CHOICES (an alist) in a small popup at the cursor."
  (let ((collection (lambda (string pred action)
                      (if (eq action 'metadata)
                          '(metadata (display-sort-function . identity)
                                     (cycle-sort-function . identity))
                        (complete-with-action action (mapcar #'car choices) string pred)))))
    (if (and (fboundp 'vertico-mode) (fboundp 'vertico-posframe-mode))
        (let ((vertico-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)
              (vertico-posframe-width (min 70 (max 30 (+ 6 (apply #'max (mapcar (lambda (c) (string-width (car c))) choices))))))
              (vertico-posframe-height (min 12 (max 3 (length choices))))
              (vertico-count (min 12 (length choices)))
              (was (bound-and-true-p vertico-mode)))
          (vertico-mode 1)
          (vertico-posframe-mode 1)
          (unwind-protect (completing-read prompt collection nil t)
            (unless was (vertico-mode -1))))
      (completing-read prompt collection nil t))))

;;; the right-click menu
;; ---------------------------------------------------------------------

(defun my/code--has (method)
  (and (my/code--lsp-p) (lsp-feature? method)))

(defun my/code--action-items (actions)
  (let* ((shown (seq-take actions my/code-menu-max-actions))
         (more (seq-drop actions my/code-menu-max-actions))
         (item (lambda (a)
                 (list (my/code--title a) (lambda () (interactive) (my/code--run a))))))
    `(,@(mapcar item shown)
      ,@(when more `(("More actions" :sub ,(mapcar item more)))))))

(defun my/code--menu-items (actions)
  (let ((lsp (my/code--lsp-p))
        (region (use-region-p)))
    `(,@(when actions (append (my/code--action-items actions) '("--")))
      ,@(when lsp
          `(("Go to definition" lsp-find-definition :keys "g d"
             :enable ,(my/code--has "textDocument/definition"))
            ("Go to type definition" lsp-find-type-definition
             :enable ,(my/code--has "textDocument/typeDefinition"))
            ("Go to implementations" lsp-find-implementation :keys "SPC g i"
             :enable ,(my/code--has "textDocument/implementation"))
            ("Find references" lsp-find-references :keys "SPC c R"
             :enable ,(my/code--has "textDocument/references"))
            ("Show documentation" lsp-describe-thing-at-point :keys "K"
             :enable ,(my/code--has "textDocument/hover"))
            "--"
            ("Rename symbol…" lsp-rename :keys "SPC c r"
             :enable ,(my/code--has "textDocument/rename"))
            ("Code actions…" my/code-actions :keys "M-RET")
            ,(if region
                 `("Format selection" lsp-format-region :keys "SPC c f"
                   :enable ,(my/code--has "textDocument/rangeFormatting"))
               `("Format document" lsp-format-buffer :keys "SPC c f"
                 :enable ,(or (my/code--has "textDocument/formatting")
                              (my/code--has "textDocument/rangeFormatting"))))
            ("Organise imports" lsp-organize-imports :keys "SPC c i")
            "--"))
      ("Cut" kill-region :enable ,(and region (not buffer-read-only)))
      ("Copy" kill-ring-save :enable ,region)
      ("Paste" yank :enable ,(not buffer-read-only))
      ,@(unless region '(("Select all" mark-whole-buffer))))))

(defun my/code-context-menu (event)
  "Right click in code: the fixes for that spot, then the usual things."
  (interactive "e")
  (let* ((posn (event-start event))
         (pos (posn-point posn))
         (window (posn-window posn)))
    (when (window-live-p window) (select-window window))
    ;; clicking inside the selection keeps it; elsewhere moves the cursor
    (when (and pos (not (and (use-region-p) (<= (region-beginning) pos (region-end)))))
      (deactivate-mark)
      (goto-char pos))
    (let ((actions (my/code--actions)))
      (popup-menu (my/menu "Code" (my/code--menu-items actions)) event))))

(defvar my/code-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-3] #'my/code-context-menu)
    (define-key map [mouse-3] #'ignore)
    map))

(define-minor-mode my/code-menu-mode
  "Right-click shows a menu of code actions and editing."
  :keymap my/code-menu-mode-map)

(add-hook 'prog-mode-hook #'my/code-menu-mode)
(add-hook 'text-mode-hook #'my/code-menu-mode)
(add-hook 'conf-mode-hook #'my/code-menu-mode)

;;; keys
;; ---------------------------------------------------------------------

(defun my/code-format ()
  "Format the selection, or the whole file."
  (interactive)
  (unless (my/code--lsp-p) (user-error "No language server in this buffer"))
  (if (use-region-p) (lsp-format-region (region-beginning) (region-end)) (lsp-format-buffer)))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual))
    (evil-define-key state my/code-menu-mode-map
      (kbd "M-RET") #'my/code-actions
      (kbd "C-.") #'my/code-actions))
  (evil-define-key '(normal visual) 'global
    (kbd "<leader>ca") #'my/code-actions
    (kbd "<leader>cr") #'lsp-rename
    (kbd "<leader>cf") #'my/code-format
    (kbd "<leader>ci") #'lsp-organize-imports
    (kbd "<leader>cR") #'lsp-find-references))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC c" "code / compile")
  (which-key-add-key-based-replacements "SPC c a" "code actions")
  (which-key-add-key-based-replacements "SPC c r" "rename symbol")
  (which-key-add-key-based-replacements "SPC c f" "format")
  (which-key-add-key-based-replacements "SPC c i" "organise imports")
  (which-key-add-key-based-replacements "SPC c R" "references"))

(provide 'my-code)

;;; my-code.el ends here
