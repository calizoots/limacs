;;; pkgs/my-tree.el  -*- lexical-binding: t; -*-

;; the file tree: drag and drop, a right-click menu, and file keys

;; NeoTree, made to manage files like VS Code's explorer:
;;
;; Drag a file or folder onto a folder (or onto a file: its folder) to
;; move it there, after a quick "Move X into Y?".  Files you have open
;; follow the move.  Drop a file onto your code to open it there.
;;
;; Right-click a file or folder for: open, open to the side, new file,
;; new folder, rename, duplicate, cut, copy, paste, delete (to the bin),
;; copy path, search in the folder, a terminal there, the file manager.
;; Right-click empty space for new / paste / refresh / hidden files.
;;
;; Keys (normal state, in the tree)
;; ---------------------------------------------------------------------
;;   N  new file (a/b/c.rs makes the folders)     +  new folder
;;   R  rename      y  copy     x  cut     p  paste into this folder
;;   d  delete (to the bin, asks first)            Y  copy the path
;;   m  the menu, from the keyboard

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function neo-buffer--get-filename-current-line "neotree")
(declare-function neotree-refresh "neotree")
(declare-function neo-global--window-exists-p "neotree")
(declare-function neo-global--get-window "neotree")
(declare-function neo-buffer--expanded-node-p "neotree")
(declare-function neo-buffer--set-expand "neotree")
(declare-function neo-buffer--refresh "neotree")
(declare-function evil-define-key "evil-core")
(declare-function my/menu "tabs")
(declare-function my/project-root "projects")
(declare-function my/panel--show "activity-bar")
(declare-function consult-ripgrep "consult")
(defvar neo-buffer--start-node)
(defvar neo-show-hidden-files)

;;; file operations (no tree needed: these are what the tests check)
;; ---------------------------------------------------------------------

(defun my/tree--dir-of (path)
  "PATH if it's a folder, else the folder it's in."
  (file-name-as-directory (if (file-directory-p path) path (file-name-directory (directory-file-name path)))))

(defun my/tree--free-name (dir name)
  "NAME in DIR, or \"NAME copy\", \"NAME copy 2\"... if that's taken."
  (let* ((ext (file-name-extension name t))
         (base (if (and ext (not (string-prefix-p "." name))) (file-name-sans-extension name) name))
         (ext (if (string-prefix-p "." name) "" (or ext "")))
         (candidate (expand-file-name name dir))
         (n 1))
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name (format "%s copy%s%s" base (if (= n 1) "" (format " %d" n)) ext) dir)
            n (1+ n)))
    candidate))

(defun my/tree--follow (from to)
  "Open buffers visiting FROM (a file, or anything inside a folder) now visit TO."
  (let ((from (directory-file-name (expand-file-name from)))
        (to (directory-file-name (expand-file-name to))))
    (dolist (b (buffer-list))
      (when-let ((file (buffer-file-name b)))
        (let ((file (expand-file-name file)))
          (when (or (equal file from) (string-prefix-p (concat from "/") file))
            (with-current-buffer b
              (let ((modified (buffer-modified-p)))
                (set-visited-file-name (concat to (substring file (length from))) t t)
                (set-buffer-modified-p modified)))))))))

(defun my/tree-move (path dir)
  "Move PATH into folder DIR; return the new path.  Open buffers follow."
  (let* ((dir (file-name-as-directory dir))
         (name (file-name-nondirectory (directory-file-name path)))
         (target (expand-file-name name dir)))
    (cond ((equal (directory-file-name (expand-file-name path)) (directory-file-name target))
           (user-error "%s is already there" name))
          ((and (file-directory-p path)
                (string-prefix-p (file-name-as-directory (expand-file-name path)) dir))
           (user-error "Can't move a folder into itself"))
          ((file-exists-p target)
           (user-error "%s already has a %s" (file-name-nondirectory (directory-file-name dir)) name)))
    (rename-file (directory-file-name path) target)
    (my/tree--follow path target)
    target))

(defun my/tree-copy-into (path dir)
  "Copy PATH into folder DIR (as \"name copy\" if taken); return the copy."
  (let ((target (my/tree--free-name dir (file-name-nondirectory (directory-file-name path)))))
    (if (file-directory-p path)
        (copy-directory path target t t t)
      (copy-file path target nil t t))
    target))

(defun my/tree-new-file (dir name)
  "Make file NAME (it may hold folders: a/b/c.rs) in DIR; return its path."
  (let ((path (expand-file-name name dir)))
    (when (file-exists-p path) (user-error "%s already exists" name))
    (make-directory (file-name-directory path) t)
    (write-region "" nil path nil 'silent)
    path))

(defun my/tree-new-folder (dir name)
  (let ((path (expand-file-name name dir)))
    (when (file-exists-p path) (user-error "%s already exists" name))
    (make-directory path t)
    path))

(defun my/tree-rename (path name)
  "Give PATH the new NAME (same folder); return the new path.  Open buffers follow."
  (let ((target (expand-file-name name (file-name-directory (directory-file-name path)))))
    (when (file-exists-p target) (user-error "%s already exists" name))
    (make-directory (file-name-directory target) t)
    (rename-file (directory-file-name path) target)
    (my/tree--follow path target)
    target))

(defun my/tree-delete (path)
  "Move PATH to the bin; close buffers that were visiting it."
  (let ((delete-by-moving-to-trash t)
        (gone (directory-file-name (expand-file-name path))))
    (if (file-directory-p path)
        (delete-directory path t t)
      (delete-file path t))
    (dolist (b (buffer-list))
      (when-let ((file (buffer-file-name b)))
        (when (or (equal (expand-file-name file) gone)
                  (string-prefix-p (concat gone "/") (expand-file-name file)))
          (unless (buffer-modified-p b) (kill-buffer b)))))))

;;; where in the tree
;; ---------------------------------------------------------------------

(defun my/tree--root ()
  (or (and (boundp 'neo-buffer--start-node) neo-buffer--start-node)
      (and (fboundp 'my/project-root) (my/project-root))
      default-directory))

(defun my/tree--path-at (pos)
  "The file or folder on the line at POS in the tree, or nil."
  (save-excursion
    (goto-char pos)
    (neo-buffer--get-filename-current-line)))

(defun my/tree--here ()
  "The file or folder at point, or the tree's root."
  (or (neo-buffer--get-filename-current-line) (my/tree--root)))

(defun my/tree--refresh ()
  (when (and (fboundp 'neo-global--window-exists-p) (neo-global--window-exists-p))
    (with-selected-window (neo-global--get-window)
      (neotree-refresh t))))

(defun my/tree--code-window ()
  "Where files from the tree open."
  (or (and (fboundp 'my/activity--editor-window) (my/activity--editor-window))
      (next-window)))

(defun my/tree--short (path)
  (file-name-nondirectory (directory-file-name path)))

;;; dragging
;; ---------------------------------------------------------------------

(defun my/tree--parent (path)
  (file-name-as-directory (file-name-directory (directory-file-name path))))

(defun my/tree--can-drop-p (from dir)
  "Non-nil if moving FROM into DIR would do something."
  (and from dir
       (not (equal (my/tree--parent from) dir))
       (not (and (file-directory-p from)
                 (string-prefix-p (file-name-as-directory (expand-file-name from)) dir)))))

(defun my/tree--drop (from dir)
  "Move FROM into DIR after asking."
  (when (and (my/tree--can-drop-p from dir)
             (y-or-n-p (format "Move %s into %s? " (my/tree--short from) (my/tree--short dir))))
    (my/tree-move from dir)
    (my/tree--refresh)
    (message "Moved %s into %s" (my/tree--short from) (my/tree--short dir))))

(defun my/tree-drag (event &optional from dir)
  "Move what was dragged (FROM) into DIR, or where EVENT ended; dropped
on code, open it there."
  (interactive "e")
  (let* ((start (event-start event))
         (end (event-end event))
         (from (or from (my/tree--path-at (posn-point start))))
         (end-window (posn-window end)))
    (cond
     ((null from) nil)
     ;; dropped outside the tree: open it in that window
     ((and (window-live-p end-window)
           (not (eq end-window (posn-window start))))
      (unless (file-directory-p from)
        (select-window end-window)
        (find-file from)))
     (t
      (my/tree--drop from (or dir (my/tree--dir-of (or (and (posn-point end) (my/tree--path-at (posn-point end)))
                                                        (my/tree--root)))))))))

;; the preview while dragging

(defface my/tree-drop-folder '((t)) "The folder something would be dropped into.")
(defface my/tree-drop-line '((t)) "Where it would land (a line on top of a row).")
(defface my/tree-drop-line-below '((t)) "Where it would land (under the last row).")
(defface my/tree-dragged '((t)) "The row being dragged.")

(defvar my/tree--drag-overlays nil)

(defun my/tree--drag-clear ()
  (mapc #'delete-overlay my/tree--drag-overlays)
  (setq my/tree--drag-overlays nil))

(defun my/tree--overlay (start end &rest props)
  (let ((ov (make-overlay start end)))
    (while props (overlay-put ov (pop props) (pop props)))
    (push ov my/tree--drag-overlays)
    ov))

(defun my/tree--rows ()
  "Every row of the tree: ((START . PATH) ...) top to bottom."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((path (neo-buffer--get-filename-current-line)))
          (push (cons (line-beginning-position) path) rows))
        (forward-line 1)))
    (nreverse rows)))

(defun my/tree--sorts-before-p (a a-dir b b-dir)
  "Non-nil if A (a folder when A-DIR) lists before B: folders first, then by name."
  (if (eq (and a-dir t) (and b-dir t))
      (funcall (or (bound-and-true-p neo-filepath-sort-function) #'string<) a b)
    a-dir))

(defun my/tree--landing (from dir rows)
  "Where in ROWS FROM would show inside DIR: the row it'd go before, or
\='(after . POS) past the last of DIR's rows, or nil when DIR's contents
aren't shown."
  (let* ((name (my/tree--short from))
         (from-dir (file-directory-p from))
         (children (seq-filter (lambda (r) (equal (my/tree--parent (cdr r)) dir)) rows))
         (dir-row (seq-find (lambda (r) (equal (file-name-as-directory (cdr r)) dir)) rows))
         (root-p (equal dir (file-name-as-directory (my/tree--root)))))
    (cond
     ;; a folder that's open (or the root): between its children
     ((or children root-p
          (and dir-row (neo-buffer--expanded-node-p (directory-file-name (cdr dir-row)))))
      (or (car (seq-find (lambda (r)
                           (and (not (equal (cdr r) from))
                                (my/tree--sorts-before-p name from-dir
                                                         (my/tree--short (cdr r)) (file-directory-p (cdr r)))))
                         children))
          ;; after the last thing inside DIR (its last child's own contents too)
          (let ((last (or (car (last children)) dir-row)))
            (when last
              (let ((after (seq-find (lambda (r) (and (> (car r) (car last))
                                                      (not (string-prefix-p (file-name-as-directory (cdr last)) (cdr r)))))
                                     rows)))
                (cons 'after (if after (car after) (point-max))))))))
     (t nil))))

(defun my/tree--drag-show (from dir rows)
  "Preview dropping FROM into DIR: tint the folder, draw a line where it'd land."
  (my/tree--drag-clear)
  (when-let ((from-row (seq-find (lambda (r) (equal (cdr r) from)) rows)))
    (save-excursion
      (goto-char (car from-row))
      (my/tree--overlay (line-beginning-position) (line-end-position) 'face 'my/tree-dragged)))
  (when (my/tree--can-drop-p from dir)
    ;; the folder and everything shown inside it
    (when-let ((dir-row (seq-find (lambda (r) (equal (file-name-as-directory (cdr r)) dir)) rows)))
      (let ((after (seq-find (lambda (r) (and (> (car r) (car dir-row))
                                              (not (string-prefix-p dir (cdr r)))))
                             rows)))
        (my/tree--overlay (car dir-row) (if after (car after) (point-max)) 'face 'my/tree-drop-folder)))
    ;; a hairline along the top of the row it would go before (nothing
    ;; moves); past the end of the tree, along the bottom of the last row
    (let ((landing (my/tree--landing from dir rows)))
      (pcase landing
        ((pred integerp) (my/tree--hairline landing 'top))
        (`(after . ,pos)
         (if (< pos (point-max))
             (my/tree--hairline pos 'top)
           (my/tree--hairline (save-excursion (goto-char (point-max)) (forward-line -1) (point))
                              'bottom)))))))

(defun my/tree--hairline (pos edge)
  "A thin accent line along the top (or bottom) EDGE of the row at POS."
  (save-excursion
    (goto-char pos)
    (my/tree--overlay (line-beginning-position) (min (point-max) (1+ (line-end-position)))
                      'face (if (eq edge 'top) 'my/tree-drop-line 'my/tree-drop-line-below)
                      'priority 100)))

(defun my/tree--expand (path)
  "Open folder PATH in the tree (while dragging)."
  (neo-buffer--set-expand (directory-file-name path) t)
  (neo-buffer--refresh t))

(defun my/tree--collapse (path)
  (neo-buffer--set-expand (directory-file-name path) nil)
  (neo-buffer--refresh t))

(defvar my/tree-spring-delay 0.45 "Seconds resting on a closed folder before it opens mid-drag.")

(defun my/tree-mouse-down (event)
  "Press on a name: a drag moves it (with a preview), a click opens it."
  (interactive "e")
  ;; neotree opens the line point is on, so be on the clicked line
  (mouse-set-point event)
  (let* ((window (posn-window (event-start event)))
         (from (my/tree--path-at (posn-point (event-start event))))
         (root (file-name-as-directory (my/tree--root)))
         (dir nil) (hover nil) (hover-since 0) (opened nil) (dragging nil) up)
    (unwind-protect
        (track-mouse
          (while (not up)
            (let ((ev (read-event nil nil 0.1)))
              (cond
               ;; resting: a closed folder under the mouse springs open
               ((null ev)
                (when (and dragging hover (file-directory-p hover)
                           (not (neo-buffer--expanded-node-p (directory-file-name hover)))
                           (> (- (float-time) hover-since) my/tree-spring-delay)
                           (my/tree--can-drop-p from (file-name-as-directory hover)))
                  (my/tree--expand hover)
                  (push hover opened)
                  (my/tree--drag-show from dir (my/tree--rows))))
               ((mouse-movement-p ev)
                (setq dragging t)
                (let ((posn (event-start ev)))
                  (if (and (eq (posn-window posn) window) (posn-point posn))
                      (let* ((onto (my/tree--path-at (posn-point posn)))
                             (folder (and onto (file-directory-p onto) onto)))
                        (setq dir (my/tree--dir-of (or onto root)))
                        (unless (equal folder hover) (setq hover folder hover-since (float-time)))
                        (my/tree--drag-show from dir (my/tree--rows)))
                    ;; over the code: no landing line in the tree
                    (setq hover nil dir nil)
                    (my/tree--drag-clear))))
               (t (setq up ev))))))
      (my/tree--drag-clear))
    (cond
     ;; moved while held: a drag
     ((and (consp up) (memq 'drag (event-modifiers up)))
      (my/tree-drag up from (and (eq (posn-window (event-end up)) window) dir))
      ;; folders that only opened on the way close again
      (when (window-live-p window)
        (with-selected-window window
          (dolist (f opened)
            (unless (and dir (string-prefix-p (file-name-as-directory f) dir))
              (ignore-errors (my/tree--collapse f)))))))
     ;; let go where it started: a click, which buttons take as mouse-2
     ((and (consp up) (memq 'click (event-modifiers up)))
      (setq unread-command-events
            (cons (cons 'mouse-2 (cdr up)) unread-command-events)))
     ;; anything else (a key pressed mid-click): leave it be
     (up (push up unread-command-events)))))

(defun my/tree-apply-drag-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let ((bg (my/tab--face-color 'default :background "#1e1f22"))
          (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
          (accent (my/tab--face-color 'link :foreground "#56a8f5")))
      (set-face-attribute 'my/tree-drop-folder nil :background (my/tab--mix accent bg 0.09) :extend t)
      ;; one pixel, softened: it marks the spot without shouting
      (set-face-attribute 'my/tree-drop-line nil :overline (my/tab--mix accent bg 0.75) :extend t)
      (set-face-attribute 'my/tree-drop-line-below nil
                          :underline (list :color (my/tab--mix accent bg 0.75) :position t) :extend t)
      (set-face-attribute 'my/tree-dragged nil :foreground (my/tab--mix fg bg 0.4)))))

(my/tree-apply-drag-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/tree-apply-drag-theme))

;;; the commands
;; ---------------------------------------------------------------------

(defvar my/tree--clipboard nil "(cut|copy . PATHS) waiting to be pasted.")

(defun my/tree-new-file-here (&optional at)
  "New file in the folder at point (or AT)."
  (interactive)
  (let* ((dir (my/tree--dir-of (or at (my/tree--here))))
         (name (read-string (format "New file in %s/: " (my/tree--short dir)))))
    (unless (string-blank-p name)
      (let ((path (my/tree-new-file dir name)))
        (my/tree--refresh)
        (select-window (my/tree--code-window))
        (find-file path)))))

(defun my/tree-new-folder-here (&optional at)
  (interactive)
  (let* ((dir (my/tree--dir-of (or at (my/tree--here))))
         (name (read-string (format "New folder in %s/: " (my/tree--short dir)))))
    (unless (string-blank-p name)
      (my/tree-new-folder dir name)
      (my/tree--refresh))))

(defun my/tree-rename-here (&optional at)
  (interactive)
  (let* ((path (or at (my/tree--here)))
         (name (read-string (format "Rename %s to: " (my/tree--short path)) (my/tree--short path))))
    (unless (or (string-blank-p name) (equal name (my/tree--short path)))
      (my/tree-rename path name)
      (my/tree--refresh))))

(defun my/tree-duplicate-here (&optional at)
  (interactive)
  (let* ((path (or at (my/tree--here)))
         (copy (my/tree-copy-into path (file-name-directory (directory-file-name path)))))
    (my/tree--refresh)
    (message "Made %s" (my/tree--short copy))))

(defun my/tree-copy-here (&optional at)
  (interactive)
  (let ((path (or at (my/tree--here))))
    (setq my/tree--clipboard (cons 'copy (list path)))
    (message "Copied %s: p in a folder pastes it" (my/tree--short path))))

(defun my/tree-cut-here (&optional at)
  (interactive)
  (let ((path (or at (my/tree--here))))
    (setq my/tree--clipboard (cons 'cut (list path)))
    (message "Cut %s: p in a folder moves it there" (my/tree--short path))))

(defun my/tree-paste-here (&optional at)
  "Paste what was copied or cut into the folder at point."
  (interactive)
  (unless my/tree--clipboard (user-error "Nothing copied or cut"))
  (let ((dir (my/tree--dir-of (or at (my/tree--here)))))
    (dolist (path (cdr my/tree--clipboard))
      (if (eq (car my/tree--clipboard) 'cut)
          (my/tree-move path dir)
        (my/tree-copy-into path dir)))
    (when (eq (car my/tree--clipboard) 'cut) (setq my/tree--clipboard nil))
    (my/tree--refresh)))

(defun my/tree-delete-here (&optional at)
  "Delete the file or folder at point (to the bin), after asking."
  (interactive)
  (let ((path (or at (my/tree--here))))
    (when (equal (file-name-as-directory path) (file-name-as-directory (my/tree--root)))
      (user-error "That's the whole project"))
    (when (yes-or-no-p (format "Move %s%s to the bin? " (my/tree--short path)
                               (if (file-directory-p path) " and everything in it" "")))
      (my/tree-delete path)
      (my/tree--refresh))))

(defun my/tree-copy-path (&optional at relative)
  (interactive)
  (let* ((path (or at (my/tree--here)))
         (text (if relative (file-relative-name path (my/tree--root)) (expand-file-name path))))
    (kill-new text)
    (message "Copied %s" text)))

(defun my/tree-open-side (&optional at)
  "Open the file at point beside the current code window."
  (interactive)
  (let ((path (or at (my/tree--here))))
    (unless (file-directory-p path)
      (let ((code (my/tree--code-window)))
        (select-window (split-window code nil 'right))
        (find-file path)))))

(defun my/tree-terminal-here (&optional at)
  "A terminal in the folder at point, in the bottom panel."
  (interactive)
  (let* ((dir (my/tree--dir-of (or at (my/tree--here))))
         (buffer (let ((default-directory dir))
                   (save-window-excursion
                     (call-interactively (if (fboundp 'ghostel) #'ghostel #'shell))
                     (current-buffer)))))
    (if (fboundp 'my/panel--show)
        (my/panel--show 'terminal buffer)
      (pop-to-buffer buffer))))

(defun my/tree-reveal (&optional at)
  "Show the folder at point in the desktop's file manager."
  (interactive)
  (call-process "xdg-open" nil 0 nil (my/tree--dir-of (or at (my/tree--here)))))

(defun my/tree-search-here (&optional at)
  "Search the text of every file in the folder at point."
  (interactive)
  (let ((dir (my/tree--dir-of (or at (my/tree--here)))))
    (if (fboundp 'my/with-vertico)
        (my/with-vertico (lambda () (interactive) (consult-ripgrep dir)))
      (consult-ripgrep dir))))

;;; the menu
;; ---------------------------------------------------------------------

(defmacro my/tree--on (fn path)
  `(lambda () (interactive) (,fn ,path)))

(defun my/tree--menu-items (path)
  "What the right-click menu offers for PATH (nil: empty space)."
  (let* ((dir-p (and path (file-directory-p path)))
         (target (or path (my/tree--root)))
         (paste (and my/tree--clipboard
                     (format "Paste %s" (my/tree--short (cadr my/tree--clipboard))))))
    `(,@(when (and path (not dir-p))
          `(("Open" ,(lambda () (interactive) (select-window (my/tree--code-window)) (find-file path)))
            ("Open to the side" ,(my/tree--on my/tree-open-side path))
            "--"))
      ("New file…" ,(my/tree--on my/tree-new-file-here target) :keys "N")
      ("New folder…" ,(my/tree--on my/tree-new-folder-here target) :keys "+")
      ,@(when path
          `(("Rename…" ,(my/tree--on my/tree-rename-here path) :keys "R")
            ("Duplicate" ,(my/tree--on my/tree-duplicate-here path))))
      "--"
      ,@(when path
          `(("Cut" ,(my/tree--on my/tree-cut-here path) :keys "x")
            ("Copy" ,(my/tree--on my/tree-copy-here path) :keys "y")))
      (,(or paste "Paste") ,(my/tree--on my/tree-paste-here target) :keys "p" :enable ,(and paste t))
      ,@(when path
          `(("Delete" ,(my/tree--on my/tree-delete-here path) :keys "d")))
      "--"
      ,@(when path
          `(("Copy path" ,(my/tree--on my/tree-copy-path path) :keys "Y")
            ("Copy relative path" ,(lambda () (interactive) (my/tree-copy-path path t)))
            "--"))
      ("Search in folder…" ,(my/tree--on my/tree-search-here target))
      ("Open terminal here" ,(my/tree--on my/tree-terminal-here target))
      ("Show in file manager" ,(my/tree--on my/tree-reveal target))
      ,@(unless path
          `("--"
            ("Refresh" ,(lambda () (interactive) (my/tree--refresh)) :keys "r")
            ("Show hidden files" neotree-hidden-file-toggle :keys "'"
             :button (:toggle . (bound-and-true-p neo-show-hidden-files))))))))

(defun my/tree-menu (event)
  "Right-click in the tree."
  (interactive "e")
  (let* ((posn (event-start event))
         (pos (posn-point posn)))
    (select-window (posn-window posn))
    (when pos (goto-char pos))
    (let ((path (and pos (my/tree--path-at pos))))
      (popup-menu (my/menu "Explorer" (my/tree--menu-items path)) event))))

(defun my/tree-menu-at-point ()
  "The right-click menu, from the keyboard."
  (interactive)
  (let ((path (neo-buffer--get-filename-current-line)))
    (popup-menu (my/menu "Explorer" (my/tree--menu-items path))
                (let ((xy (posn-x-y (posn-at-point))))
                  (list (list (car xy) (+ (cdr xy) (frame-char-height))) (selected-window))))))

;;; looks: a highlighted row instead of a cursor box
;; ---------------------------------------------------------------------

(defface my/tree-current '((t)) "The row you're on in the tree.")

(defun my/tree-apply-theme (&rest _)
  (when (fboundp 'my/tab--mix)
    (let ((bg (my/tab--face-color 'default :background "#1e1f22"))
          (fg (my/tab--face-color 'default :foreground "#dfe1e5")))
      (set-face-attribute 'my/tree-current nil :background (my/tab--mix fg bg 0.1) :extend t))))

(defun my/tree--setup ()
  (setq-local cursor-type nil
              evil-normal-state-cursor '(nil))
  (face-remap-add-relative 'hl-line 'my/tree-current)
  (hl-line-mode 1))

(add-hook 'neotree-mode-hook #'my/tree--setup)
(my/tree-apply-theme)
(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/tree-apply-theme))

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'neotree
  ;; on the file and folder names themselves (their keymaps come first)
  (dolist (map (list neotree-file-button-keymap neotree-dir-button-keymap))
    (define-key map [down-mouse-1] #'my/tree-mouse-down)  ; click opens, drag moves
    (define-key map [drag-mouse-1] #'my/tree-drag)
    (define-key map [down-mouse-3] #'my/tree-menu)
    (define-key map [mouse-3] #'ignore))
  ;; and the rest of the tree (empty space, indentation)
  (define-key neotree-mode-map [down-mouse-3] #'my/tree-menu)
  (define-key neotree-mode-map [mouse-3] #'ignore)
  (with-eval-after-load 'evil
    (evil-define-key 'normal neotree-mode-map
      "N" #'my/tree-new-file-here
      "+" #'my/tree-new-folder-here
      "R" #'my/tree-rename-here
      "y" #'my/tree-copy-here
      "x" #'my/tree-cut-here
      "p" #'my/tree-paste-here
      "d" #'my/tree-delete-here
      "Y" #'my/tree-copy-path
      "m" #'my/tree-menu-at-point
      [down-mouse-3] #'my/tree-menu
      [mouse-3] #'ignore)))

(provide 'my-tree)

;;; my-tree.el ends here
