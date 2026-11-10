;;; runtime/my-git.el  -*- lexical-binding: t; -*-

;; Source Control: the git panel beside the activity bar

;; What VS Code's Source Control view does, sized for the sidebar: the
;; branch and how far it is from its remote, a commit message box with
;; a Commit button (that turns into Push / Sync / Publish when there's
;; nothing to commit), staged and unstaged changes, stashes and the
;; last few commits.  Rows show their actions (stage, discard, open)
;; when the cursor or the mouse is on them.
;;
;; Pushing and pulling run in the background.  Anything that asks for a
;; password or passphrase does it in the desktop's own window
;; (ksshaskpass), never in Emacs.
;;
;; Keys (normal state)
;; ---------------------------------------------------------------------
;;   SPC g s  open / close the panel     SPC g c  commit
;;   SPC g P  push    SPC g p  pull       SPC g b  switch branch
;;
;; In the panel:
;;   RET  show the diff (focus stays here)    o  open the file
;;   s    stage        u  unstage             x  discard changes
;;   S    stage all    U  unstage all         TAB  fold a section
;;   c    write the message / commit          C  amend the last commit
;;   P    push         F  pull                f  fetch
;;   b    switch branch                       B  new branch
;;   z    stash        Z  pop the last stash  X  undo the last commit
;;   g    refresh      $  git's output        q  close     ?  keys

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-normal-state "evil-states")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function my/project-root "projects")
(declare-function my/tab--mix "tabs")
(declare-function my/tab--face-color "tabs")
(declare-function my/menu "tabs")
(declare-function my/activity--editor-window "activity-bar")
(declare-function my/activity-render "activity-bar")

(defvar my/git-recent-count 8 "How many recent commits the panel lists.")
(defvar my/git-max-rows 300 "Most changed files listed (a missing .gitignore can mean thousands).")
(defvar my/git-interval 5 "Seconds between refreshes while a panel is showing.")
(defvar my/git-output-buffer "*git output*")

;;; running git
;; ---------------------------------------------------------------------

(defconst my/git--global-args '("--no-pager" "-c" "core.quotepath=false" "-c" "color.ui=false"))

(defun my/git--call (&rest args)
  "Run git ARGS in `default-directory'; (EXIT . OUTPUT), stderr included."
  (with-temp-buffer
    (let ((code (apply #'process-file "git" nil t nil (append my/git--global-args args))))
      (cons code (buffer-string)))))

(defun my/git--out (&rest args)
  "Standard output of git ARGS, or nil when it fails."
  (with-temp-buffer
    (when (eq 0 (apply #'process-file "git" nil (list t nil) nil
                       (append my/git--global-args args)))
      (buffer-string))))

(defun my/git--first-error (output)
  "The line of OUTPUT worth showing."
  (let ((lines (split-string output "\n" t "[ \t]+")))
    (or (seq-find (lambda (l) (string-match-p "\\`\\(error\\|fatal\\|ERROR\\)" l)) lines)
        (seq-find (lambda (l) (not (string-prefix-p "hint:" l))) lines)
        "failed")))

(defun my/git--do (&rest args)
  "Run git ARGS now; signal its error, or return its output."
  (let ((result (apply #'my/git--call args)))
    (unless (eq 0 (car result))
      (my/git--log (cons "git" args) (cdr result))
      (user-error "git %s: %s" (car args) (my/git--first-error (cdr result))))
    (cdr result)))

(defun my/git--log (command output)
  "Keep COMMAND and its OUTPUT in the git output buffer."
  (with-current-buffer (get-buffer-create my/git-output-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (concat "$ " (string-join command " ") "\n") 'face 'bold)
              output (if (string-suffix-p "\n" output) "" "\n") "\n"))))

(defun my/git-root (&optional dir)
  "Top folder of the git repository holding DIR (or the project, or here)."
  (let ((default-directory (or dir
                               (and (fboundp 'my/project-root) (my/project-root))
                               default-directory)))
    (when-let ((top (my/git--out "rev-parse" "--show-toplevel")))
      (file-name-as-directory (string-trim top)))))

;; background jobs, one at a time per repository

(defvar my/git--busy (make-hash-table :test #'equal)
  "Repository root -> what it's doing (\"Pushing…\").")

(defun my/git--environment ()
  "The environment for git jobs: passwords go to the desktop's own prompt."
  (append (list "GIT_TERMINAL_PROMPT=0")
          (when (and (getenv "SSH_ASKPASS") (not (getenv "SSH_ASKPASS_REQUIRE")))
            (list "SSH_ASKPASS_REQUIRE=force"))
          process-environment))

(defun my/git--async (root label args &optional on-done)
  "Run git ARGS in ROOT in the background, showing LABEL in its panel.
ON-DONE gets t on success, nil on failure."
  (when (gethash root my/git--busy)
    (user-error "Still %s" (downcase (gethash root my/git--busy))))
  (puthash root label my/git--busy)
  (my/git--render-root root)
  (let* ((default-directory root)
         (process-environment (my/git--environment))
         (out (generate-new-buffer " *git job*"))
         (command (cons "git" args)))
    (make-process
     :name "git" :buffer out :connection-type 'pipe :noquery t
     :command (append (list "git") my/git--global-args args)
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((ok (eq 0 (process-exit-status proc)))
               (output (with-current-buffer out (buffer-string))))
           (kill-buffer out)
           (remhash root my/git--busy)
           (my/git--log command output)
           (if ok
               (message "%s done" (string-remove-suffix "…" label))
             (message "%s failed: %s   ($ in the panel for the details)"
                      (string-remove-suffix "…" label) (my/git--first-error output)))
           (when on-done (funcall on-done ok))
           (my/git--render-root root)))))))

;;; reading the repository
;; ---------------------------------------------------------------------

(defun my/git--after-fields (string n)
  "STRING after its first N space-separated fields."
  (let ((i 0))
    (dotimes (_ n) (setq i (1+ (string-search " " string i))))
    (substring string i)))

(defun my/git--status (root)
  "What's going on in ROOT, as a plist."
  (let* ((default-directory root)
         (parts (split-string (or (my/git--out "--no-optional-locks" "status" "--porcelain=v2"
                                               "--branch" "-z" "--untracked-files=all")
                                  "")
                              "\0" t))
         branch oid upstream (ahead 0) (behind 0) staged changes conflicts)
    (while parts
      (let ((p (pop parts)))
        (cond
         ((string-prefix-p "# branch.head " p) (setq branch (substring p 14)))
         ((string-prefix-p "# branch.oid " p) (setq oid (substring p 13)))
         ((string-prefix-p "# branch.upstream " p) (setq upstream (substring p 18)))
         ((string-match "\\`# branch.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" p)
          (setq ahead (string-to-number (match-string 1 p))
                behind (string-to-number (match-string 2 p))))
         ((string-match "\\`\\([12]\\) \\(.\\)\\(.\\) " p)
          (let* ((x (match-string 2 p)) (y (match-string 3 p))
                 (path (my/git--after-fields p (if (equal (match-string 1 p) "1") 8 9)))
                 (orig (when (equal (match-string 1 p) "2") (pop parts))))
            (unless (equal x ".") (push (list :path path :orig orig :code x) staged))
            (unless (equal y ".") (push (list :path path :code y) changes))))
         ((string-prefix-p "u " p)
          (push (list :path (my/git--after-fields p 10) :code "!") conflicts))
         ((string-prefix-p "? " p)
          (push (list :path (substring p 2) :code "U") changes)))))
    (list :root root :branch branch :oid oid :upstream upstream
          :ahead ahead :behind behind
          :staged (nreverse staged) :changes (nreverse changes) :conflicts (nreverse conflicts)
          :remotes (split-string (or (my/git--out "remote") "") "\n" t)
          :commits (unless (equal oid "(initial)") (my/git--commits))
          :stashes (my/git--stashes))))

(defun my/git--ago (seconds)
  (let ((d (- (float-time) seconds)))
    (cond ((< d 60) "now")
          ((< d 3600) (format "%dm" (/ d 60)))
          ((< d 86400) (format "%dh" (/ d 3600)))
          ((< d (* 86400 30)) (format "%dd" (/ d 86400)))
          ((< d (* 86400 365)) (format "%dmo" (/ d (* 86400 30))))
          (t (format "%dy" (/ d (* 86400 365)))))))

(defun my/git--commits ()
  (mapcar (lambda (line)
            (pcase-let ((`(,hash ,subject ,time ,author) (split-string line "\x1f")))
              (list :hash hash :subject subject :ago (my/git--ago (string-to-number time))
                    :author author)))
          (split-string (or (my/git--out "log" (format "-n%d" my/git-recent-count)
                                         "--format=%h%x1f%s%x1f%ct%x1f%an")
                            "")
                        "\n" t)))

(defun my/git--stashes ()
  (mapcar (lambda (line)
            (pcase-let ((`(,ref ,subject) (split-string line "\x1f")))
              (list :ref ref :subject subject)))
          (split-string (or (my/git--out "stash" "list" "--format=%gd%x1f%s") "") "\n" t)))

;;; looks
;; ---------------------------------------------------------------------

(defface my/git-section '((t :weight semibold)) "A section title (CHANGES).")
(defface my/git-dim '((t)) "Folders, times and other quiet text.")
(defface my/git-badge '((t)) "The count beside a section title.")
(defface my/git-box '((t)) "The commit message box.")
(defface my/git-placeholder '((t)) "The message box when it's empty.")
(defface my/git-button '((t)) "The big button under the message box.")
(defface my/git-action '((t)) "A row's action icons.")
(defface my/git-modified '((t)) "M")
(defface my/git-added '((t)) "A and U")
(defface my/git-deleted '((t)) "D")
(defface my/git-conflict '((t)) "Merge conflicts.")
(defface my/git-hover '((t)) "An action under the mouse.")
(defface my/git-selected '((t)) "The row you're on.")

(defun my/git-apply-theme (&rest _)
  (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
         (fg (my/tab--face-color 'default :foreground "#dfe1e5"))
         (green (my/tab--face-color 'success :foreground "#6aab73"))
         (yellow (my/tab--face-color 'warning :foreground "#d5b778"))
         (red (my/tab--face-color 'error :foreground "#cf8e6d"))
         (line (my/tab--mix fg bg 0.22)))
    (set-face-attribute 'my/git-section nil :foreground (my/tab--mix fg bg 0.62) :height 0.85)
    (set-face-attribute 'my/git-dim nil :foreground (my/tab--mix fg bg 0.45))
    (set-face-attribute 'my/git-badge nil :foreground (my/tab--mix fg bg 0.85)
                        :background (my/tab--mix fg bg 0.16) :height 0.85)
    (set-face-attribute 'my/git-box nil :background (my/tab--mix fg bg 0.05)
                        :box (list :line-width '(1 . 1) :color line))
    (set-face-attribute 'my/git-placeholder nil :foreground (my/tab--mix fg bg 0.4))
    (set-face-attribute 'my/git-button nil :foreground fg
                        :background (my/tab--mix fg bg 0.17)
                        :box (list :line-width '(1 . 4) :color (my/tab--mix fg bg 0.17)))
    (set-face-attribute 'my/git-action nil :foreground (my/tab--mix fg bg 0.7))
    (set-face-attribute 'my/git-selected nil :background (my/tab--mix fg bg 0.12) :extend t)
    (set-face-attribute 'my/git-hover nil :foreground fg :background (my/tab--mix fg bg 0.14))
    ;; softened so nothing shouts
    (set-face-attribute 'my/git-modified nil :foreground (my/tab--mix yellow bg 0.85))
    (set-face-attribute 'my/git-added nil :foreground (my/tab--mix green bg 0.85))
    (set-face-attribute 'my/git-deleted nil :foreground (my/tab--mix red bg 0.7))
    (set-face-attribute 'my/git-conflict nil :foreground (my/tab--mix red bg 0.9) :weight 'bold)))

(defun my/git--code-face (code)
  (pcase code
    ((or "A" "U" "C") 'my/git-added)
    ("D" 'my/git-deleted)
    ("!" 'my/git-conflict)
    (_ 'my/git-modified)))

(defun my/git--icon (name fallback)
  (or (ignore-errors (nerd-icons-codicon name)) fallback))

(defun my/git--glyph (icon &optional face)
  "ICON (from nerd-icons) as plain text in its font, with FACE on top."
  (let ((family (plist-get (get-text-property 0 'face icon) :family)))
    (propertize (substring-no-properties icon)
                'face `(:family ,(or family 'unspecified) :height 1.0
                        ,@(when face (list :inherit face))))))

(defun my/git--file-icon (path)
  (or (ignore-errors
        (let ((icon (nerd-icons-icon-for-file (file-name-nondirectory path))))
          (propertize icon 'face (list :inherit (get-text-property 0 'face icon) :height 0.95))))
      " "))

(defun my/git--right (string)
  "STRING pushed against the right edge, one column in (measured in pixels)."
  (concat (propertize " " 'display
                      `(space :align-to (- right (,(+ (string-pixel-width string)
                                                       (frame-char-width))))))
          string))

(defun my/git--cols (string)
  "Columns STRING takes on screen, rounded up."
  (ceiling (string-pixel-width string) (frame-char-width)))

;;; rows that show their actions when you're on them
;; ---------------------------------------------------------------------

;; Every row is rendered twice: plainly, and with its action icons.
;; Only one shows at a time; `buffer-invisibility-spec' hides the
;; actions of every row (+N) except the active one, whose plain version
;; (-N) is hidden instead.

(defvar-local my/git--rows 0 "How many rows have action versions.")
(defvar-local my/git--active nil "The row showing its actions.")

(defun my/git--actions (actions)
  "ACTIONS, each (ICON HELP FUNCTION), as clickable icons."
  (mapconcat (lambda (action)
               (pcase-let ((`(,icon ,help ,fn) action))
                 (propertize (concat " " (my/git--glyph icon 'my/git-action) " ")
                             'my/git-click fn 'help-echo help 'mouse-face 'my/git-hover)))
             actions ""))

(defun my/git--two-ways (plain active)
  "PLAIN and ACTIVE versions of a row; only one shows at a time."
  (let ((n (cl-incf my/git--rows)))
    (concat (propertize plain 'invisible (- n))
            (propertize active 'invisible n))))

(defun my/git--row-number-at (pos)
  (let ((inv (get-text-property pos 'invisible)))
    (when (integerp inv) (abs inv))))

(defun my/git--activate (n)
  "Show row N's actions (and hide the previous row's)."
  (unless (eq n my/git--active)
    (let ((spec (cl-remove-if #'integerp buffer-invisibility-spec)))
      (setq buffer-invisibility-spec
            (append spec (cl-loop for i from 1 to my/git--rows
                                  collect (if (eql i n) (- i) i))))
      (setq my/git--active n))))

(defun my/git--follow-point ()
  "Show the actions of the row point is on."
  (my/git--activate (save-excursion
                      (let ((pos (line-beginning-position)))
                        (or (my/git--row-number-at pos)
                            (let ((next (next-single-property-change pos 'invisible nil
                                                                     (line-end-position))))
                              (and next (< next (line-end-position))
                                   (my/git--row-number-at next))))))))

(defun my/git--hover (window _object pos)
  "Show the actions of the row under the mouse (from `help-echo')."
  (when (and (window-live-p window) pos)
    (with-current-buffer (window-buffer window)
      (when-let ((n (my/git--row-number-at pos)))
        (my/git--activate n))
      (get-text-property pos 'my/git-tip))))

;;; drawing
;; ---------------------------------------------------------------------

(defvar-local my/git--root nil "The repository this panel shows.")
(defvar-local my/git--data nil "The last `my/git--status'.")
(defvar-local my/git--folded '(stashes) "Sections folded shut.")

(defun my/git--width ()
  (if-let ((window (get-buffer-window (current-buffer) t)))
      (window-max-chars-per-line window)
    34))

(defun my/git--fill (string prop value)
  "Give STRING's parts that have no PROP the VALUE (inner ones win)."
  (let ((i 0) (n (length string)))
    (while (< i n)
      (let ((next (next-single-property-change i prop string n)))
        (unless (get-text-property i prop string)
          (put-text-property i next prop value string))
        (setq i next))))
  string)

(defun my/git--line (content &rest props)
  "Insert CONTENT as one line with PROPS where its parts don't have their own."
  (let ((string (concat content "\n")))
    (my/git--fill string 'help-echo #'my/git--hover)
    (while props
      (my/git--fill string (pop props) (pop props)))
    (insert string)))

(defun my/git--truncate (string width)
  (if (<= (string-width string) width) string
    (truncate-string-to-width string (max 0 width) nil nil "…")))

(defun my/git--message-text ()
  (when-let ((buffer (my/git--message-buffer my/git--root t)))
    (with-current-buffer buffer (string-trim (buffer-string)))))

(defun my/git--top (data width)
  "The branch line, message box and big button."
  (let* ((branch (plist-get data :branch))
         (ahead (plist-get data :ahead)) (behind (plist-get data :behind))
         (busy (gethash my/git--root my/git--busy))
         (staged (plist-get data :staged)) (changes (plist-get data :changes))
         (message (my/git--message-text))
         (inner (- width 4)))
    ;; branch
    (my/git--line
     (concat " " (my/git--glyph (my/git--icon "nf-cod-git_branch" "⎇") 'my/git-dim) " "
             (propertize (or branch "?") 'face 'default)
             (cond ((not (plist-get data :upstream))
                    (propertize "  not published" 'face 'my/git-dim))
                   ((and (zerop ahead) (zerop behind)) "")
                   (t (concat "  "
                              (propertize (string-trim
                                           (concat (unless (zerop behind) (format "↓%d " behind))
                                                   (unless (zerop ahead) (format "↑%d" ahead))))
                                          'face 'my/git-dim 'mouse-face 'my/git-hover
                                          'my/git-click #'my/git-sync
                                          'help-echo "Sync: pull, then push"))))
             (if busy (my/git--right (propertize busy 'face 'my/git-dim)) ""))
     'my/git-row-action #'my/git-switch-branch
     'mouse-face 'my/git-hover
     'my/git-tip "Click to switch branch")
    (insert "\n")
    ;; message box
    (let ((shown (cond ((my/git--message-window) "Writing the message above…")
                       ((and message (not (string-empty-p message)))
                        (car (split-string message "\n")))
                       (t "Message  (c)"))))
      (insert " ")
      (my/git--line
       (propertize (string-pad (concat " " (my/git--truncate shown (- inner 3))) inner)
                   'face (list (if (and message (not (string-empty-p message))
                                        (not (my/git--message-window)))
                                   'default 'my/git-placeholder)
                               'my/git-box))
       'my/git-row-action #'my/git-edit-message))
    (insert "\n")
    ;; the big button does the next sensible thing
    (pcase-let ((`(,label ,fn) (my/git--main-action data staged changes message)))
      (insert " ")
      (let* ((pad (max 0 (- (* inner (frame-char-width)) (string-pixel-width label))))
             (text (concat (propertize " " 'display `(space :width (,(/ pad 2))))
                           label
                           (propertize " " 'display `(space :width (,(- pad (/ pad 2)))))))) 
        ;; the button's face goes under the icon's own (its font)
        (add-face-text-property 0 (length text) 'my/git-button t text)
        (my/git--line (propertize text 'mouse-face 'my/git-hover)
                      'my/git-row-action fn)))
    (insert "\n")))

(defun my/git--main-action (data staged changes message)
  "(LABEL FUNCTION) for the button under the message box."
  (let ((ahead (plist-get data :ahead)) (behind (plist-get data :behind))
        (check (my/git--glyph (my/git--icon "nf-cod-check" "✓"))))
    (cond
     ((or staged changes (and message (not (string-empty-p message))))
      (list (concat check (if (or staged (null changes)) " Commit" " Commit all"))
            #'my/git-commit))
     ((null (plist-get data :remotes)) (list (concat check " Commit") #'my/git-commit))
     ((not (plist-get data :upstream))
      (list (concat (my/git--glyph (my/git--icon "nf-cod-cloud_upload" "↑")) " Publish branch")
            #'my/git-push))
     ((or (> ahead 0) (> behind 0))
      (list (concat (my/git--glyph (my/git--icon "nf-cod-sync" "⟳"))
                    (format " Sync changes  %s%s"
                            (if (> behind 0) (format "%d↓ " behind) "")
                            (if (> ahead 0) (format "%d↑" ahead) "")))
            #'my/git-sync))
     (t (list (concat check " Commit") #'my/git-commit)))))

(defun my/git--section (id title count actions)
  "A section title row; ACTIONS show when you're on it."
  (let* ((folded (memq id my/git--folded))
         (arrow (my/git--glyph (my/git--icon (if folded "nf-cod-chevron_right" "nf-cod-chevron_down")
                                             (if folded "›" "⌄"))
                               'my/git-section))
         (head (concat " " arrow " " (propertize title 'face 'my/git-section)))
         (badge (if count (propertize (format " %d " count) 'face 'my/git-badge) "")))
    (my/git--line
     (my/git--two-ways (concat head (my/git--right badge))
                       (concat head (my/git--right (concat (my/git--actions actions) badge))))
     'my/git-section id 'my/git-row-action (lambda () (interactive) (my/git-toggle-section id)))))

(defun my/git--file-row (entry kind width)
  "A changed file: icon, name, folder, letter; actions when you're on it."
  (let* ((path (plist-get entry :path))
         (code (plist-get entry :code))
         (name (file-name-nondirectory (directory-file-name path)))
         (dir (directory-file-name (or (file-name-directory path) "")))
         (actions (pcase kind
                    ('staged `((,(my/git--icon "nf-cod-go_to_file" "↗") "Open file" my/git-open)
                               (,(my/git--icon "nf-cod-remove" "−") "Unstage" my/git-unstage)))
                    ('conflicts `((,(my/git--icon "nf-cod-go_to_file" "↗") "Open file" my/git-open)
                                  (,(my/git--icon "nf-cod-add" "+") "Mark resolved (stage)" my/git-stage)))
                    (_ `((,(my/git--icon "nf-cod-go_to_file" "↗") "Open file" my/git-open)
                         (,(my/git--icon "nf-cod-discard" "↶") "Discard changes" my/git-discard)
                         (,(my/git--icon "nf-cod-add" "+") "Stage" my/git-stage)))))
         (letter (propertize code 'face (my/git--code-face code)))
         (room (- width 9))               ; indent, icon, letter, edges
         (name-shown (my/git--truncate name room))
         (name-face (if (equal code "D") '(:strike-through t) 'default))
         (head (concat "   " (my/git--file-icon path) " " (propertize name-shown 'face name-face)))
         (buttons (my/git--actions actions))
         (dir-room (- room (string-width name-shown) 1))
         (dir-room-active (- dir-room (my/git--cols buttons) -1)))
    (my/git--line
     (my/git--two-ways
      (concat head (if (and (> dir-room 2) (not (string-empty-p dir)))
                       (propertize (concat " " (my/git--truncate dir dir-room)) 'face 'my/git-dim)
                     "")
              (my/git--right letter))
      (concat head (if (and (> dir-room-active 2) (not (string-empty-p dir)))
                       (propertize (concat " " (my/git--truncate dir dir-room-active)) 'face 'my/git-dim)
                     "")
              (my/git--right (concat buttons letter))))
     'my/git-entry entry 'my/git-kind kind
     'my/git-row-action #'my/git-show-diff
     'mouse-face 'hl-line)))

(defun my/git--render ()
  "Draw this panel from `my/git--data'."
  (let* ((inhibit-read-only t)
         (data my/git--data)
         (width (my/git--width))
         (window (get-buffer-window (current-buffer) t))
         (here (if window (window-point window) (point)))
         (key (my/git--key-at here))
         (line (line-number-at-pos here))
         (start (and window (window-start window))))
    (erase-buffer)
    (setq my/git--rows 0 my/git--active nil)
    (insert "\n")
    (if (not (plist-get data :branch))
        (insert (propertize "  Not a git repository.\n\n" 'face 'my/git-dim)
                (propertize "  Press I to start one here.\n" 'face 'my/git-dim))
      (my/git--top data width)
      (let ((conflicts (plist-get data :conflicts))
            (staged (plist-get data :staged))
            (changes (plist-get data :changes))
            (stage-all `(,(my/git--icon "nf-cod-add" "+") "Stage all" my/git-stage-all))
            (unstage-all `(,(my/git--icon "nf-cod-remove" "−") "Unstage all" my/git-unstage-all))
            (discard-all `(,(my/git--icon "nf-cod-discard" "↶") "Discard all changes" my/git-discard-all)))
        (when conflicts
          (my/git--section 'conflicts "MERGE CHANGES" (length conflicts) nil)
          (unless (memq 'conflicts my/git--folded)
            (dolist (e conflicts) (my/git--file-row e 'conflicts width))))
        (when staged
          (my/git--section 'staged "STAGED CHANGES" (length staged) (list unstage-all))
          (unless (memq 'staged my/git--folded)
            (dolist (e staged) (my/git--file-row e 'staged width))))
        (my/git--section 'changes "CHANGES" (length changes)
                         (and changes (list discard-all stage-all)))
        (unless (memq 'changes my/git--folded)
          (if changes
              (progn
                (dolist (e (seq-take changes my/git-max-rows)) (my/git--file-row e 'changes width))
                (when (> (length changes) my/git-max-rows)
                  (insert (propertize (format "   … and %d more (a .gitignore missing?)\n"
                                              (- (length changes) my/git-max-rows))
                                      'face 'my/git-dim))))
            (unless (or staged conflicts)
              (insert (propertize "   Nothing changed\n" 'face 'my/git-dim)))))
        (insert "\n")
        (when-let ((stashes (plist-get data :stashes)))
          (my/git--section 'stashes "STASHES" (length stashes) nil)
          (unless (memq 'stashes my/git--folded)
            (dolist (s stashes)
              (my/git--line
               (concat "   " (my/git--glyph (my/git--icon "nf-cod-archive" "▤") 'my/git-dim) " "
                       (my/git--truncate (plist-get s :subject) (- width 6)))
               'my/git-stash s 'my/git-row-action #'my/git-show-stash 'mouse-face 'hl-line)))
          (insert "\n"))
        (when-let ((commits (plist-get data :commits)))
          (my/git--section 'commits "RECENT COMMITS" nil nil)
          (unless (memq 'commits my/git--folded)
            (dolist (c commits)
              (let* ((ago (plist-get c :ago))
                     (room (- width 6 (string-width ago) 2)))
                (my/git--line
                 (concat "   " (my/git--glyph (my/git--icon "nf-cod-git_commit" "○") 'my/git-dim) " "
                         (my/git--truncate (plist-get c :subject) room)
                         (my/git--right (propertize ago 'face 'my/git-dim)))
                 'my/git-commit c 'my/git-row-action #'my/git-show-commit 'mouse-face 'hl-line
                 'my/git-tip (format "%s  %s" (plist-get c :hash) (plist-get c :author)))))))))
    ;; put the cursor back where it was
    (goto-char (or (and key (my/git--find-key key))
                   (progn (goto-char (point-min)) (forward-line (1- line)) (point))))
    ;; every row's action version starts hidden
    (setq buffer-invisibility-spec
          (cons t (number-sequence 1 my/git--rows))
          my/git--active nil)
    (when window
      (set-window-point window (point))
      (when start (set-window-start window (min start (point-max)) t)))
    (my/git--follow-point)))

(defun my/git--key-at (pos)
  "What's at POS, to find it again after a redraw."
  (or (when-let ((e (get-text-property pos 'my/git-entry)))
        (list 'file (get-text-property pos 'my/git-kind) (plist-get e :path)))
      (when-let ((s (get-text-property pos 'my/git-section))) (list 'section s))
      (when-let ((c (get-text-property pos 'my/git-commit))) (list 'commit (plist-get c :hash)))))

(defun my/git--find-key (key)
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (when (equal (my/git--key-at (point)) key) (setq found (point)))
        (forward-line 1))
      found)))

;;; the panel buffer
;; ---------------------------------------------------------------------

(defvar my/git--timer nil)

(defun my/git--buffers ()
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'my/git-mode)) (buffer-list)))

(defun my/git-refresh (&rest _)
  "Read the repository again and redraw."
  (interactive)
  (when (derived-mode-p 'my/git-mode)
    (setq my/git--data (my/git--status my/git--root))
    (my/git--render)))

(defun my/git--render-root (root)
  "Refresh every panel showing ROOT."
  (dolist (buffer (my/git--buffers))
    (with-current-buffer buffer
      (when (equal my/git--root root) (my/git-refresh)))))

(defun my/git--tick ()
  (let ((shown (seq-filter (lambda (b) (get-buffer-window b t)) (my/git--buffers))))
    (if (null shown)
        (progn (cancel-timer my/git--timer) (setq my/git--timer nil))
      ;; not while you're typing in the minibuffer or the message box
      (unless (or (minibufferp) (active-minibuffer-window))
        (dolist (b shown) (with-current-buffer b (my/git-refresh)))))))

(defun my/git--after-save ()
  "Refresh panels for the repository a saved file is in."
  (when buffer-file-name
    (let ((file (file-truename buffer-file-name)))
      (dolist (b (my/git--buffers))
        (when (and (get-buffer-window b t)
                   (string-prefix-p (file-truename (buffer-local-value 'my/git--root b)) file))
          (with-current-buffer b (my/git-refresh)))))))

(add-hook 'after-save-hook #'my/git--after-save)

(defvar my/git-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'my/git-click)
    (define-key map [mouse-3] #'my/git-context-menu)
    (define-key map [down-mouse-3] #'ignore)
    map))

(define-derived-mode my/git-mode special-mode "Source Control"
  "The Source Control panel."
  (setq-local truncate-lines t
              cursor-type nil
              evil-normal-state-cursor '(nil)
              evil-insert-state-cursor '(nil)
              revert-buffer-function #'my/git-refresh
              buffer-invisibility-spec (list t)
              line-spacing 0.15)
  (display-line-numbers-mode -1)
  (face-remap-add-relative 'hl-line 'my/git-selected)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'my/git--follow-point nil t)
  (add-hook 'window-size-change-functions #'my/git--resized nil t))

(defun my/git--resized (window)
  (with-current-buffer (window-buffer window)
    (when (derived-mode-p 'my/git-mode) (my/git--render))))

(defun my/git--buffer (root)
  "The Source Control buffer for ROOT."
  (let ((buffer (get-buffer-create
                 (format "*Source Control · %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/git-mode) (my/git-mode))
      (setq my/git--root root default-directory root)
      (my/git-refresh))
    (unless my/git--timer
      (setq my/git--timer (run-at-time my/git-interval my/git-interval #'my/git--tick)))
    buffer))

(defun my/git--panel-root ()
  (or (and (derived-mode-p 'my/git-mode) my/git--root)
      (and (derived-mode-p 'my/git-message-mode) my/git--message-root)
      (seq-some (lambda (w) (and (window-parameter w 'my-activity-git)
                                 (buffer-local-value 'my/git--root (window-buffer w))))
                (window-list nil 'nomini))
      (my/git-root)
      (user-error "Not in a git repository")))

;;; clicking
;; ---------------------------------------------------------------------

(defun my/git-click (event)
  "Do what was clicked: an action icon, or the row's own thing."
  (interactive "e")
  (let* ((posn (event-start event))
         (pos (posn-point posn)))
    (select-window (posn-window posn))
    (when pos
      (goto-char pos)
      (my/git--follow-point)
      (let ((fn (or (get-text-property pos 'my/git-click)
                    (get-text-property pos 'my/git-row-action))))
        (when fn (call-interactively fn))))))

(defun my/git-context-menu (event)
  "Right click: what you can do with this row."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (select-window (posn-window (event-start event)))
    (when pos (goto-char pos) (my/git--follow-point))
    (popup-menu (my/menu "Source Control" (my/git--menu-items)) event)))

(defun my/git--menu-items ()
  (let ((entry (get-text-property (point) 'my/git-entry))
        (kind (get-text-property (point) 'my/git-kind)))
    `(,@(when entry
          `(("Show the changes" my/git-show-diff :keys "RET")
            ("Open the file" my/git-open :keys "o")
            ,(if (eq kind 'staged) '("Unstage" my/git-unstage :keys "u")
               '("Stage" my/git-stage :keys "s"))
            ,(unless (eq kind 'staged) '("Discard changes" my/git-discard :keys "x"))
            "--"))
      ("Commit" my/git-commit :keys "c")
      ("Amend the last commit" my/git-amend :keys "C")
      ("Undo the last commit" my/git-undo-commit :keys "X")
      "--"
      ("Pull" my/git-pull :keys "F")
      ("Push" my/git-push :keys "P")
      ("Sync (pull then push)" my/git-sync)
      ("Fetch" my/git-fetch :keys "f")
      "--"
      ("Switch branch…" my/git-switch-branch :keys "b")
      ("New branch…" my/git-new-branch :keys "B")
      ("Delete a branch…" my/git-delete-branch)
      "--"
      ("Stash changes…" my/git-stash :keys "z")
      ("Pop the last stash" my/git-stash-pop :keys "Z")
      "--"
      ("Stage all" my/git-stage-all :keys "S")
      ("Unstage all" my/git-unstage-all :keys "U")
      ("Git's output" my/git-show-output :keys "$")
      ("Refresh" my/git-refresh :keys "g"))))

(defun my/git-menu (&optional _window)
  "The ⋯ menu of the panel's title bar."
  (interactive)
  (popup-menu (my/menu "Source Control" (my/git--menu-items))))

;;; files
;; ---------------------------------------------------------------------

(defun my/git--entry ()
  (or (get-text-property (point) 'my/git-entry)
      (user-error "Not on a file")))

(defun my/git--paths (entry)
  (delq nil (list (plist-get entry :path) (plist-get entry :orig))))

(defun my/git-stage ()
  "Stage the file at point."
  (interactive)
  (let ((default-directory my/git--root))
    (apply #'my/git--do "add" "-A" "--" (my/git--paths (my/git--entry)))
    (my/git-refresh)))

(defun my/git-unstage ()
  "Unstage the file at point."
  (interactive)
  (let ((default-directory my/git--root)
        (entry (my/git--entry)))
    (if (equal (plist-get my/git--data :oid) "(initial)")
        (apply #'my/git--do "rm" "--cached" "-q" "--" (my/git--paths entry))
      (apply #'my/git--do "restore" "--staged" "--" (my/git--paths entry)))
    (my/git-refresh)))

(defun my/git-discard ()
  "Throw away the changes to the file at point (an untracked file goes to the trash)."
  (interactive)
  (let* ((default-directory my/git--root)
         (entry (my/git--entry))
         (path (plist-get entry :path)))
    (when (eq (get-text-property (point) 'my/git-kind) 'staged)
      (user-error "Unstage it first (u)"))
    (when (yes-or-no-p (if (equal (plist-get entry :code) "U")
                           (format "Move the new file %s to the trash? " path)
                         (format "Discard your changes to %s? " path)))
      (if (equal (plist-get entry :code) "U")
          (let ((delete-by-moving-to-trash t)) (delete-file (expand-file-name path) t))
        (my/git--do "restore" "--worktree" "--" path))
      (my/git--revert-file (expand-file-name path))
      (my/git-refresh))))

(defun my/git--revert-file (file)
  (when-let ((buffer (get-file-buffer file)))
    (with-current-buffer buffer
      (if (file-exists-p file) (revert-buffer t t t) (set-buffer-modified-p nil)))))

(defun my/git-stage-all ()
  (interactive)
  (let ((default-directory (my/git--panel-root)))
    (my/git--do "add" "-A")
    (my/git--render-root default-directory)))

(defun my/git-unstage-all ()
  (interactive)
  (let ((default-directory (my/git--panel-root)))
    (if (equal (plist-get my/git--data :oid) "(initial)")
        (my/git--do "rm" "-r" "--cached" "-q" ".")
      (my/git--do "reset" "-q"))
    (my/git--render-root default-directory)))

(defun my/git-discard-all ()
  "Discard every unstaged change; new files go to the trash."
  (interactive)
  (let* ((default-directory (my/git--panel-root))
         (changes (plist-get my/git--data :changes))
         (new (seq-filter (lambda (e) (equal (plist-get e :code) "U")) changes)))
    (when (and changes
               (yes-or-no-p (format "Discard all %d changes%s? "
                                    (length changes)
                                    (if new (format " (%d new files go to the trash)" (length new)) ""))))
      (my/git--do "restore" "--worktree" "--" ".")
      (let ((delete-by-moving-to-trash t))
        (dolist (e new) (delete-file (expand-file-name (plist-get e :path)) t)))
      (dolist (e changes) (my/git--revert-file (expand-file-name (plist-get e :path))))
      (my/git-refresh))))

(defun my/git-open ()
  "Open the file at point in the code area."
  (interactive)
  (let ((file (expand-file-name (plist-get (my/git--entry) :path) my/git--root)))
    (unless (file-exists-p file) (user-error "It's been deleted"))
    (select-window (or (my/git--editor-window) (selected-window)))
    (find-file file)))

;;; diffs
;; ---------------------------------------------------------------------

(defun my/git--editor-window ()
  (if (fboundp 'my/activity--editor-window) (my/activity--editor-window)
    (next-window)))

(defun my/git--show (title root args &optional focus)
  "Show git ARGS' output in diff-mode in the code area, titled TITLE."
  (let* ((default-directory root)
         (output (cdr (apply #'my/git--call args)))
         (buffer (get-buffer-create "*git diff*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (string-empty-p (string-trim output)) "No differences.\n" output))
        (goto-char (point-min)))
      (diff-mode)
      (setq default-directory root
            buffer-read-only t
            header-line-format (concat " " (propertize title 'face 'my/git-section)))
      (when (fboundp 'evil-define-key)
        (evil-define-key 'normal 'local "q" #'quit-window)))
    (let ((window (my/git--editor-window)))
      (when window
        (set-window-buffer window buffer)
        (set-window-point window 1)
        (when focus (select-window window))))
    buffer))

(defun my/git-show-diff ()
  "Show the changes of the file at point (the cursor stays in the panel)."
  (interactive)
  (let* ((entry (my/git--entry))
         (kind (get-text-property (point) 'my/git-kind))
         (path (plist-get entry :path)))
    (my/git--show
     (format "%s · %s" path (pcase kind ('staged "staged") ('conflicts "conflict") (_ "changes")))
     my/git--root
     (cond ((eq kind 'staged) (list "diff" "--cached" "-M" "--" path (or (plist-get entry :orig) path)))
           ((equal (plist-get entry :code) "U") (list "diff" "--no-index" "--" "/dev/null" path))
           (t (list "diff" "--" path))))))

(defun my/git-show-commit ()
  "Show the commit at point."
  (interactive)
  (let ((c (or (get-text-property (point) 'my/git-commit) (user-error "Not on a commit"))))
    (my/git--show (format "%s  %s" (plist-get c :hash) (plist-get c :subject))
                  my/git--root (list "show" "--stat" "-p" (plist-get c :hash)))))

(defun my/git-show-stash ()
  (interactive)
  (let ((s (or (get-text-property (point) 'my/git-stash) (user-error "Not on a stash"))))
    (my/git--show (plist-get s :subject) my/git--root
                  (list "stash" "show" "-p" "--include-untracked" (plist-get s :ref)))))

(defun my/git-show-output ()
  "What git last said."
  (interactive)
  (let ((buffer (get-buffer-create my/git-output-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (goto-char (point-max)))
    (select-window (or (my/git--editor-window) (selected-window)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;; the commit message
;; ---------------------------------------------------------------------

(defvar-local my/git--message-root nil)
(defvar-local my/git--amend nil "Non-nil when this message amends the last commit.")

(defvar my/git-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my/git-commit)
    (define-key map (kbd "C-c C-k") #'my/git-message-close)
    map))

(define-derived-mode my/git-message-mode text-mode "Commit message"
  "Write a commit message: C-c C-c commits, C-c C-k puts it away (kept)."
  (setq-local header-line-format '(:eval (my/git--message-header)))
  (display-line-numbers-mode -1)
  (visual-line-mode 1)
  (add-hook 'after-change-functions
            (lambda (&rest _) (my/git--soon-render my/git--message-root)) nil t))

(defun my/git--message-header ()
  (let ((title (concat " " (propertize (if my/git--amend "AMEND LAST COMMIT" "MESSAGE")
                                       'face 'my/git-section))))
    (if (fboundp 'my/panel--button)
        (concat title
                (apply #'concat
                       (my/panel--right
                        (concat (my/panel--button (my/git--icon "nf-cod-check" "✓")
                                                  "Commit  (C-c C-c)"
                                                  (lambda (w) (with-selected-window w (my/git-commit))))
                                (my/panel--button (my/git--icon "nf-cod-close" "✕")
                                                  "Put it away, kept for later  (C-c C-k)"
                                                  (lambda (w) (with-selected-window w (my/git-message-close))))))))
      (concat title (propertize "  C-c C-c commit  C-c C-k close" 'face 'my/git-dim)))))

(defvar my/git--render-timer nil)

(defun my/git--soon-render (root)
  "Redraw ROOT's panel shortly (the message box follows what you type)."
  (when my/git--render-timer (cancel-timer my/git--render-timer))
  (setq my/git--render-timer
        (run-with-idle-timer 0.3 nil
                             (lambda ()
                               (dolist (b (my/git--buffers))
                                 (with-current-buffer b
                                   (when (equal my/git--root root) (my/git--render))))))))

(defun my/git--message-buffer (root &optional existing)
  "ROOT's commit message buffer (nil when EXISTING and there isn't one)."
  (let ((name (format " *commit message · %s*" root)))
    (if existing (get-buffer name)
      (let ((buffer (get-buffer-create name)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'my/git-message-mode) (my/git-message-mode))
          (setq my/git--message-root root default-directory root))
        buffer))))

(defun my/git--message-window ()
  (seq-find (lambda (w) (window-parameter w 'my-git-message)) (window-list nil 'nomini)))

(defun my/git-edit-message (&optional amend)
  "Write the commit message in a box above the panel."
  (interactive)
  (let* ((root (my/git--panel-root))
         (buffer (my/git--message-buffer root))
         (panel (seq-find (lambda (w) (window-parameter w 'my-activity-git)) (window-list nil 'nomini)))
         (window (or (my/git--message-window)
                     (if panel
                         (let ((w (split-window panel -7 'above)))
                           (set-window-parameter w 'my-git-message t)
                           (set-window-parameter w 'mode-line-format 'none)
                           (set-window-parameter w 'no-delete-other-windows t)
                           w)
                       (display-buffer buffer '(display-buffer-at-bottom (window-height . 8)))))))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (with-current-buffer buffer
      (when (and amend (not my/git--amend))
        (setq my/git--amend t)
        (let ((default-directory root))
          (erase-buffer)
          (insert (string-trim-right (or (my/git--out "log" "-1" "--format=%B") ""))))))
    (select-window window)
    (goto-char (point-max))
    (when (fboundp 'evil-insert-state) (evil-insert-state))
    (message "C-c C-c commits · C-c C-k puts it away (kept)")
    (my/git--render-root root)))

(defun my/git-message-close ()
  "Put the message box away; what you wrote is kept."
  (interactive)
  (let ((root my/git--message-root))
    (when-let ((window (my/git--message-window)))
      (delete-window window))
    (when (fboundp 'evil-normal-state) (evil-normal-state))
    (when-let ((panel (seq-find (lambda (w) (window-parameter w 'my-activity-git)) (window-list nil 'nomini))))
      (select-window panel))
    (when root (my/git--render-root root))))

(defun my/git-close-message-window ()
  "Close the message box (the git panel is going)."
  (when-let ((window (my/git--message-window))) (delete-window window)))

(defun my/git--tidy ()
  "The message box goes when its panel does."
  (when (and (my/git--message-window)
             (not (seq-find (lambda (w) (window-parameter w 'my-activity-git))
                            (window-list nil 'nomini))))
    (run-at-time 0 nil #'my/git-close-message-window)))

(add-hook 'window-configuration-change-hook #'my/git--tidy)

(defun my/git-amend ()
  "Change the last commit's message (and add what's staged to it)."
  (interactive)
  (my/git-edit-message t))

(defun my/git-commit ()
  "Commit what's staged with the message from the box.
Nothing staged: offers to commit every change.  No message yet: opens the box."
  (interactive)
  (let* ((root (my/git--panel-root))
         (buffer (my/git--message-buffer root))
         (data (my/git--status root))
         (message (with-current-buffer buffer (string-trim (buffer-string))))
         (amend (buffer-local-value 'my/git--amend buffer))
         (default-directory root))
    (cond
     ((plist-get data :conflicts) (user-error "Resolve the merge conflicts first"))
     ((and (not amend) (null (plist-get data :staged)) (null (plist-get data :changes)))
      (user-error "Nothing to commit"))
     ((string-empty-p message) (my/git-edit-message))
     (t
      (when (and (not amend) (null (plist-get data :staged)))
        (unless (y-or-n-p (format "Nothing is staged.  Commit all %d changes? "
                                  (length (plist-get data :changes))))
          (user-error "Stage what you want to commit (s)"))
        (my/git--do "add" "-A"))
      (let ((file (make-temp-file "commit-msg")))
        (with-temp-file file (insert message "\n"))
        (my/git--async root (if amend "Amending…" "Committing…")
                       (append (list "commit" "-F" file) (when amend (list "--amend")))
                       (lambda (ok)
                         (delete-file file)
                         (when ok
                           (with-current-buffer buffer
                             (erase-buffer)
                             (setq my/git--amend nil))
                           (my/git--after-commit)))))))))

(defun my/git--after-commit ()
  "Put the message box away; back to the panel if you were typing in it."
  (when-let ((window (my/git--message-window)))
    (let ((was-here (eq window (selected-window))))
      (delete-window window)
      (when-let ((panel (and was-here
                             (seq-find (lambda (w) (window-parameter w 'my-activity-git))
                                       (window-list nil 'nomini)))))
        (select-window panel)
        (when (fboundp 'evil-normal-state) (evil-normal-state))))))

(defun my/git-undo-commit ()
  "Undo the last commit, keeping its changes staged."
  (interactive)
  (let* ((root (my/git--panel-root))
         (default-directory root)
         (data (my/git--status root))
         (subject (or (my/git--out "log" "-1" "--format=%s") ""))
         (pushed (and (plist-get data :upstream) (zerop (plist-get data :ahead)))))
    (when (equal (plist-get data :oid) "(initial)") (user-error "No commits yet"))
    (when (yes-or-no-p (format "Undo \"%s\"%s? "
                               (string-trim subject)
                               (if pushed " (it's already pushed: pushing again will need --force)" "")))
      (my/git--do "reset" "--soft" "HEAD~1")
      ;; the message comes back into the box
      (with-current-buffer (my/git--message-buffer root)
        (erase-buffer)
        (insert (string-trim-right (or (my/git--out "log" "-1" "--format=%B" "HEAD@{1}") ""))))
      (my/git--render-root root))))

;;; remotes
;; ---------------------------------------------------------------------

(defun my/git--remote (data)
  (let ((remotes (plist-get data :remotes)))
    (cond ((null remotes) (user-error "This repository has no remote (git remote add origin URL)"))
          ((member "origin" remotes) "origin")
          ((null (cdr remotes)) (car remotes))
          (t (completing-read "Remote: " remotes nil t)))))

(defun my/git-push ()
  "Push the branch (publishing it first time round)."
  (interactive)
  (let* ((root (my/git--panel-root))
         (data (my/git--status root))
         (branch (plist-get data :branch)))
    (when (equal branch "(detached)") (user-error "Not on a branch"))
    (if (plist-get data :upstream)
        (my/git--async root "Pushing…" (list "push"))
      (let ((remote (my/git--remote data)))
        (when (y-or-n-p (format "Publish %s to %s? " branch remote))
          (my/git--async root "Publishing…" (list "push" "-u" remote branch)))))))

(defun my/git-pull ()
  "Pull into the branch."
  (interactive)
  (let ((root (my/git--panel-root)))
    (my/git--async root "Pulling…" (list "pull"))
    (my/git--revert-after root)))

(defun my/git--revert-after (root)
  "Once ROOT's job is over, reload its open files that changed on disk."
  (let ((timer nil))
    (setq timer (run-at-time 0.5 0.5
                             (lambda ()
                               (unless (gethash root my/git--busy)
                                 (cancel-timer timer)
                                 (dolist (b (buffer-list))
                                   (with-current-buffer b
                                     (when (and buffer-file-name (not (buffer-modified-p))
                                                (string-prefix-p root buffer-file-name)
                                                (not (verify-visited-file-modtime b)))
                                       (revert-buffer t t t))))))))))

(defun my/git-sync ()
  "Pull, then push."
  (interactive)
  (let ((root (my/git--panel-root)))
    (my/git--async root "Pulling…" (list "pull")
                   (lambda (ok)
                     (when ok
                       (run-at-time 0 nil (lambda () (my/git--async root "Pushing…" (list "push")))))))
    (my/git--revert-after root)))

(defun my/git-fetch ()
  (interactive)
  (my/git--async (my/git--panel-root) "Fetching…" (list "fetch" "--all" "--prune")))

;;; branches and stashes
;; ---------------------------------------------------------------------

(defun my/git--pick (fn)
  "Run FN with its prompt in the popup when that's set up."
  (if (fboundp 'my/with-vertico) (my/with-vertico fn) (call-interactively fn)))

(defun my/git-switch-branch ()
  "Switch to a branch (a remote one gets a local branch tracking it)."
  (interactive)
  (my/git--pick #'my/git--switch-branch))

(defun my/git--switch-branch (branch)
  (interactive
   (let* ((default-directory (my/git--panel-root))
          (local (split-string (or (my/git--out "for-each-ref" "--format=%(refname:short)" "refs/heads") "") "\n" t))
          (remote (seq-remove (lambda (b) (or (string-suffix-p "/HEAD" b)
                                              (member (replace-regexp-in-string "\\`[^/]+/" "" b) local)))
                              (split-string (or (my/git--out "for-each-ref" "--format=%(refname:short)" "refs/remotes") "") "\n" t))))
     (list (completing-read "Switch to branch: " (append local remote) nil t))))
  (let ((root (my/git--panel-root)))
    (let ((default-directory root))
      (if (string-match "\\`[^/]+/\\(.+\\)\\'" branch)
          (if (member branch (split-string (or (my/git--out "for-each-ref" "--format=%(refname:short)" "refs/heads") "") "\n" t))
              (my/git--do "switch" branch)
            (my/git--do "switch" "--track" branch))
        (my/git--do "switch" branch)))
    (my/git--revert-after root)
    (my/git--render-root root)
    (message "On %s" branch)))

(defun my/git-new-branch (name)
  "Start a branch NAME here and switch to it."
  (interactive (list (read-string "New branch: ")))
  (let ((root (my/git--panel-root)))
    (when (string-empty-p (string-trim name)) (user-error "No name"))
    (let ((default-directory root)) (my/git--do "switch" "-c" (string-trim name)))
    (my/git--render-root root)))

(defun my/git-delete-branch ()
  (interactive)
  (my/git--pick #'my/git--delete-branch))

(defun my/git--delete-branch (branch)
  (interactive
   (let ((default-directory (my/git--panel-root)))
     (list (completing-read "Delete branch: "
                            (split-string (or (my/git--out "for-each-ref" "--format=%(refname:short)" "refs/heads") "") "\n" t)
                            nil t))))
  (let* ((root (my/git--panel-root)) (default-directory root))
    (when (yes-or-no-p (format "Delete the branch %s? " branch))
      (let ((result (my/git--call "branch" "-d" branch)))
        (unless (eq 0 (car result))
          (if (yes-or-no-p (format "%s isn't merged anywhere.  Delete it anyway (its commits go)? " branch))
              (my/git--do "branch" "-D" branch)
            (user-error "Kept %s" branch))))
      (my/git--render-root root))))

(defun my/git-stash (message)
  "Put every change (new files too) aside, as MESSAGE."
  (interactive (list (read-string "Stash message (optional): ")))
  (let* ((root (my/git--panel-root)) (default-directory root))
    (apply #'my/git--do "stash" "push" "--include-untracked"
           (unless (string-empty-p message) (list "-m" message)))
    (my/git--revert-after root)
    (my/git--render-root root)))

(defun my/git-stash-pop ()
  "Bring back the stash at point, or the last one."
  (interactive)
  (let* ((root (my/git--panel-root)) (default-directory root)
         (stash (get-text-property (point) 'my/git-stash)))
    (my/git--do "stash" "pop" (if stash (plist-get stash :ref) "stash@{0}"))
    (my/git--revert-after root)
    (my/git--render-root root)))

(defun my/git-init ()
  "Start a git repository in this panel's folder."
  (interactive)
  (when (and my/git--root (y-or-n-p (format "Start a git repository in %s? " my/git--root)))
    (let ((default-directory my/git--root)) (my/git--do "init"))
    (my/git-refresh)))

;;; moving around
;; ---------------------------------------------------------------------

(defun my/git-toggle-section (&optional id)
  "Fold or unfold the section at point (or ID)."
  (interactive)
  (let ((id (or id (get-text-property (point) 'my/git-section)
                (save-excursion
                  (while (and (not (bobp)) (not (get-text-property (point) 'my/git-section)))
                    (forward-line -1))
                  (get-text-property (point) 'my/git-section)))))
    (when id
      (setq my/git--folded (if (memq id my/git--folded) (delq id my/git--folded)
                             (cons id my/git--folded)))
      (my/git--render)
      (when-let ((pos (my/git--find-key (list 'section id)))) (goto-char pos)))))

(defun my/git-ret ()
  "The row's own thing: diff, fold, show the commit, edit the message..."
  (interactive)
  (if-let ((fn (get-text-property (point) 'my/git-row-action)))
      (call-interactively fn)
    (user-error "Nothing to do here")))

(defun my/git-quit ()
  (interactive)
  (if (fboundp 'my/activity-git) (my/activity-git) (quit-window)))

(defun my/git-keys ()
  (interactive)
  (message (concat "RET diff  o open  s/u stage/unstage  x discard  S/U all  TAB fold  "
                   "c commit  C amend  X undo commit  P push  F pull  f fetch  "
                   "b branch  B new  z stash  Z pop  g refresh  $ output  q close")))

;;; the panel's title-bar buttons
;; ---------------------------------------------------------------------

(defun my/git-title-buttons ()
  "(ICON HELP FUNCTION) for the panel's title bar."
  `((,(my/git--icon "nf-cod-check" "✓") "Commit  (c)"
     ,(lambda (w) (with-selected-window w (my/git-commit))))
    (,(my/git--icon "nf-cod-refresh" "↻") "Refresh  (g)"
     ,(lambda (w) (with-selected-window w (my/git-refresh))))
    (,(my/git--icon "nf-cod-ellipsis" "⋯") "More: push, pull, branches, stash…"
     ,(lambda (w) (with-selected-window w (my/git-menu))))))

;;; keys
;; ---------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/git-mode 'normal)
  (evil-set-initial-state 'my/git-message-mode 'insert)
  (evil-define-key 'normal my/git-mode-map
    (kbd "RET") #'my/git-ret
    (kbd "<return>") #'my/git-ret
    (kbd "TAB") #'my/git-toggle-section
    (kbd "<tab>") #'my/git-toggle-section
    [down-mouse-1] #'ignore
    [mouse-1] #'my/git-click
    "o" #'my/git-open
    "s" #'my/git-stage
    "u" #'my/git-unstage
    "x" #'my/git-discard
    "S" #'my/git-stage-all
    "U" #'my/git-unstage-all
    "c" #'my/git-commit
    "C" #'my/git-amend
    "X" #'my/git-undo-commit
    "P" #'my/git-push
    "F" #'my/git-pull
    "f" #'my/git-fetch
    "b" #'my/git-switch-branch
    "B" #'my/git-new-branch
    "z" #'my/git-stash
    "Z" #'my/git-stash-pop
    "I" #'my/git-init
    "g" #'my/git-refresh
    "$" #'my/git-show-output
    "q" #'my/git-quit
    "?" #'my/git-keys)
  (evil-define-key 'normal my/git-message-mode-map
    "q" #'my/git-message-close
    (kbd "RET") #'newline)
  (evil-define-key 'normal 'global
    (kbd "<leader>gc") #'my/git-commit
    (kbd "<leader>gP") #'my/git-push
    (kbd "<leader>gp") #'my/git-pull
    (kbd "<leader>gb") #'my/git-switch-branch
    (kbd "<leader>gf") #'my/git-fetch))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC g" "git")
  (which-key-add-key-based-replacements "SPC g c" "commit")
  (which-key-add-key-based-replacements "SPC g P" "push")
  (which-key-add-key-based-replacements "SPC g p" "pull")
  (which-key-add-key-based-replacements "SPC g b" "switch branch")
  (which-key-add-key-based-replacements "SPC g f" "fetch"))

(with-eval-after-load 'tabs
  (my/git-apply-theme)
  (dolist (fn '(load-theme enable-theme disable-theme))
    (advice-add fn :after #'my/git-apply-theme)))

(provide 'my-git)

;;; my-git.el ends here
