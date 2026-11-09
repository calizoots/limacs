;;; ui/tabs.el  -*- lexical-binding: t; -*-

;; JetBrains-style project tabs

;; One tab bar across the top.  Each tab is a workspace with its own window
;; layout; tabs linked to a project (see editor/projects.el) show the project
;; name, other tabs show their current buffer.
;;
;; Every tab is drawn as a small SVG so it can have an exact height, crisp
;; text, and a solid accent bar under the active tab.  Colors are derived
;; from the current theme and rebuilt on every theme change.

(require 'cl-lib)
(require 'color)
(require 'xml)
(require 'tab-bar)

(use-package all-the-icons :ensure t)

;;; settings
;; ---------------------------------------------------------------------

(defvar my/tab-height 34        "Tab height in pixels.")
(defvar my/tab-hpad 14          "Padding on each side of a tab, in pixels.")
(defvar my/tab-gap 7            "Space between icon and label, in pixels.")
(defvar my/tab-accent-height 3  "Thickness of the active tab's accent bar.")
(defvar my/tab-font-scale 0.95  "Tab label size relative to the default font.")
(defvar my/tab-max-chars 28     "Longer labels are truncated with an ellipsis.")
(defvar my/tab-accent nil
  "Accent color for the active tab, or nil to take it from the theme.")

(setq tab-bar-format '(my/tab-bar-format-unpinned
                       tab-bar-format-align-right
                       my/tab-bar-format-pinned)
      tab-bar-separator ""
      tab-bar-auto-width nil
      tab-bar-select-tab-modifiers '(super)
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-show t
      tab-bar-tab-hints nil
      tab-bar-close-button-show nil   ; we draw our own
      tab-bar-new-button-show nil
      ;; no raised outline when hovering the × (it drew a 1px line beside it)
      tab-bar-button-relief 0)

(tab-bar-mode 1)

;;; color helpers
;; ---------------------------------------------------------------------

(defun my/tab--rgb (color)
  "COLOR as (R G B) floats 0..1, or nil if it is not a usable color."
  (and (stringp color)
       (not (string-prefix-p "unspecified" color))
       (ignore-errors (color-name-to-rgb color))))

(defun my/tab--hex (color)
  (let ((rgb (my/tab--rgb color)))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) "#000000")))

(defun my/tab--mix (a b amount)
  "Blend color A into B; AMOUNT (0..1) is A's share."
  (let ((ra (my/tab--rgb a)) (rb (my/tab--rgb b)))
    (if (and ra rb)
        (apply #'color-rgb-to-hex
               (append (cl-mapcar (lambda (x y) (+ (* amount x) (* (- 1 amount) y)))
                                  ra rb)
                       '(2)))
      (or b a))))

(defun my/tab--luma (color)
  (let ((c (my/tab--rgb color)))
    (if c (+ (* 0.2126 (nth 0 c)) (* 0.7152 (nth 1 c)) (* 0.0722 (nth 2 c))) 0.0)))

(defun my/tab--face-color (face attr fallback)
  (let ((c (face-attribute face attr nil t)))
    (if (my/tab--rgb c) (my/tab--hex c) fallback)))

;; palette, filled in by `my/tab-apply-theme'
(defvar my/tab--c-active "#1e1f22")   ; active tab = editor background
(defvar my/tab--c-strip  "#2b2d30")   ; bar behind the tabs
(defvar my/tab--c-fg     "#dfe1e5")   ; active label
(defvar my/tab--c-dim    "#9da0a8")   ; inactive label
(defvar my/tab--c-accent "#3574f0")   ; active tab underline
(defvar my/tab--c-border "#1e1f22")   ; hairline under the strip
(defvar my/tab--c-running "#5fb85f")  ; dot shown while a project process runs

;;; font metrics
;; ---------------------------------------------------------------------

(defun my/tab--font-family ()
  (let ((f (face-attribute 'default :family nil t)))
    (if (stringp f) f "monospace")))

(defun my/tab--font-px ()
  "Pixel size of the tab label font."
  (let ((px (or (ignore-errors (aref (font-info (face-font 'default)) 2))
                (frame-char-height))))
    (* px my/tab-font-scale)))

(defun my/tab--text-width (string)
  "Pixel width of STRING in the tab label font."
  (round (* my/tab-font-scale
            (if (fboundp 'string-pixel-width)
                (string-pixel-width (propertize string 'face 'default))
              (* (string-width string) (frame-char-width))))))

;;; SVG pieces
;; ---------------------------------------------------------------------

(defvar my/tab--image-cache (make-hash-table :test #'equal))

(defun my/tab--image (svg)
  (or (gethash svg my/tab--image-cache)
      (puthash svg
               (create-image svg 'svg t :ascent 'center :scale 1 :pointer 'hand)
               my/tab--image-cache)))

(defun my/tab--svg (width bg active body)
  "SVG of WIDTH px with background BG, accent bar if ACTIVE, then BODY."
  (let ((h my/tab-height))
    (concat
     (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>" width h)
     (format "<rect width='%d' height='%d' fill='%s'/>" width h bg)
     (if active
         (format "<rect y='%d' width='%d' height='%d' fill='%s'/>"
                 (- h my/tab-accent-height) width my/tab-accent-height my/tab--c-accent)
       (format "<rect y='%d' width='%d' height='1' fill='%s'/>"
               (1- h) width my/tab--c-border))
     body
     "</svg>")))

(defun my/tab--baseline (size)
  "Y baseline that vertically centers a glyph of SIZE px."
  (+ (/ (- my/tab-height (if (> my/tab-accent-height 0) 1 0)) 2.0) (* 0.35 size)))

(defun my/tab--svg-text (x string family size color &optional weight)
  (format "<text x='%d' y='%.1f' font-family='%s' font-size='%.1fpx' fill='%s'%s>%s</text>"
          x (my/tab--baseline size) (xml-escape-string family) size color
          (if weight (format " font-weight='%s'" weight) "")
          (xml-escape-string string)))

(defun my/tab--icon-parts (icon)
  "Return (CHAR FAMILY COLOR) for an all-the-icons string ICON."
  (when (and (stringp icon) (> (length icon) 0))
    (let* ((face (get-text-property 0 'face icon))
           (family (plist-get face :family))
           (inherit (plist-get face :inherit))
           (color (and inherit (my/tab--face-color inherit :foreground nil))))
      (and family (list (substring-no-properties icon) family color)))))

;;; what a tab shows
;; ---------------------------------------------------------------------

(defun my/tab--project (tab)
  (let ((id (alist-get 'my-project-id (cdr tab))))
    (and id (fboundp 'my/project-by-id) (my/project-by-id id))))

(defun my/tab--project-modified-p (project)
  (let ((root (plist-get project :root)))
    (cl-some (lambda (b)
               (let ((f (buffer-file-name b)))
                 (and f (buffer-modified-p b) (string-prefix-p root f))))
             (buffer-list))))

(defun my/tab--icon (tab project)
  (ignore-errors
    (cond
     (project (all-the-icons-octicon "repo"))
     ;; the music tab from pkgs/my-music.el
     ((equal (alist-get 'name (cdr tab)) "Music") (all-the-icons-faicon "music"))
     ;; the mail tab from pkgs/my-mail.el
     ((equal (alist-get 'name (cdr tab)) "Mail") (all-the-icons-faicon "envelope"))
     ;; the services tab from runtime/my-services.el
     ((equal (alist-get 'name (cdr tab)) "Services") (all-the-icons-faicon "server"))
     (t
      (let ((buf (get-buffer (alist-get 'name (cdr tab)))))
        (when buf
          (with-current-buffer buf
            (if buffer-file-name
                (all-the-icons-icon-for-file buffer-file-name)
              (all-the-icons-icon-for-mode major-mode)))))))))

(defun my/tab--label (tab project)
  (let ((s (if project (plist-get project :name) (alist-get 'name (cdr tab)))))
    (if (> (length s) my/tab-max-chars)
        (concat (substring s 0 (1- my/tab-max-chars)) "…")
      s)))

(defun my/tab--modified-p (tab project)
  (if project
      (my/tab--project-modified-p project)
    (let ((buf (get-buffer (alist-get 'name (cdr tab)))))
      (and buf (buffer-file-name buf) (buffer-modified-p buf)))))

;;; tab rendering
;; ---------------------------------------------------------------------

(defun my/tab-render (tab)
  "Propertized string drawing TAB."
  (let* ((active   (eq (car tab) 'current-tab))
         (project  (my/tab--project tab))
         (label    (my/tab--label tab project))
         (modified (my/tab--modified-p tab project))
         (bg       (if active my/tab--c-active my/tab--c-strip))
         (fg       (if active my/tab--c-fg my/tab--c-dim))
         (family   (my/tab--font-family))
         (px       (my/tab--font-px))
         (icon     (my/tab--icon-parts (my/tab--icon tab project)))
         (icon-px  (* px 1.1))
         (icon-w   (if icon (round (* icon-px 1.15)) 0))
         (label-x  (+ my/tab-hpad (if icon (+ icon-w my/tab-gap) 0)))
         (body-w   (+ label-x (my/tab--text-width label) 6))
         (running  (and project (fboundp 'my/run-running-count)
                        (> (my/run-running-count (plist-get project :root)) 0)))
         (icon-color (cond ((null icon) nil)
                           (project (if active my/tab--c-accent my/tab--c-dim))
                           (t (or (nth 2 icon) fg))))
         (body (my/tab--svg
                body-w bg active
                (concat
                 (when icon
                   (format "<text x='%.1f' y='%.1f' text-anchor='middle' font-family='%s' font-size='%.1fpx' fill='%s'>%s</text>"
                           (+ my/tab-hpad (/ icon-w 2.0)) (my/tab--baseline icon-px)
                           (xml-escape-string (nth 1 icon)) icon-px icon-color
                           (xml-escape-string (nth 0 icon))))
                 ;; green dot on the icon while a project process runs
                 (when (and icon running)
                   (format "<circle cx='%.1f' cy='%.1f' r='3.2' fill='%s' stroke='%s' stroke-width='1.5'/>"
                           (+ my/tab-hpad icon-w -1.5) (+ (/ my/tab-height 2.0) 4.5)
                           my/tab--c-running bg))
                 (my/tab--svg-text label-x label family px fg
                                   (and active project "600")))))
         ;; close slot: × on the active tab, ● when there are unsaved files
         (slot 16)
         (tail-w (+ slot (- my/tab-hpad 4)))
         (cx (/ slot 2.0)) (cy (/ (- my/tab-height 1) 2.0))
         (mark (cond
                (modified
                 (format "<circle cx='%.1f' cy='%.1f' r='3.5' fill='%s'/>" cx cy fg))
                (active
                 (format (concat "<g stroke='%s' stroke-width='1.4' stroke-linecap='round'>"
                                 "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f'/>"
                                 "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f'/></g>")
                         my/tab--c-dim
                         (- cx 3.5) (- cy 3.5) (+ cx 3.5) (+ cy 3.5)
                         (- cx 3.5) (+ cy 3.5) (+ cx 3.5) (- cy 3.5)))
                (t "")))
         (tail (my/tab--svg tail-w bg active mark)))
    (concat (propertize " " 'display (my/tab--image body))
            (propertize " " 'display (my/tab--image tail)
                        'close-tab (or active modified)
                        'help-echo (if modified "Unsaved changes – click to close tab"
                                     "Close tab")))))

(defun my/tab-bar-tab-name-format (tab _i)
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (condition-case err
          (my/tab-render tab)
        (error
         (message "tabs: render failed: %S" err)
         (format " %s " (alist-get 'name (cdr tab)))))
    ;; terminal fallback: plain text tabs
    (let* ((project (my/tab--project tab))
           (label (my/tab--label tab project)))
      (propertize (format " %s%s " label (if (my/tab--modified-p tab project) " ●" ""))
                  'face (funcall tab-bar-tab-face-function tab)))))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)

;; redraw tabs when a buffer's modified state changes, so ● updates
(defvar-local my/tab--last-modified nil)
(defun my/tab--maybe-refresh ()
  (let ((m (buffer-modified-p)))
    (unless (eq m my/tab--last-modified)
      (setq my/tab--last-modified m)
      (force-mode-line-update t))))
(add-hook 'post-command-hook #'my/tab--maybe-refresh)
(add-hook 'after-save-hook (lambda () (force-mode-line-update t)))

;;; theme integration
;; ---------------------------------------------------------------------

(defun my/tab-apply-theme (&rest _)
  "Recompute the tab palette from the current theme."
  (clrhash my/tab--image-cache)
  (let* ((bg    (my/tab--face-color 'default :background "#1e1f22"))
         (fg    (my/tab--face-color 'default :foreground "#dfe1e5"))
         (dark  (< (my/tab--luma bg) 0.5))
         (strip (my/tab--mix (if dark "#ffffff" "#000000") bg (if dark 0.045 0.05)))
         (accent (or my/tab-accent
                     (my/tab--face-color 'cursor :background nil)
                     (my/tab--face-color 'link :foreground "#3574f0"))))
    (setq my/tab--c-active bg
          my/tab--c-strip  strip
          my/tab--c-fg     fg
          my/tab--c-dim    (my/tab--mix fg bg 0.62)
          my/tab--c-accent (my/tab--hex accent)
          my/tab--c-running (my/tab--face-color 'success :foreground "#5fb85f")
          my/tab--c-border (my/tab--mix (if dark "#000000" "#000000") strip 0.25))
    (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive))
      (set-face-attribute face nil
                          :background strip :foreground my/tab--c-dim
                          :box nil :underline nil :overline nil
                          :weight 'normal :inherit nil))
    (set-face-attribute 'tab-bar-tab nil :background bg :foreground fg))
  (force-mode-line-update t))

(my/tab-apply-theme)
(advice-add 'load-theme :after #'my/tab-apply-theme)
(advice-add 'enable-theme :after #'my/tab-apply-theme)
(advice-add 'disable-theme :after #'my/tab-apply-theme)

;; Emacs daemon: the first GUI frame is when real colors/fonts are known
(defun my/tab-on-new-frame (frame)
  (with-selected-frame frame (my/tab-apply-theme)))
(add-hook 'after-make-frame-functions #'my/tab-on-new-frame)

(defun my/tab-debug ()
  "Show what the tab renderer sees."
  (interactive)
  (message "tabs: graphic=%s svg=%s font=%s %.1fpx | active=%s strip=%s accent=%s"
           (display-graphic-p) (image-type-available-p 'svg)
           (my/tab--font-family) (my/tab--font-px)
           my/tab--c-active my/tab--c-strip my/tab--c-accent))

;;; pinned tabs
;; ---------------------------------------------------------------------

;; A pinned tab sits at the far right of the bar, out of the way (music,
;; mail, services...).  The flag lives in the tab itself as `my/pinned'
;; (the time it was pinned); tab-bar copies unknown parameters along, so
;; it survives switching.  Pinned tabs are kept at the end of the tab
;; list, so s-1.. still counts left to right across everything, while
;; s-M-1.. counts pinned tabs from the right edge: the first one you
;; pinned is always s-M-1.

(defun my/tab-pinned-p (tab)
  (alist-get 'my/pinned (cdr tab)))

(defun my/tab--pinned-tabs ()
  "Pinned tabs, right edge first."
  (reverse (seq-filter #'my/tab-pinned-p (funcall tab-bar-tabs-function))))

(defun my/tab-keep-pinned-right (&rest _)
  "Reorder this frame's tabs: unpinned in order, then pinned.
Pinned tabs are ordered newest-pinned leftmost, so older pins keep
their place against the right edge."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (pinned (sort (seq-filter #'my/tab-pinned-p tabs)
                       (lambda (a b) (> (my/tab-pinned-p a) (my/tab-pinned-p b)))))
         (new (append (seq-remove #'my/tab-pinned-p tabs) pinned)))
    (unless (equal (mapcar #'cdr new) (mapcar #'cdr tabs))
      (tab-bar-tabs-set new)
      (force-mode-line-update))))

(advice-add 'tab-bar-new-tab-to :after #'my/tab-keep-pinned-right)
(advice-add 'tab-bar-move-tab-to :after #'my/tab-keep-pinned-right)

(defun my/tab-pin-toggle (&optional tab-number)
  "Pin the current tab (or tab TAB-NUMBER) to the right of the bar, or unpin it."
  (interactive)
  (let ((tab (if tab-number
                 (nth (1- tab-number) (funcall tab-bar-tabs-function))
               (tab-bar--current-tab-find))))
    (if (my/tab-pinned-p tab)
        (setf (cdr tab) (assq-delete-all 'my/pinned (cdr tab)))
      (setf (alist-get 'my/pinned (cdr tab)) (float-time)))
    (my/tab-keep-pinned-right)
    (force-mode-line-update)
    (message "%s %s" (if (my/tab-pinned-p tab) "Pinned" "Unpinned")
             (alist-get 'name (cdr tab)))))

(defun my/tab-select-pinned (n)
  "Switch to the Nth pinned tab, counting from the right edge."
  (let ((tab (nth (1- n) (my/tab--pinned-tabs))))
    (if tab
        (tab-bar-select-tab (1+ (seq-position (funcall tab-bar-tabs-function) tab #'eq)))
      (message "No pinned tab %d" n))))

(defun my/tab--format-some (pinned)
  "Format the tabs whose pinned state is PINNED, keeping real tab numbers."
  (let ((i 0))
    (mapcan (lambda (tab)
              (setq i (1+ i))
              (when (eq (and (my/tab-pinned-p tab) t) pinned)
                (tab-bar--format-tab tab i)))
            (funcall tab-bar-tabs-function))))

(defun my/tab-bar-format-unpinned () (my/tab--format-some nil))
(defun my/tab-bar-format-pinned () (my/tab--format-some t))

;;; right-click menu
;; ---------------------------------------------------------------------

;; Right click a tab for what you can do with it (pin, project, close...),
;; or the empty part of the bar to open things.  Actions on another tab
;; switch to it first, so they behave exactly like their keys there.

(defun my/menu (title items)
  "A popup menu keymap called TITLE from ITEMS, in order.
Each item is \"--\" (a separator), (LABEL :sub ITEMS) for a submenu,
\(LABEL COMMAND . PROPS) where PROPS go to `menu-item' (:enable FORM,
:button ...), or nil, which is skipped."
  (let ((map (make-sparse-keymap title))
        (n 0))
    (dolist (item items)
      (let ((key (vector (intern (format "item-%d" (cl-incf n))))))
        (pcase item
          ('nil nil)
          ((pred stringp) (define-key-after map key (list 'menu-item item)))
          (`(,label :sub ,sub)
           (define-key-after map key (list 'menu-item label (my/menu label sub))))
          (`(,label ,command . ,props)
           (define-key-after map key `(menu-item ,label ,command ,@props))))))
    map))

(defmacro my/tab--do (&rest body)
  `(lambda () (interactive) ,@body))

(defun my/tab--on (n command)
  "A menu action: go to tab N, then run COMMAND there."
  (my/tab--do (tab-bar-select-tab n)
              (call-interactively command)))

(defun my/tab--picker (command)
  "COMMAND, with its prompt in the vertico popup when that's set up."
  (if (fboundp 'my/project--vertico) (my/project--vertico command) command))

(defun my/tab-close-others (n)
  "Close every tab but N, keeping pinned ones."
  ;; stand on N so closing the others never moves you elsewhere
  (tab-bar-select-tab n)
  (let ((tabs (funcall tab-bar-tabs-function)))
    (dolist (i (number-sequence (length tabs) 1 -1))
      (unless (or (= i n) (my/tab-pinned-p (nth (1- i) tabs)))
        (tab-bar-close-tab i)))))

(defun my/tab-close-right (n)
  "Close the unpinned tabs right of tab N."
  ;; stand on N so closing the others never moves you elsewhere
  (tab-bar-select-tab n)
  (let ((tabs (funcall tab-bar-tabs-function)))
    (dolist (i (number-sequence (length tabs) (1+ n) -1))
      (unless (my/tab-pinned-p (nth (1- i) tabs))
        (tab-bar-close-tab i)))))

(defun my/tab--project-items (n project)
  "Project submenu items for tab N, whose project is PROJECT (or nil)."
  (let* ((root (plist-get project :root))
         (others (seq-remove (lambda (r) (equal r root))
                             (and (fboundp 'my/project-known-roots)
                                  (my/project-known-roots)))))
    `(,@(when project
          `((,(format "New tab in %s" (plist-get project :name)) ,(my/tab--on n #'my/project-new-tab))
            ("Find file…" ,(my/tab--on n #'my/project-find-file))
            ("Search in project…" ,(my/tab--on n #'my/project-grep))
            ("Terminal" ,(my/tab--on n #'my/project-terminal))
            ("Files (dired)" ,(my/tab--on n #'my/project-dired))
            "--"
            ("Run panel" ,(my/tab--do (tab-bar-select-tab n) (my/run-panel-show root)))
            ("Start servers" ,(my/tab--do (tab-bar-select-tab n) (my/run-start-servers root)))
            ("Stop everything…" ,(my/tab--do (tab-bar-select-tab n) (my/run-stop-all root)))
            ("Services" ,(my/tab--do (my/services-show-project root)))
            "--"))
      ("Open project…" ,(my/tab--picker #'my/project-open))
      ,(when others
         `("Switch to"
           :sub ,(mapcar (lambda (r)
                           (list (file-name-nondirectory (directory-file-name r))
                                 (my/tab--do (my/project-switch r))))
                         (seq-take others 12))))
      (,(if project "Attach this tab to another project…" "Attach this tab to a project…")
       ,(my/tab--on n (my/tab--picker #'my/project-attach)))
      ,(when project
         `(,(format "Close project %s…" (plist-get project :name))
           ,(my/tab--on n #'my/project-close))))))

(defun my/tab--apps-items ()
  `(("Music" my/music :enable (fboundp 'my/music))
    ("Services" my/services-open-tab :enable (fboundp 'my/services-open-tab))
    ("Mail" my/mail :enable (fboundp 'my/mail))))

(defun my/tab-context-menu (event)
  "Right-click menu for the clicked tab, or for the empty tab bar."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         ;; the selected tab comes back as `current-tab', with no number
         (n (if (eq (car-safe item) 'current-tab)
                (1+ (tab-bar--current-tab-index))
              (and item (tab-bar--key-to-number (nth 0 item)))))
         (tabs (funcall tab-bar-tabs-function))
         (count (length tabs)))
    (popup-menu
     (if (not (integerp n))
         (my/menu "Tabs"
                  `(("New tab" tab-bar-new-tab)
                    ("Open project…" ,(my/tab--picker #'my/project-open))
                    ,@(when (fboundp 'my/project-known-roots)
                        `(("Recent projects"
                           :sub ,(mapcar (lambda (r)
                                           (list (file-name-nondirectory (directory-file-name r))
                                                 (my/tab--do (my/project-switch r))))
                                         (seq-take (my/project-known-roots) 12)))))
                    "--"
                    ,@(my/tab--apps-items)
                    ,(when tab-bar-closed-tabs '("--"))
                    ,(when tab-bar-closed-tabs '("Reopen closed tab" tab-bar-undo-close-tab))))
       (let* ((tab (nth (1- n) tabs))
              (pinned (my/tab-pinned-p tab))
              (project (my/tab--project tab)))
         (my/menu
          (my/tab--label tab project)
          `((,(if pinned "Unpin" "Pin to the right") ,(my/tab--do (my/tab-pin-toggle n)))
            ,(unless project
               `("Rename…" ,(my/tab--do (tab-bar-rename-tab
                                         (read-string "Tab name: " (alist-get 'name (cdr tab)))
                                         n))))
            "--"
            ("Project" :sub ,(my/tab--project-items n project))
            "--"
            ("New tab" tab-bar-new-tab)
            ("Duplicate" ,(my/tab--do (tab-bar-duplicate-tab nil n)))
            ("Move to a new window" ,(my/tab--do (tab-bar-detach-tab n)))
            ,(unless pinned
               `("Move left" ,(my/tab--do (tab-bar-select-tab n) (tab-bar-move-tab -1))
                 :enable ,(> n 1)))
            ,(unless pinned
               `("Move right" ,(my/tab--do (tab-bar-select-tab n) (tab-bar-move-tab 1))
                 :enable ,(and (< n count) (not (my/tab-pinned-p (nth n tabs))))))
            "--"
            ("Close" ,(my/tab--do (tab-bar-close-tab n)) :enable ,(> count 1))
            ("Close other tabs (keeps pinned)" ,(my/tab--do (my/tab-close-others n))
             :enable ,(> count 1))
            ("Close tabs to the right" ,(my/tab--do (my/tab-close-right n))
             :enable ,(< n count))
            ,(when tab-bar-closed-tabs '("Reopen closed tab" tab-bar-undo-close-tab))
            "--"
            ,@(my/tab--apps-items)))))
     event)))

(keymap-set tab-bar-map "<down-mouse-3>" #'my/tab-context-menu)

;;; keybindings
;; ---------------------------------------------------------------------

(cl-loop for i from 1 to 10 do
         (let ((key (if (= i 10) "0" (number-to-string i))))
           (global-set-key
            (kbd (concat "s-" key))
            `(lambda () (interactive) (tab-bar-select-tab ,i)))))

(with-eval-after-load 'ghostel
  (cl-loop for i from 1 to 10 do
           (let ((key (if (= i 10) "0" (number-to-string i))))
             (define-key ghostel-semi-char-mode-map
               (kbd (concat "s-" key))
               `(lambda () (interactive) (tab-bar-select-tab ,i))))))

(cl-loop for i from 1 to 9 do
         (let ((n i))
           (global-set-key (kbd (format "s-M-%d" n))
                           (lambda () (interactive) (my/tab-select-pinned n)))))

(with-eval-after-load 'ghostel
  (cl-loop for i from 1 to 9 do
           (let ((n i))
             (define-key ghostel-semi-char-mode-map (kbd (format "s-M-%d" n))
               (lambda () (interactive) (my/tab-select-pinned n))))))

(global-set-key (kbd "s-M-p") #'my/tab-pin-toggle)
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>tp") #'my/tab-pin-toggle))

(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-w") 'tab-close)
(global-set-key (kbd "s-m") 'tab-window-detach)

(defun my/tab-to-split (tab-number)
  "Pull the buffer from TAB-NUMBER into a new vertical split."
  (interactive "ntab number to pull: ")
  (let* ((tabs (tab-bar-tabs))
         (tab (nth (1- tab-number) tabs))
         (state (alist-get 'ws tab))
         (buffer-info (when state
                        (cl-labels ((find-buf (tree)
                                      (cond ((null tree) nil)
                                            ((and (listp tree) (assoc 'buffer tree))
                                             (alist-get 'buffer tree))
                                            ((listp tree)
                                             (cl-some #'find-buf tree))
                                            (t nil))))
                          (find-buf state))))
         (buffer-name (if (listp buffer-info) (car buffer-info) buffer-info)))
    (if (and (stringp buffer-name) (get-buffer buffer-name))
        (progn
          (split-window-right)
          (other-window 1)
          (switch-to-buffer buffer-name))
      (message "could not find a valid buffer string in tab %d (found: %s)"
               tab-number buffer-name))))

(provide 'tabs)

;;; tabs.el ends here
