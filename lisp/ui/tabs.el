;; ui/tabs.el --- JetBrains-style project tabs  -*- lexical-binding: t; -*-

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

;; ---------------------------------------------------------------------
;; Settings
;; ---------------------------------------------------------------------

(defvar my/tab-height 34        "Tab height in pixels.")
(defvar my/tab-hpad 14          "Padding on each side of a tab, in pixels.")
(defvar my/tab-gap 7            "Space between icon and label, in pixels.")
(defvar my/tab-accent-height 3  "Thickness of the active tab's accent bar.")
(defvar my/tab-font-scale 0.95  "Tab label size relative to the default font.")
(defvar my/tab-max-chars 28     "Longer labels are truncated with an ellipsis.")
(defvar my/tab-accent nil
  "Accent color for the active tab, or nil to take it from the theme.")

(setq tab-bar-format '(tab-bar-format-tabs)
      tab-bar-separator ""
      tab-bar-auto-width nil
      tab-bar-select-tab-modifiers '(super)
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-show t
      tab-bar-tab-hints nil
      tab-bar-close-button-show nil   ; we draw our own
      tab-bar-new-button-show nil)

(tab-bar-mode 1)

;; ---------------------------------------------------------------------
;; Color helpers
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

;; Palette, filled in by `my/tab-apply-theme'.
(defvar my/tab--c-active "#1e1f22")   ; active tab = editor background
(defvar my/tab--c-strip  "#2b2d30")   ; bar behind the tabs
(defvar my/tab--c-fg     "#dfe1e5")   ; active label
(defvar my/tab--c-dim    "#9da0a8")   ; inactive label
(defvar my/tab--c-accent "#3574f0")   ; active tab underline
(defvar my/tab--c-border "#1e1f22")   ; hairline under the strip

;; ---------------------------------------------------------------------
;; Font metrics
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

;; ---------------------------------------------------------------------
;; SVG pieces
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

;; ---------------------------------------------------------------------
;; What a tab shows
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
    (if project
        (all-the-icons-octicon "repo")
      (let ((buf (get-buffer (alist-get 'name (cdr tab)))))
        (when buf
          (with-current-buffer buf
            (if buffer-file-name
                (all-the-icons-icon-for-file buffer-file-name)
              (all-the-icons-icon-for-mode major-mode))))))))

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

;; ---------------------------------------------------------------------
;; Tab rendering
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

;; Redraw tabs when a buffer's modified state changes, so ● updates.
(defvar-local my/tab--last-modified nil)
(defun my/tab--maybe-refresh ()
  (let ((m (buffer-modified-p)))
    (unless (eq m my/tab--last-modified)
      (setq my/tab--last-modified m)
      (force-mode-line-update t))))
(add-hook 'post-command-hook #'my/tab--maybe-refresh)
(add-hook 'after-save-hook (lambda () (force-mode-line-update t)))

;; ---------------------------------------------------------------------
;; Theme integration
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

;; Emacs daemon: the first GUI frame is when real colors/fonts are known.
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

;; ---------------------------------------------------------------------
;; Keybindings
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
