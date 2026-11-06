;; ui/tabs.el --- IDE-style tabs with rounded active tab  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)

(tab-bar-mode 1)

;; ---------------------------------------------------------------------
;; Settings
;; ---------------------------------------------------------------------

(defvar my/tab-rounded t
  "Draw rounded top corners on the active tab with two tiny images.
The images use P3 PPM data via Emacs's pbm image type.  Set to nil for flat tabs (no images at all).")

(defvar my/tab-images t
  "Master switch.  nil = no bitmaps at all: plain space padding and flat tabs.
Use it to rule the images out if Emacs ever misbehaves.")

(defvar my/tab-height 15
  "Height of every tab, in pixels.
Raise it if the bottom corners look notched (the icon font makes rows taller
than this), lower it for slimmer tabs.")
(defvar my/tab-radius 8  "Corner radius in pixels.")
(defvar my/tab-hpad  14  "Horizontal padding inside each tab, in pixels.")
(defvar my/tab-icon-height 0.9
  "Size of tab icons relative to the text.")
(defvar my/tab-icon-v-adjust nil
  "Vertical icon offset (negative = lower).  nil keeps all-the-icons'
own per-icon baseline correction, which is usually right.")

;; --- Core behavior ---
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-select-tab-modifiers '(super)
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-show 1
      tab-bar-tab-hints nil
      tab-bar-close-button-show 'selected
      tab-bar-new-button-show nil
      tab-bar-tab-name-truncated-max 24)

;; --- Close glyph ---
(defun my/tab-bar-set-close-button ()
  (setq tab-bar-close-button
        (propertize " ×" 'close-tab t)))

;; --- Icons ---
(use-package all-the-icons :ensure t)

(defun my/tab-bar-tab-icon (tab)
  (condition-case nil
      (let ((buf (get-buffer (alist-get 'name tab)))
            (args (append (list :height my/tab-icon-height)
                          (when my/tab-icon-v-adjust
                            (list :v-adjust my/tab-icon-v-adjust)))))
        (if buf
            (with-current-buffer buf
              (concat (if buffer-file-name
                          (apply #'all-the-icons-icon-for-file buffer-file-name args)
                        (apply #'all-the-icons-icon-for-mode major-mode args))
                      " "))
          ""))
    (error "")))

;; ---------------------------------------------------------------------
;; Color helpers (own math: independent of color.el's lighten/darken)
;; ---------------------------------------------------------------------

(defun my/tab--rgb (color)
  "COLOR as a list (R G B) of floats from 0 to 1, or nil if unusable."
  (when (stringp color)
    (cond
     ((string-match "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'" color)
      (mapcar (lambda (i) (/ (string-to-number (match-string i color) 16) 255.0))
              '(1 2 3)))
     ((string-match "\\`#\\([[:xdigit:]]\\{4\\}\\)\\([[:xdigit:]]\\{4\\}\\)\\([[:xdigit:]]\\{4\\}\\)\\'" color)
      (mapcar (lambda (i) (/ (string-to-number (match-string i color) 16) 65535.0))
              '(1 2 3)))
     (t (ignore-errors (color-name-to-rgb color))))))

(defun my/tab--hex (rgb-or-color)
  "Format an (R G B) list, or a color string, as #rrggbb."
  (let ((rgb (if (stringp rgb-or-color) (my/tab--rgb rgb-or-color) rgb-or-color)))
    (if rgb
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (v) (round (* 255 (max 0.0 (min 1.0 v))))) rgb))
      "#000000")))

(defun my/tab--mix (a b amount)
  "Mix color A and color B; AMOUNT (0 to 1) is the share of A.  Returns #rrggbb."
  (let ((ra (my/tab--rgb a)) (rb (my/tab--rgb b)))
    (if (and ra rb)
        (my/tab--hex (cl-mapcar (lambda (x y) (+ (* amount x) (* (- 1 amount) y)))
                                ra rb))
      (or b a))))

(defun my/tab--luma (color)
  "Perceived brightness of COLOR, 0 (black) to 1 (white)."
  (let ((c (my/tab--rgb color)))
    (if c (+ (* 0.2126 (nth 0 c)) (* 0.7152 (nth 1 c)) (* 0.0722 (nth 2 c))) 0.0)))

(defun my/tab--color (face attr fallback)
  "Color of ATTR on FACE, or FALLBACK if it is unset (e.g. in a terminal)."
  (let ((c (face-attribute face attr nil t)))
    (if (my/tab--rgb c) c fallback)))

;; ---------------------------------------------------------------------
;; Rounded corners: small bitmap "caps" on each side of the ACTIVE tab
;; ---------------------------------------------------------------------

(defvar my/tab--active-hex "#000000")  ; tab color (set with the faces)
(defvar my/tab--strip-hex  "#000000")  ; color of the bar behind the tabs
(defvar my/tab--cap-cache (make-hash-table :test #'equal))

(defun my/tab--caps-p ()
  "Non-nil when rounded caps can be shown in the selected frame."
  (and my/tab-rounded (my/tab--images-p)))

(defun my/tab--height ()
  "Pixel height of every tab."
  my/tab-height)

(defun my/tab--images-p ()
  "Non-nil when small bitmaps can be shown in the selected frame."
  (and my/tab-images
       (display-graphic-p)
       (image-type-available-p 'ppm)))

(defun my/tab--solid-ppm (w h color)
  "PPM (P3) source for a W by H block filled with COLOR."
  (let* ((c (my/tab--rgb color))
         (px (mapconcat (lambda (v) (number-to-string (round (* 255 v)))) c " ")))
    (format "P3\n%d %d\n255\n%s\n" w h
            (mapconcat #'identity (make-list (* w h) px) " "))))

(defun my/tab--block (width color face)
  "A WIDTH-pixel block of COLOR that is exactly as tall as a tab.
Every tab has one on each side; that is what makes all tabs the same height.
Falls back to plain spaces in FACE where bitmaps are not available."
  (or (and (my/tab--images-p)
           (ignore-errors
             (let* ((w (max 1 width))
                    (h (my/tab--height))
                    (key (list 'solid w h color))
                    (img (or (gethash key my/tab--cap-cache)
                             (puthash key
                                      (create-image (my/tab--solid-ppm w h color)
                                                    'pbm t :ascent 'center)
                                      my/tab--cap-cache))))
               (propertize " " 'face face 'display img))))
      (propertize (make-string (max 1 (round (/ width 7.0))) ?\s) 'face face)))

(defun my/tab--cap-ppm (side fill strip r h)
  "PPM (P3) source for one cap, R pixels wide and H tall.
The tab shape is FILL, the bar behind it STRIP, anti-aliased by 4x4
supersampling.  SIDE is `left' or `right'."
  (let ((f (my/tab--rgb fill))
        (s (my/tab--rgb strip))
        (n 4)
        rows)
    (dotimes (y h)
      (dotimes (x r)
        (let ((hit 0))
          (dotimes (j n)
            (dotimes (i n)
              (let* ((sx (+ x (/ (+ i 0.5) n)))
                     (sy (+ y (/ (+ j 0.5) n)))
                     (dx (- (if (eq side 'left) sx (- r sx)) r))
                     (dy (- sy r)))
                (when (or (>= sy r) (<= (+ (* dx dx) (* dy dy)) (* r r)))
                  (setq hit (1+ hit))))))
          (let ((cov (/ hit (float (* n n)))))
            (push (mapconcat
                   (lambda (k)
                     (number-to-string
                      (round (* 255 (+ (* cov (nth k f))
                                       (* (- 1 cov) (nth k s)))))))
                   '(0 1 2) " ")
                  rows)))))
    (format "P3\n%d %d\n255\n%s\n" r h (mapconcat #'identity (nreverse rows) " "))))

(defun my/tab--cap (side)
  "Propertized string showing the SIDE cap image, or nil if it can't be made."
  (ignore-errors
    (let* ((r my/tab-radius)
           (h (my/tab--height))
           (key (list side my/tab--active-hex my/tab--strip-hex r h))
           (img (or (gethash key my/tab--cap-cache)
                    (puthash key
                             (create-image
                              (my/tab--cap-ppm side my/tab--active-hex
                                               my/tab--strip-hex r h)
                              'pbm t :ascent 'center)
                             my/tab--cap-cache))))
      (propertize " " 'face 'tab-bar-tab 'display img))))

;; ---------------------------------------------------------------------
;; Tab string
;; ---------------------------------------------------------------------

(defun my/tab-bar-tab-name-format (tab i)
  (ignore i)
  (condition-case nil
      (let* ((current-p (eq (car tab) 'current-tab))
             (face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive))
             (name (alist-get 'name tab))
             (buf (get-buffer name))
             (modified (and buf (buffer-file-name buf) (buffer-modified-p buf)))
             (close (cond (modified (propertize " ●" 'close-tab t))
                          (current-p tab-bar-close-button)
                          (t "")))
             (body (copy-sequence (concat (my/tab-bar-tab-icon tab) name close)))
             (left  (and current-p (my/tab--caps-p) (my/tab--cap 'left)))
             (right (and left (my/tab--cap 'right)))
             ;; with caps, the caps supply part of the side padding
             (side  (if (and left right)
                        (max 0 (- my/tab-hpad my/tab-radius))
                      my/tab-hpad)))
        ;; append: our face only fills in what the icon/close glyph leave unset
        (add-face-text-property 0 (length body) face t body)
        (let ((color (if current-p my/tab--active-hex my/tab--strip-hex)))
          (concat left
                  (my/tab--block side color face)
                  body
                  (my/tab--block side color face)
                  right)))
    (error (format " %s " (alist-get 'name tab)))))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)

;; ---------------------------------------------------------------------
;; Faces, rebuilt after each theme load and for each new frame
;; ---------------------------------------------------------------------

(defun my/tab-bar-apply-theme-faces (&rest _)
  (clrhash my/tab--cap-cache)
  (let* ((bg       (my/tab--color 'default :background "#1e1e1e"))
         (fg       (my/tab--color 'default :foreground "#d4d4d4"))
         (luma     (my/tab--luma bg))
         ;; the strip contrasts with the editor: lighter on near-black
         ;; themes, darker on everything else
         (strip-bg (cond ((< luma 0.10) (my/tab--mix "#ffffff" bg 0.07))
                         ((< luma 0.50) (my/tab--mix "#000000" bg 0.25))
                         (t             (my/tab--mix "#000000" bg 0.07))))
         (dim-fg   (my/tab--mix fg bg 0.60)))

    (setq my/tab--active-hex (my/tab--hex bg)
          my/tab--strip-hex  (my/tab--hex strip-bg))

    (set-face-attribute 'tab-bar nil
                        :background strip-bg
                        :foreground dim-fg
                        :box nil)

    (set-face-attribute 'tab-bar-tab nil
                        :background bg
                        :foreground fg
                        :weight 'normal
                        :box nil)

    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background strip-bg
                        :foreground dim-fg
                        :weight 'normal
                        :box nil))
  (my/tab-bar-set-close-button)
  (force-mode-line-update t))

(my/tab-bar-apply-theme-faces)
(advice-add 'load-theme :after #'my/tab-bar-apply-theme-faces)
(advice-add 'enable-theme :after #'my/tab-bar-apply-theme-faces)

;; Emacs daemon: when this file runs there is no GUI frame yet, so colors and
;; image support are unknown.  Recompute whenever a new frame appears.
(defun my/tab-bar-on-new-frame (frame)
  (with-selected-frame frame
    (my/tab-bar-apply-theme-faces)))
(add-hook 'after-make-frame-functions #'my/tab-bar-on-new-frame)

;; --- Diagnostics: M-x my/tab-debug ---
(defun my/tab-debug ()
  "Show why the tabs look the way they do."
  (interactive)
  (message (concat "tabs: emacs %s | %s | graphic=%s | pbm=%s svg=%s | rounded=%s caps-p=%s"
                   " | active=%s strip=%s | tab-height=%s | daemon=%s")
           emacs-version system-type (display-graphic-p)
           (image-type-available-p 'pbm) (image-type-available-p 'svg)
           my/tab-rounded (my/tab--caps-p)
           my/tab--active-hex my/tab--strip-hex (my/tab--height)
           (and (daemonp) t)))

;; --- Keybindings (unchanged) ---
(cl-loop for i from 1 to 10 do
         (let ((key (if (= i 10) "0" (number-to-string i))))
           (global-set-key
            (kbd (concat "s-" key))
            `(lambda () (interactive) (tab-bar-select-tab ,i)))))

(with-eval-after-load 'vterm
  (cl-loop for i from 1 to 10 do
           (let ((key (if (= i 10) "0" (number-to-string i))))
             (define-key vterm-mode-map
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
