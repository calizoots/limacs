;;; pkgs/my-hover.el  -*- lexical-binding: t; -*-

;; hover docs: rest the mouse on a symbol, see its signature and docs

;; The popup is lsp-ui's doc frame, and only that: lsp-ui's sideline
;; (text down the right of every line) stays off.  It opens where the
;; mouse is, signature first, in the theme's colours.
;;
;; Keys
;; ---------------------------------------------------------------------
;;   mouse rests on a symbol   the popup
;;   K  /  SPC c h             the same popup for the symbol at the cursor
;;   K again                   into the popup to scroll it (q leaves)
;;   the mouse can move onto the popup and scroll it with the wheel
;;   (Settings: "Hover docs" on/off, and how long the mouse has to rest)

(require 'cl-lib)

(declare-function lsp-ui-doc-glance "lsp-ui-doc")
(declare-function lsp-ui-doc-mode "lsp-ui-doc")
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function evil-define-key "evil-core")
(declare-function my/tab--mix "tabs")
(declare-function my/tab--face-color "tabs")

(defvar lsp-ui-doc-border)
(defvar lsp-ui-doc-show-with-mouse)
(defvar lsp-ui-doc-delay)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable nil          ; no text down the right of lines
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor nil     ; the cursor resting doesn't pop it up
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-include-signature t
        lsp-ui-doc-header nil
        lsp-ui-doc-delay 0.3
        lsp-ui-doc-max-width 90
        lsp-ui-doc-max-height 20
        lsp-ui-doc-enhanced-markdown t)
  :config
  (my/hover-apply-theme))

(defun my/hover-apply-theme (&rest _)
  "Popup colours from the theme: a soft panel with a thin border."
  (when (and (fboundp 'my/tab--mix) (facep 'lsp-ui-doc-background))
    (let* ((bg (my/tab--face-color 'default :background "#1e1f22"))
           (fg (my/tab--face-color 'default :foreground "#dfe1e5")))
      (set-face-attribute 'lsp-ui-doc-background nil :background (my/tab--mix fg bg 0.06))
      (setq lsp-ui-doc-border (my/tab--mix fg bg 0.25))
      (when (facep 'lsp-ui-doc-header)
        (set-face-attribute 'lsp-ui-doc-header nil :background (my/tab--mix fg bg 0.1)
                            :foreground fg)))))

(dolist (fn '(load-theme enable-theme disable-theme))
  (advice-add fn :after #'my/hover-apply-theme))

(defvar my/hover-padding 10 "Pixels of space inside the popup's left edge.")

(defun my/hover--tidy (&rest _)
  "After lsp-ui fills the popup: pad its lines, and quieten the divider."
  (when-let ((buffer (get-buffer (lsp-ui-doc--make-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            ;; a string, so the padding takes the popup's background
            (pad (propertize " " 'display `(space :width (,my/hover-padding))
                             'face 'lsp-ui-doc-background))
            (line (and (fboundp 'my/tab--mix)
                       (my/tab--mix (my/tab--face-color 'default :foreground "#dfe1e5")
                                    (my/tab--face-color 'default :background "#1e1f22")
                                    0.18))))
        ;; only lines with text: the divider and lsp-ui's thin spacer lines
        ;; would grow to a full line's height with a prefix
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((bol (line-beginning-position)) (eol (line-end-position)))
              (when (and (< bol eol)
                         (not (text-property-any bol eol 'lsp-ui-doc--replace-hr t))
                         (string-match-p "[^ \t]" (buffer-substring-no-properties bol eol)))
                (put-text-property bol (min (point-max) (1+ eol)) 'line-prefix pad)
                (put-text-property bol (min (point-max) (1+ eol)) 'wrap-prefix pad)))
            (forward-line 1)))
        ;; lsp-ui draws the divider in a fixed bright grey
        (when line
          (let ((pos (point-min)))
            (while (setq pos (next-single-property-change pos 'lsp-ui-doc--replace-hr))
              (when (get-text-property pos 'lsp-ui-doc--replace-hr)
                (put-text-property pos (1+ pos) 'face `(:background ,line))))))))))

(declare-function lsp-ui-doc--make-buffer-name "lsp-ui-doc")
(with-eval-after-load 'lsp-ui-doc
  (advice-add 'lsp-ui-doc--render-buffer :after #'my/hover--tidy))

;;; reaching the popup with the mouse
;; ---------------------------------------------------------------------
;; lsp-ui hides the popup the moment the mouse leaves the word, and the
;; way down to the popup crosses other text.  So leaving only schedules
;; a hide: if the mouse is on the popup by then, it stays (and scrolls
;; with the wheel) until the mouse leaves it.

(defvar my/hover-grace 0.6 "Seconds to reach the popup after leaving the word.")
(defvar my/hover--hide-timer nil)

(declare-function lsp-ui-doc--hide-frame "lsp-ui-doc")
(declare-function lsp-ui-doc--get-frame "lsp-ui-doc")
(declare-function lsp-ui-doc--visible-p "lsp-ui-doc")
(declare-function lsp-ui-doc-focus-frame "lsp-ui-doc")

(defun my/hover--mouse-on-popup-p ()
  "Non-nil while the mouse is over the docs popup."
  (let ((frame (and (fboundp 'lsp-ui-doc--get-frame) (lsp-ui-doc--get-frame))))
    (when (and frame (frame-live-p frame) (frame-visible-p frame))
      (let* ((mouse (mouse-pixel-position))
             (on (car mouse)) (x (cadr mouse)) (y (cddr mouse)))
        (or (eq on frame)
            ;; reported against the main frame: is it inside the popup's box?
            (and (eq on (frame-parent frame)) x y
                 (let* ((pos (frame-position frame))
                        (left (car pos)) (top (cdr pos)))
                   (and (<= left x (+ left (frame-pixel-width frame)))
                        (<= top y (+ top (frame-pixel-height frame)))))))))))

(defun my/hover--cancel-hide (&rest _)
  (when my/hover--hide-timer
    (cancel-timer my/hover--hide-timer)
    (setq my/hover--hide-timer nil)))

(defun my/hover--hide-later (hide)
  "Run HIDE (lsp-ui's) unless the mouse makes it onto the popup first."
  (my/hover--cancel-hide)
  (setq my/hover--hide-timer
        (run-at-time my/hover-grace nil
                     (lambda ()
                       (setq my/hover--hide-timer nil)
                       (if (my/hover--mouse-on-popup-p)
                           ;; on the popup: look again once it's left
                           (my/hover--hide-later hide)
                         (funcall hide))))))

(defun my/hover--mouse-moved (fn event)
  "lsp-ui's mouse handler (FN), with its hiding put off."
  (let ((hide (symbol-function 'lsp-ui-doc--hide-frame)))
    (cl-letf (((symbol-function 'lsp-ui-doc--hide-frame)
               (lambda (&rest _) (my/hover--hide-later hide))))
      (funcall fn event))))

(defun my/hover--keep-while-on-it (hide &rest args)
  "Every way lsp-ui closes the popup (window changes, scrolling…) waits
while the mouse is on it, and happens once it leaves."
  (if (my/hover--mouse-on-popup-p)
      (my/hover--hide-later (lambda () (apply hide args)))
    (my/hover--cancel-hide)
    (apply hide args)))

(with-eval-after-load 'lsp-ui-doc
  (advice-add 'lsp-ui-doc--hide-frame :around #'my/hover--keep-while-on-it)
  (advice-add 'lsp-ui-doc--handle-mouse-movement :around #'my/hover--mouse-moved)
  ;; new docs showing: whatever hide was pending is for the old ones
  (advice-add 'lsp-ui-doc--display :before #'my/hover--cancel-hide))

(defun my/hover-docs ()
  "The docs for the symbol at the cursor, in the same popup as hovering.
Pressed again while it shows: into the popup, to scroll it (q leaves)."
  (interactive)
  (cond ((and (bound-and-true-p lsp-mode) (fboundp 'lsp-ui-doc--visible-p) (lsp-ui-doc--visible-p))
         (lsp-ui-doc-focus-frame))
        ((and (bound-and-true-p lsp-mode) (fboundp 'lsp-ui-doc-glance)) (lsp-ui-doc-glance))
        ((bound-and-true-p lsp-mode) (lsp-describe-thing-at-point))
        (t (call-interactively #'describe-symbol))))

(defun my/hover-set-mouse (on)
  "Hovering shows docs (ON) or doesn't, in every open buffer."
  (setq-default lsp-ui-doc-show-with-mouse on)
  (dolist (b (buffer-list))
    (with-current-buffer b
      ;; the mode sets up mouse tracking as it turns on
      (when (bound-and-true-p lsp-ui-doc-mode)
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc-mode 1)))))

(with-eval-after-load 'evil
  (with-eval-after-load 'lsp-mode
    (evil-define-key 'normal 'global
      (kbd "K") #'my/hover-docs
      (kbd "<leader>ch") #'my/hover-docs)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC c h" "docs for this"))

(provide 'my-hover)

;;; my-hover.el ends here
