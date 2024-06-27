;;; init-theme.el --- Appearance settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Disable GUI elements
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)        ; Give some breathing room??

;; Fix window size on AwesomeWM and tiling window managers
(setq frame-resize-pixelwise t)

;; Do not inherit any colors or settings from X resources
;; I'm trying to keep everything local to my Emacs install
(setq inhibit-x-resources t)

(global-hl-line-mode 1) ;; This overlays faces, so you can't use `describe-face' on them

;; Don't prompt to confirm theme safety
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(modus-vivendi))


;; Prettify non-printing characters
;; Line feed character ^L can be inserted with
;; C-q `quoted-insert' C-l
;; https://github.com/purcell/page-break-lines
;; TODO add a toggle for page-break-lines-mode
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init (global-page-break-lines-mode))


;; Highlighting of certain TODO-like keywords
;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))


;; Making links clickable and highlighted
;; Default binding is "C-c RET" for `goto-address-at-point'
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
(use-package goto-addr
  :init
  (global-goto-address-mode))

(with-eval-after-load 'goto-addr
  (keymap-set goto-address-highlight-keymap "C-c C-o" 'goto-address-at-point))


;; Help see matching delimiters...especially useful for lisp!
;; Also identifies unmatched delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; On by default according to `show-paren-predicate'
(use-package paren)


(jrs/add-toggle-keys
  "p" '(show-paren-local-mode :which-key "paren")
  "C-p" '(show-paren-mode :which-key "global paren")
  "t" '(consult-theme :which-key "theme")
  "T" 'toggle-theme)


(provide 'init-theme)
;;; init-theme.el ends here
