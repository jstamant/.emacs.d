;;; init-theme.el --- Appearance settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Disable GUI elements
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Fix window size on AwesomeWM and tiling window managers
(setq frame-resize-pixelwise t)

;; Do not inherit any colors or settings from X resources
;; I'm trying to keep everything local to my Emacs install
(setq inhibit-x-resources t)

(global-hl-line-mode 1) ;; This overlays faces, so you can't use `describe-face' on them

;; Don't prompt to confirm theme safety
(setq custom-safe-themes t)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))


;; Get a more recent version than what's included with Emacs
;; There are many customization options for Prot's themes
;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-mixed-fonts t)
  ;; (setq modus-vivendi-palette-overrides
  ;;       '((bg-main "#222222")))
  (setq modus-themes-headings
        '((0 . (variable-pitch 1.8))
          (1 . (1.5))
          (2 . (1.3))
          (3 . (1.2))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (1.1))))
  (load-theme 'modus-vivendi))


;; Prettify non-printing characters
;; Line feed character ^L can be inserted with
;; C-q `quoted-insert' C-l
;; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :init (global-page-break-lines-mode))


;; Highlighting of certain keywords in comments, see `hl-todo-keyword-faces'
(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (add-to-list 'hl-todo-keyword-faces '("TODO" warning bold))
  (add-to-list 'hl-todo-keyword-faces '("DEPRECATED" font-lock-doc-face bold))
  (add-to-list 'hl-todo-keyword-faces '("PERF" font-lock-doc-face bold))
  (add-to-list 'hl-todo-keyword-faces '("NOTE" warning bold)))


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
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))


;; On by default according to `show-paren-predicate'
(use-package paren)


(keymap-set toggles-map "p" '("paren" . show-paren-local-mode))
(keymap-set toggles-map "C-p" '("global paren" . show-paren-mode))
(keymap-set toggles-map "t" '("theme" . consult-theme))
(keymap-set toggles-map "T" 'toggle-theme)


(provide 'init-theme)
;;; init-theme.el ends here
