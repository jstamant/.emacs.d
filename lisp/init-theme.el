;;; init-theme.el --- Appearance settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Disable GUI elements
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)        ; Give some breathing room??

;; Fix window size on AwesomeWM
(setq frame-resize-pixelwise t)

;; Do not inherit any colors or settings from X resources
;; I'm trying to keep everything local to my Emacs install
(setq inhibit-x-resources t)

(use-package spacemacs-theme
  :ensure t)

;; TODO need to add a toggle for hl-line-mode
(global-hl-line-mode 1) ;; This currently overlays faces, so you can't use `describe-face' on them
;;(load-theme 'spacemacs-dark t)

;; Don't prompt to confirm theme safety
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(spacemacs-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(spacemacs-dark))
  (reapply-themes))

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
;; TODO add some keybinds for navigating links
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
(use-package goto-addr
  :init
  (global-goto-address-mode))

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
  "t" '(consult-theme :which-key "theme"))




(provide 'init-theme)
;;; init-theme.el ends here
