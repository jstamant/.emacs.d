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


  :init


(provide 'init-theme)
;;; init-theme.el ends here
