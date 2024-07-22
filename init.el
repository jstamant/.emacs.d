;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking)


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-package)


(require 'init-modal)
(require 'init-keybindings)
;; (require 'init-meow)
;; (require 'init-evil)
;; (use-package expand-region :ensure t
;;   :bind '("C-\\" er/expand-region))
(unbind-key "C-z" 'global-map) ; used to be 'suspend-frame, which would minimze emacs

(setq user-full-name "Justin St-Amant")

(defvar drive-directory
  (cond ((equal system-type 'gnu/linux)
         "~/drive/")
        ((equal system-type 'darwin)
         (cond ((file-directory-p "~/Google Drive/My Drive/")
                ;; This is when only one account is added
                "~/Google Drive/My Drive/")
               ((file-directory-p "~/jstamant24@gmail.com - Google Drive/My Drive/")
                ;; This is when more than one accounts are added
                "~/jstamant24@gmail.com - Google Drive/My Drive/")))
        ((equal system-type 'windows-nt)
         (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
           (concat userprofile "/Google Drive/"))))
  "The absolute path to the Google Drive directory under any operating system.")

(defvar onedrive-directory
  (when (eq system-type 'windows-nt)
      (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
        (concat userprofile "/OneDrive - Manitoba Hydro/")))
  "The absolute path to your work OneDrive directory. Only for Windows.")

(when (eq system-type 'windows-nt)
    (setenv "HOME" (getenv "USERPROFILE")))

(defun at-work ()
  "t if using a work computer."
  (equal "MH" (substring (system-name) 0 2)))
(defun at-home ()
  "t if using a non-work computer."
  (not (at-work)))

(defun init ()
  "Shortcut for finding your Emacs configuration file.
Finds `user-init-file'"
  (interactive)
  (find-file user-init-file))
(defun reload ()
  "Shortcut for reloading your Emacs configuration.
This is great for when you're tinkering on your `user-init-file'"
  (interactive)
  (load user-init-file))

;; Enable some pre-disabled commands
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'scroll-left 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Remove the default GNU startup message. You can change it if you want.
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t) ; Disable the default Emacs startup screen
(setq use-dialog-box nil) ; Disables dialog boxes for mouse-driven actions

;; Sound settings
(setq ring-bell-function 'ignore) ; Turn off audible bell
(setq visible-bell t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Miscellaneous settings
(setq scroll-preserve-screen-position t) ; Keep point position on screen when scrolling

(use-package command-log-mode :ensure t)

(require 'init-theme)
(require 'init-modeline)
(require 'init-dashboard)

(require 'init-editor)
(require 'init-navigation)

(setq delete-by-moving-to-trash t) ; Use trash instead of destroying files

(require 'init-abbrev)
(require 'init-flymake)

;;;; CALC SETTINGS
(use-package calc
  :bind ("C-x c" . calc))

(require 'init-dired)

;;;; EMOJIFY SETTINGS
;; Not sure if I like this mode.
;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :init
;;   (setq emojify-display-style 'image) ; Or can be set to 'unicode
;;   (setq emojify-emoji-styles '(unicode github))) ; Don't display ascii emojis


(require 'init-completions)
(require 'init-capf)
(require 'init-files)
(require 'init-buffers)
(require 'init-windows)
(require 'init-mc)

;;;; FLYSPELL SETTINGS
;; (use-package flyspell
;;   :hook (text-mode . flyspell-mode)) ; Start spell checking on all modes derived from text-mode

(require 'init-spelling)
(require 'init-ledger)

(require 'init-git)

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t
  :config
  (setq pkgbuild-update-sums-on-save nil))

;;;; SHELL-SCRIPT SETTINGS
(use-package sh-script
  :ensure t
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))

;;;; TERM SETTINGS
(use-package term
  :ensure t)

;;;; VIEW MODE
(use-package view
  :bind ("C-x v" . view-mode))

(require 'init-web)

(require 'init-help)
(require 'init-lisp)
(require 'init-projectile)

(require 'init-lsp)
(require 'init-formatter)

(require 'init-tree-sitter)

;;; Languages

(require 'init-c)
(require 'init-cfml)
(require 'init-javascript)
(use-package json-mode :ensure t :mode "\\.json\\'" :init (setq js-indent-level 2))
(use-package lua-mode :ensure t)
(use-package markdown-mode :ensure t)
(require 'init-mermaid)
(require 'init-nix)
(require 'init-org)
(require 'init-python)
(require 'init-rust)
(use-package vimrc-mode :ensure t :mode "\\.vim\\(rc\\)?\\'")
(require 'init-vue)
(use-package yaml-mode :ensure t :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

;;; Packages that have little-to-no configuration

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Finally, load variables configured from the 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
