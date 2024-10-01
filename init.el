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
(require 'init-package-manager)

(require 'init-evil)
(require 'init-keybindings)
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

;;; Editor
(require 'init-capf)
(require 'init-completions)
(require 'init-editor)
(require 'init-mc)
(require 'init-navigation)

;;; Window management
(require 'init-files)
(require 'init-buffers)
(require 'init-windows)

;;; UI
(require 'init-theme)
(require 'init-ui)
(require 'init-modeline)

;;; Checkers
(require 'init-syntax)
(require 'init-spelling)

;;; Emacs built-in packages
(keymap-set ctl-x-map "c" 'calc)
(general-spc "xc" 'calc)
(require 'init-abbrev)
(require 'init-dired)
(require 'init-help)
;;;; VIEW MODE
(use-package view
  :bind ("C-x v" . view-mode))

;;; Tools
;; Package to show key-presses in a *command-log* buffer
;; https://github.com/lewang/command-log-mode
(use-package command-log-mode :ensure t)
(require 'init-formatter)
(require 'init-git)
(require 'init-keyfreq)
(require 'init-lsp)
(require 'init-projectile)
(require 'init-tree-sitter)
;;;; TERM SETTINGS
(use-package term
  :ensure t)

;;; Languages
(require 'init-c)
(require 'init-cfml)
(require 'init-javascript)
(require 'init-json)
(require 'init-ledger)
(require 'init-lisp)
(require 'init-lua)
(require 'init-markdown)
(require 'init-mermaid)
(require 'init-nix)
(require 'init-org)
(require 'init-pkgbuild)
(require 'init-python)
(require 'init-rust)
(require 'init-sh)
(require 'init-toml)
(require 'init-vimrc)
(require 'init-vue)
(require 'init-web)
(require 'init-yaml)


;; Finally, load variables configured from the 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
