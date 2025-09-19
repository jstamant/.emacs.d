;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking)
(require 'init-performance)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-package-manager)

(require 'init-keybindings)
;;(require 'init-evil)
(unbind-key "C-z" 'global-map) ; used to be 'suspend-frame, which would minimze emacs

(require 'init-environment)

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
(setq view-read-only t) ;; Visit read-only buffers in view-mode by default

;;;; MAN MODE
(keymap-set help-map "M" 'man)

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
