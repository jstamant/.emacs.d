;;; init.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-performance)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))
(require 'init-package-manager)

;;; Core setup
(require 'core-keybindings)
;; TODO move to core-*
(require 'init-theme)
(require 'init-ui)
(require 'init-modeline)
;; NOTE hmm...variables in here are user-specific, but required by org
(require 'init-environment)

;;; Editor
(require 'init-capf)
(require 'init-completions)
(require 'init-editor)
(require 'init-navigation)

;;; Window management
(require 'init-files)
(require 'init-buffers)
(require 'init-windows)

;;; Checkers
(require 'init-syntax)
(require 'init-spelling)

;;; Emacs built-in packages
(require 'init-abbrev)
(require 'init-dired)
(require 'init-help)
;;;; VIEW MODE
(setq view-read-only t) ;; Visit read-only buffers in view-mode by default


;;; Tools
(use-package daemons :straight t)
;; Package to show key-presses in a *command-log* buffer
;; https://github.com/lewang/command-log-mode
(use-package command-log-mode :straight t)
(require 'init-formatter)
(require 'init-git)
(use-package guix :straight t)
(require 'init-keyfreq)
(require 'init-lsp)
(require 'init-projectile)
(require 'init-tree-sitter)
;;;; TERM SETTINGS
(use-package term
  :straight t)

;;; Features
(require 'feature-multiple-cursors)
(require 'feature-search)

;;; Languages
;; TODO move these to lang-* files
(require 'lang-c)
(require 'init-cfml)
(require 'lang-guile)
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

;;;; Electric pairs
(electric-pair-mode 1)

;;; User setup
(require 'user-keybindings)
(require 'user-lisp)

;;; init.el ends here
