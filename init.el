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


(setq user-full-name "Justin St-Amant")

(defvar using-windows
  (equal window-system 'w32)
  "t if emacs is running in windows")

(defvar drive-directory
  (if (not using-windows)
      "~/drive/"
    (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
      (concat userprofile "/Google Drive/")))
  "The absolute path to the Google Drive directory under Linux or Windows.")

(defvar onedrive-directory
  (if using-windows
      (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
        (concat userprofile "/OneDrive - Manitoba Hydro/")))
  "The absolute path to your work OneDrive directory. Only for Windows.")

(if using-windows
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

;; TODO change font? Set as default, but customizable
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 100)
;;(set-frame-font "DejaVu Sans Mono 10" nil t)


(use-package command-log-mode :ensure t)

(require 'init-theme)
(require 'init-modeline)

(require 'init-editor)

(setq delete-by-moving-to-trash t) ; Use trash instead of destroying files

(use-package savehist
  :straight t
  :config
  (setq history-length 100)
  (savehist-mode 1))
(use-package saveplace
  :straight t
  :config
  (save-place-mode 1))

;; Set sentences to be identified by a period and a single space, instead of two spaces
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Make a quick function to insert today's date in ISO 8601
;; TODO move this to an abbrev
(defun dts ()
  "Insert today's date in ISO 8601 (YYYY-MM-DD)."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ALIASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO move these to toggles

;; Major-mode aliases
(defalias 'o 'org-mode)
(defalias 'org 'org-mode)

;; Minor-mode aliases
(defalias 'afm 'auto-fill-mode)
(defalias 'fsm 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; AUTO-REVERT MODE SETTINGS
;;TODO NEED TO ADD A TOGGLE FOR AUTO-REVERT-MODE
;; (use-package autorevert
;;   :config
;;   (setq global-auto-revert-non-file-buffers t)
;;   (global-auto-revert-mode 1))

;;;; CALC SETTINGS
(use-package calc
  :bind ("C-x c" . calc))

;;;; C/C++ MODE SETTINGS
;; clangd needs to be installed for LSP-use
(use-package cc-mode
  :straight t
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4))

;;;; DIRED SETTINGS
(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("TAB" . dired-find-file)))
;; Place this Inside `use-package dired`???
;; (use-package dired-single)
;;     (evil-collection-define-key 'normal 'dired-mode-map
;;       "h" 'dired-single-up-directory
;;       "l" 'dired-single-buffer)
;; (use-package dired-open
;;   :config
;;   ;; Doesn't work as expected!
;;   (add-to-list 'dired-open-functions #'dired-open-xdg t)
;;   ;; -- OR! --
;;   (setq dired-open-extensions '(("png" . "feh")
;;                                 ("mkv" . "mpv"))))
;; (use-package dired-hide-dotfiles
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "H" 'dired-hide-dotfiles-mode))

;;;; EMOJIFY SETTINGS
;; Not sure if I like this mode.
;; TODO add a toggle for this, which should be a hydra
;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :init
;;   (setq emojify-display-style 'image) ; Or can be set to 'unicode
;;   (setq emojify-emoji-styles '(unicode github))) ; Don't display ascii emojis


(require 'init-meow)
;; (require 'init-evil)

;;;; FLYSPELL SETTINGS
;; TODO add a toggle for flyspell mode
;; (use-package flyspell
;;   :hook (text-mode . flyspell-mode)) ; Start spell checking on all modes derived from text-mode

;;;; HELM-MODE SETTINGS
;;(use-package helm)

;;;; HELP-MODE SETTINGS
(use-package help-mode
  :bind-keymap
  ("C-c h" . help-map) ;; That's it! That's how you bind help-map to another key?? See below, figured it out
  :bind (:map help-mode-map
              ;; Some additional navigation bindings
              ;; TODO move these, as they are emacs keybindings, not really meant for evil mode
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("v" . scroll-up-command)
              ("V" . scroll-down-command)
              ("C-d" . scroll-up-command)))
;;(define-key evil-normal-state-map (kbd "SPC h") help-map) ;; Found it! This is how you do maps!


(require 'init-spelling)
(require 'init-ledger)

;;;; MAGIT SETTINGS
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

(require 'init-org)

;;;; PAREN SETTINGS
;; TODO move the toggle for show-paren-mode
(use-package paren
  :ensure t
  :bind ("C-x p" . show-paren-mode))

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t
  :config
  (setq pkgbuild-update-sums-on-save nil))

;;;; RAINBOW DELIMITERS SETTINGS
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(recentf-mode 1)

;;;; SHELL-SCRIPT SETTINGS
(use-package sh-script
  :ensure t
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))

;;;; TERM SETTINGS
(use-package term
  :ensure t)

;;;; TREESITTER SETTINGS
;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;   :after tree-sitter)

;;;; VIEW MODE
(use-package view
  :bind ("C-x v" . view-mode))

;;;; WEB SETTINGS
;; (use-package web-mode
;;   :mode
;;   "\\.css\\'"
;;   "\\.htaccess\\'"
;;   "\\.html?\\'"
;;   "\\.twig\\'"
;;   "\\.php\\'"
;;   "\\.xml\\'"
;;   :config
;;   ;; Make .html files recognize Twig templates by default
;;   (setq web-mode-engines-alist '(("twig" . "\\.html\\'")))
;;   ;; web-mode indentation settings
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;;;; WHICH-KEY SETTINGS
;; which-key is responsible for showing your options along a key sequence.
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))




;; (use-package counsel
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x b" . counsel-ibuffer)
;;          ("C-x C-f" . counsel-find-file)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history)))


;; Helpful provides us with a help-mode that shows prettier and better organized
;; help content than the default help-mode
;; I don't like its navigation, though. It doesn't reuse the same window
;; (use-package helpful)
;; (use-package helpful
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

(require 'init-lisp)
(require 'init-projectile)

;;;; LSP SETTINGS
;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred) ; defer loading until one of these commands are executed
;;   ;;:after which-key
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (lsp-mode . lsp-enable-which-key-integration))
;; ;;(XXX-mode . lsp) ; for enabling lsp upon entering certain modes

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))
;; (setq lsp-ui-doc-position 'bottom)
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-sideline-show-hover nil)

;; (use-package lsp-treemacs
;;   :after lsp)

;; (use-package lsp-ivy)

;; (use-package company
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :bind
;;   ;; TODO bind `company-complete'??
;;   (:map company-active-map
;;         ("<tab>" . company-complete-selection))
;;   (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; This adds extra info to company mode, which appears as a box, by default
;; TODO need to disable this if in a tty. Not compatible.
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))
;; ;;npm install -g typescript-language-server

;; I don't find this package useful yet?
;; Don't forget to use M-; for comment-dwim
;; (use-package evil-nerd-commenter
;;   :after evil
;;   :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; Unsure if unfill works with comments of other programming languages
;; And how exactly is this being deferred?
;; (use-package unfill
;;   :defer t
;;   :commands (unfill-region unfill-paragraph unfill-toggle)
;;   :init
;;   (global-set-key [remap fill-paragraph] #'unfill-toggle))


;; Another package to implement one day:
;; The main one is (use-package dashboard)
;; (use-package welcome-dashboard) is that right??

;; Packages that don't have any configuration
(use-package lua-mode :ensure t)
(use-package markdown-mode :ensure t)

;; Finally, load variables configured from the 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
