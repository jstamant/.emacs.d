;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking)

;; Load my customizations
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(if (not (file-exists-p custom-file)) (with-temp-buffer (write-file custom-file)))
(load custom-file 'noerror 'nomessage)

;; Initialize Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Require use-package, and install it if necessary
(if (not (require 'use-package "use-package" t))
    (progn (package-refresh-contents)
           (package-install 'use-package)))
;;(setq use-package-always-ensure t)
;; TODO - don't refresh packages if you don't have to! What about this:
;; (unless package-archive-contents
;;   (package-refresh-contents))

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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Do not inherit any colors or settings from X resources
(setq inhibit-x-resources t)
;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Disable GUI elements
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)        ; Give some breathing room??
(setq inhibit-startup-screen t) ; Disable the default Emacs startup screen
(setq use-dialog-box nil) ; Disables dialog boxes for mouse-driven actions
;;(setq scroll-margin 8)

;; Sound settings
(setq ring-bell-function 'ignore) ; Turn off audible bell
(setq visible-bell t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Miscellaneous settings
(setq scroll-preserve-screen-position t) ; Keep point position on screen when scrolling
(setq show-trailing-whitespace t)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;TODO SET THE FONT TO SOMETHING THAT WILL SUPPORT EMOJI?
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 100)

(column-number-mode 1)
;; TODO - add a toggle for this
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
;; Or...Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode
  :ensure t)

;;;; THEMES
(use-package base16-theme
  :ensure t)
(use-package spacemacs-theme
  :ensure t)
(global-hl-line-mode 1) ;; This currently overlays faces, so you can't use `describe-face' on them
(use-package modus-themes
  :ensure t)
(load-theme 'spacemacs-dark t)
;; Configure the Modus Themes' appearance
;;(setq modus-themes-mode-line '(accented borderless)
;;      modus-themes-bold-constructs t
;;      modus-themes-italic-constructs t
;;      modus-themes-fringes 'subtle
;;      modus-themes-tabs-accented t
;;      modus-themes-paren-match '(bold intense)
;;      modus-themes-prompts '(bold intense)
;;      modus-themes-completions 'opinionated
;;      modus-themes-org-blocks 'tinted-background
;;      modus-themes-scale-headings t
;;      modus-themes-region '(bg-only)
;;      modus-themes-headings
;;      '((1 . (rainbow overline background 1.4))
;;        (2 . (rainbow background 1.3))
;;        (3 . (rainbow bold 1.2))
;;        (t . (semilight 1.1))))
;; Load the dark theme by default
;;(load-theme 'modus-vivendi t)
;;(blink-cursor-mode -1)

;; Mode-line settings
(use-package jstamant-modeline)

(setq delete-by-moving-to-trash t) ; Use trash instead of destroying files

(use-package savehist
  :config
  (setq history-length 100)
  (savehist-mode 1))
(use-package saveplace
  :config
  (save-place-mode 1))

;; Set sentences to be identified by a period and a single space, instead of two spaces
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Make a quick function to insert today's date in ISO 8601
(defun dts ()
  "Insert today's date in ISO 8601 (YYYY-MM-DD)."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ALIASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;;;; CALC SETTINGS
(use-package calc
  :bind ("C-x c" . calc))

;;;; C/C++ MODE SETTINGS
;; clangd needs to be installed for LSP-use
(use-package cc-mode
  :ensure t
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
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
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
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :init
  (setq emojify-display-style 'image) ; Or can be set to 'unicode
  (setq emojify-emoji-styles '(unicode github))) ; Don't display ascii emojis

;;;; EVIL SETTINGS
;;(use-package jstamant-evil)

;;;; MEOW SETTINGS

(defun meow-setup ()
 (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
 (meow-motion-overwrite-define-key
  '("j" . meow-next)
  '("k" . meow-prev)
  '("<escape>" . ignore))
 (meow-leader-define-key
  ;; SPC j/k will run the original command in MOTION state.
  '("j" . "H-j")
  '("k" . "H-k")
  ;; Use SPC (0-9) for digit arguments.
  '("1" . meow-digit-argument)
  '("2" . meow-digit-argument)
  '("3" . meow-digit-argument)
  '("4" . meow-digit-argument)
  '("5" . meow-digit-argument)
  '("6" . meow-digit-argument)
  '("7" . meow-digit-argument)
  '("8" . meow-digit-argument)
  '("9" . meow-digit-argument)
  '("0" . meow-digit-argument)
  '("/" . meow-keypad-describe-key)
  '("?" . meow-cheatsheet))
 (meow-normal-define-key
  '("0" . meow-expand-0)
  '("9" . meow-expand-9)
  '("8" . meow-expand-8)
  '("7" . meow-expand-7)
  '("6" . meow-expand-6)
  '("5" . meow-expand-5)
  '("4" . meow-expand-4)
  '("3" . meow-expand-3)
  '("2" . meow-expand-2)
  '("1" . meow-expand-1)
  '("-" . negative-argument)
  '(";" . meow-reverse)
  '("," . meow-inner-of-thing)
  '("." . meow-bounds-of-thing)
  '("[" . meow-beginning-of-thing)
  '("]" . meow-end-of-thing)
  '("a" . meow-append)
  '("A" . meow-open-below)
  '("b" . meow-back-word)
  '("B" . meow-back-symbol)
  '("c" . meow-change)
  '("d" . meow-delete)
  '("D" . meow-backward-delete)
  '("e" . meow-next-word)
  '("E" . meow-next-symbol)
  '("f" . meow-find)
  '("g" . meow-cancel-selection)
  '("G" . meow-grab)
  '("h" . meow-left)
  '("H" . meow-left-expand)
  '("i" . meow-insert)
  '("I" . meow-open-above)
  '("j" . meow-next)
  '("J" . meow-next-expand)
  '("k" . meow-prev)
  '("K" . meow-prev-expand)
  '("l" . meow-right)
  '("L" . meow-right-expand)
  '("m" . meow-join)
  '("n" . meow-search)
  '("o" . meow-block)
  '("O" . meow-to-block)
  '("p" . meow-yank)
  '("q" . meow-quit)
  '("Q" . meow-goto-line)
  '("r" . meow-replace)
  '("R" . meow-swap-grab)
  '("s" . meow-kill)
  '("t" . meow-till)
  '("u" . meow-undo)
  '("U" . meow-undo-in-selection)
  '("v" . meow-visit)
  '("w" . meow-mark-word)
  '("W" . meow-mark-symbol)
  '("x" . meow-line)
  '("X" . meow-goto-line)
  '("y" . meow-save)
  '("Y" . meow-sync-grab)
  '("z" . meow-pop-selection)
  '("'" . repeat)
  '("<escape>" . ignore)))

(use-package meow
 :ensure t
 :config
 (meow-setup)
 (meow-global-mode 1))

;; (use-package boon
;;   :ensure t
;;   :config
;;  (boon-mode))

;;;; FLYSPELL SETTINGS
;; TODO add a toggle for flyspell mode
;; (use-package flyspell
;;   :hook (text-mode . flyspell-mode)) ; Start spell checking on all modes derived from text-mode

;;;; HELM-MODE SETTINGS
;;(use-package helm
;;  :ensure t)

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

;;;; ISPELL SETTINGS
(use-package ispell
  :config
  (setq ispell-program-name "/usr/bin/aspell")) ; Change default spell checking program from ispell to aspell

;;;; IVY SETTINGS
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;;;; LEDGER SETTINGS
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :config
  (setq ledger-default-date-format ledger-iso-date-format) ; YYYY-MM-DD
  (setq ledger-highlight-xact-under-point nil) ; Screen is less cluttered without xact highlighting
  (setq ledger-mode-should-check-version nil) ; Ignore checking the 'ledger' binary
  (setq ledger-clear-whole-transactions t) ; For reconciliation, clear whole transactions, doesn't work great. It would be nice to have this only set during reconcile-mode
  (add-to-list 'ledger-reports
               '("uncleared" "%(binary) -f %(ledger-file) reg --uncleared"))
  ;; Fix auto-completion for ledger accounts
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t))))

(use-package ledger-report
  :bind (:map ledger-report-mode-map
              ("n"   . next-line)
              ("p"   . previous-line)
              ("TAB" . ledger-report-visit-source)))

(defun ledger ()
  "Shortcut for finding my ledger file, and navigating to the end
of the buffer."
  (interactive)
  (find-file (concat drive-directory "reference/finances/finances.ledger"))
  (end-of-buffer))

;;;; MAGIT SETTINGS
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

;; https://magit.vc/manual/forge/
;; https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :ensure t)

;;;; MARKDOWN SETTINGS
(use-package markdown-mode
  :ensure t)

;;;; MULTIPLE-CURSORS SETTINGS
(use-package multiple-cursors
  :ensure t
  :bind ("C-c m c" . mc/edit-lines))

;;;; ORG-MODE SETTINGS
(use-package jstamant-org)

;;;; PAREN SETTINGS
(use-package paren
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

;;;; RECENTF SETTINGS
(use-package recentf
  :config (recentf-mode 1))

;;;; SHELL-SCRIPT SETTINGS
(use-package sh-script
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))

;;;; TERM SETTINGS
(use-package term)

;;;; TREESITTER SETTINGS
;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;   :after tree-sitter)

;;;; UNFILL SETTINGS
(use-package unfill
  :ensure t)

;;;; VIEW MODE
(use-package view
  :ensure t
  :bind ("C-x v" . view-mode))

;;;; WEB SETTINGS
;; (use-package web-mode
;;   :ensure t
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




(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))


;; Helpful provides us with a help-mode that shows prettier and better organized
;; help content than the default help-mode
;; I don't like its navigation, though. It doesn't reuse the same window
(use-package helpful
  :ensure t)
;; (use-package helpful
;;   :ensure t
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))


;; Hydra is a package that lets you call functions at the end of a prefix
;; group without needing to call the prefix again.
;; The best example is the zoom in/out Hydra
(use-package hydra
  :ensure t)
;; (defhydra hydra-text-scale (:timeout 4)
;;           "scale text"
;;           ("j" text-scale-increase "in")
;;           ("k" text-scale-decrease "out")
;;           ("f" nil "finished" :exit t))
;; (defhydra hydra-zoom (global-map "<f2>")
;;           "zoom"
;;           ("g" text-scale-increase "in")
;;           ("l" text-scale-decrease "out"))
;; (rune/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))


(use-package projectile
  :ensure t
  ;; :after '(ag rg) ; optionally depends on these two packages
  ;;:diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; TODO remove this binding, it's been moved to general
  :init
  (when (file-directory-p "~/programming")
    (setq projectile-project-search-path '("~/programming")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf) ;; Does not apply to 'alien' sort order, which is what I use
  :config
  (projectile-mode 1))

;; Need these to perform ag and ripgrep searches using projectile
(use-package ag
  :ensure t)
(use-package rg
  :ensure t)
;; TODO to implement this package once I understand projectile a bit more
;; (use-package counsel-projectile
;;   :ensure t
;;   :after projectile
;;   :config
;;   (counsel-projectile-mode 1))


;;;; LSP SETTINGS
;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred) ; defer loading until one of these commands are executed
  ;;:after which-key
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))
;;(XXX-mode . lsp) ; for enabling lsp upon entering certain modes

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

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ivy
  :ensure t)

(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  ;; TODO bind `company-complete'??
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; This adds extra info to company mode, which appears as a box, by default
;; TODO need to disable this if in a tty. Not compatible.
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))
;; ;;npm install -g typescript-language-server

;; I don't find this package useful yet?
;; Don't forget to use M-; for comment-dwim
(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package lua-mode
  :ensure t)

;; This might be cool to implement - move to beginning of line or code
;; (use-package mwim
;;   :ensure t)

;; Unsure if unfill works with comments of other programming languages
;; And how exactly is this being deferred?
(use-package unfill
  :ensure t
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

;; This would be good to implement in emacs mode:
;; (defun spacemacs/backward-kill-word-or-region (&optional arg)
;;   "Calls `kill-region' when a region is active and
;; `backward-kill-word' otherwise. ARG is passed to
;; `backward-kill-word' if no region is active."
;;   (interactive "p")
;;   (if (region-active-p)
;;       ;; call interactively so kill-region handles rectangular selection
;;       ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
;;       (call-interactively #'kill-region)
;;     (backward-kill-word arg)))
;; (global-set-key (kbd "C-w") 'spacemacs/backward-kill-word-or-region)

;; Another package to implement one day:
;; The main one is (use-package dashboard)
;; (use-package welcome-dashboard) is that right??

(use-package jstamant-keybinds)
;;; init.el ends here
