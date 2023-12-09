;;; init-evil.el --- Evil setup for modal editing -*- lexical-binding: t -*-

;;; Commentary:

;; This part of my config is outdated! I don't use it! If I use it in
;; the future, I'll need to update it and integrate it nicely!

;; I've also combined my keybindings file in here, as they belong
;; together.

;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t) ; required for evil-collection
  (setq evil-want-keybinding nil) ; required for evil-collection
  :config
  ;;(evil-set-initial-state 'help-mode 'emacs)
  ;;(evil-set-initial-state 'dired-mode 'emacs)
  (advice-add 'evil-search-next :after #'jstamant/scroll-line-to-center)
  (advice-add 'evil-search-previous :after #'jstamant/scroll-line-to-center)
  ;; This advice causes issues, for some reason
  ;; (advice-add 'evil-scroll-up :after #'jstamant/scroll-line-to-center)
  ;; (advice-add 'evil-scroll-down :after #'jstamant/scroll-line-to-center)
  (evil-mode 1))

(defun jstamant/scroll-up ()
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center nil))

(defun jstamant/scroll-down ()
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center nil))

(defun jstamant/scroll-line-to-center (&rest _)
  "My own scroll-line-to-center / recenter function.
To be used for advising functions."
  (evil-scroll-line-to-center nil))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

; Used for better default keybinds in Emacs modes
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Might be cool to implement - keybindings to inc/dec numbers quickly
;; (use-package evil-numbers
;;   :ensure t)

(use-package evil-org ; Provides evil keybinds for org-mode
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

(use-package general
  :ensure t)

;; TO MOVE THESE INTO USE-PACKAGE??
;;(general-evil-setup) ; Use this if you want key definers like general-nmap or general-imap

(general-create-definer jstamant/leader-key
  :prefix "SPC"
  :global-prefix "M-SPC")
;;:non-normal-prefix "M-SPC" ; What about this? Using non-normal-prefix

(general-create-definer jstamant/local-leader-key
  :prefix ","
  :global-prefix "M-,")

;; TODO (REQUIRE) HELPFUL?
; None of these <leader> prefixed keys would work in help-mode without
; being in the override map
(jstamant/leader-key
  ;; :keymaps 'normal
  :states 'normal
  :keymaps 'override
  "" '(nil :which-key "<leader>")
  "b" '(:ignore t :which-key "buffers")
  "bb" 'switch-to-buffer
  "bl" 'list-buffers
  "bd" 'kill-buffer
  "bk" 'kill-buffer
  "f" '(:ignore t :which-key "files")
  "fs" 'save-buffer
  "ff" 'find-file
  "fv" 'find-alternate-file
  "fe" '(:ignore t :which-key "emacs-related files")
  "fed" '((lambda () (interactive) (find-file user-init-file)) :which-key "dotfile")
  "fee" '((lambda () (interactive) (find-file (expand-file-name "jstamant-evil.el" user-lisp-directory))) :which-key "evil")
  "fek" '((lambda () (interactive) (find-file (expand-file-name "jstamant-keybinds.el" user-lisp-directory))) :which-key "keybinds")
  "fr" 'recentf-open-files
  "fd" 'dired
  "g" '(:ignore t :which-key "git")
  "gg" 'magit
  "gs" 'magit-status
  "h" '(:ignore t :which-key "help")
  "hd" '(:ignore t :which-key "describe")
  "hdf" 'helpful-callable
  ;;"hdf" 'helpful-callable ;TODO add a describe-face?
  "hdm" 'describe-mode
  "hdv" 'helpful-variable
  "l" '(:keymap lsp-command-map :package lsp :which-key "lsp") ; doesn't work the greatest
  "p" '(:keymap projectile-command-map :package projectile :which-key "projectile")
  "t"  '(:ignore t :which-key "toggles") ;not added by me
  "tt" '(counsel-load-theme :which-key "choose theme") ;not added by me
  "w" '(:ignore t :which-key "windows")
  "ww" 'other-window
  "wo" 'other-window
  "w1" 'delete-other-windows
  "w0" 'delete-window
  "wd" 'delete-window
  "wk" 'delete-window)

(jstamant/local-leader-key
  :keymaps 'normal
  "" '(:ignore t :which-key "mode-specific bindings")
  "h" 'describe-mode)

(general-define-key 
 :keymaps '(normal insert)
 ;; "C-a" 'move-beginning-of-line
 ;; "C-e" 'move-end-of-line
 "C-u" 'jstamant/scroll-up
 "C-d" 'jstamant/scroll-down)

;; (general-define-key
;;  "C-c p" '(:keymap projectile-command-map :package projectile))




(provide 'init-evil)
;;; init-evil.el ends here
