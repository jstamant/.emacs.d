;;; init-keybindings.el --- Set up generic keybindings and definers with general.el -*- lexical-binding: t -*-

;;; Commentary:

;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

;;; Code:


(use-package general
  :ensure t)

(general-create-definer jrs/leader-key
  :prefix "SPC"
  :global-prefix "M-SPC")

(general-create-definer jrs/local-leader-key
  :prefix ","
  :global-prefix "M-,")

; None of these <leader> prefixed keys would work in help-mode without
; being in the override map
(jrs/leader-key
  ;; :keymaps 'normal
  :states 'normal
  :keymaps 'override
  "" '(nil :which-key "<leader>")
  ";" 'comment-dwim
  "b" '(:ignore t :which-key "buffers")
  "bb" 'switch-to-buffer
  "bl" 'list-buffers
  "bd" 'kill-this-buffer
  "bk" 'kill-this-buffer
  "f" '(:ignore t :which-key "files")
  "fs" 'save-buffer
  "ff" 'find-file
  "fv" 'find-alternate-file
  "fe" '(:ignore t :which-key "emacs-related files")
  "fed" '((lambda () (interactive) (find-file user-init-file)) :which-key "dotfile")
  "fee" '((lambda () (interactive) (find-file (expand-file-name "jrs-evil.el" user-lisp-directory))) :which-key "evil")
  "fek" '((lambda () (interactive) (find-file (expand-file-name "jrs-keybinds.el" user-lisp-directory))) :which-key "keybinds")
  "fr" 'recentf-open-files
  "fd" 'dired
  "fj" 'dired-jump
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
  "wd" 'delete-window
  "wh" 'windmove-left
  "wk" 'delete-window
  "wl" 'windmove-right
  "wo" 'other-window
  "ww" 'other-window
  "w0" 'delete-window
  "w1" 'delete-other-windows
  "w2" 'split-window-below
  "w3" 'split-window-right)

(jrs/local-leader-key
  :keymaps 'normal
  "" '(:ignore t :which-key "mode-specific bindings")
  "h" 'describe-mode)

(general-define-key 
 :keymaps '(normal insert)
 ;; "C-a" 'move-beginning-of-line
 ;; "C-e" 'move-end-of-line
 "C-u" 'jrs/scroll-up
 "C-d" 'jrs/scroll-down)

;; (general-define-key
;;  "C-c p" '(:keymap projectile-command-map :package projectile))


(provide 'init-keybindings)
;;; init-keybindings.el ends here
