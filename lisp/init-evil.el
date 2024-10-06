;;; init-evil.el --- Evil setup for modal editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :init
  ;; NOTE that most of these MUST be set before evil loads
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t) ; required for evil-collection
  (setq evil-want-keybinding nil) ; required for evil-collection
  (setq evil-undo-system 'undo-redo)
  :config
  ;; TODO Override C-v to scroll instead of activating visual block
  ;; I'd like to toggle the visual type with a key
  ;; (general-def 'normal "C-v" 'scroll-up-command)
  ;; (general-def 'visual "V" 'evil-visual-block)
  (evil-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

;; Used for better default keybinds in Emacs modes
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; https://github.com/redguardtoo/evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (setq evilnc-comment-text-object ";")
  (keymap-set evil-inner-text-objects-map evilnc-comment-text-object 'evilnc-inner-commenter)
  (keymap-set evil-outer-text-objects-map evilnc-comment-text-object 'evilnc-outer-commenter)
  :config
  (keymap-set global-map "M-;" 'evilnc-comment-or-uncomment-lines)
  (general-spc
    ";" '(:ignore t :which-key "comment")
    "; ;" 'evilnc-comment-or-uncomment-lines
    "; c" 'evilnc-copy-and-comment-lines
    "; i" 'evilnc-toggle-invert-comment-line-by-line
    "; s" 'evilnc-comment-box
    "; t" 'evilnc-comment-or-uncomment-html-tag
    "; v" 'evilnc-toggle-comment-empty-lines
    "; y" 'evilnc-comment-and-kill-ring-save))

(general-spc "u" 'universal-argument)


(provide 'init-evil)
;;; init-evil.el ends here
