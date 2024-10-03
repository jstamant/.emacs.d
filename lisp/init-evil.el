;;; init-evil.el --- Evil setup for modal editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t) ; required for evil-collection
  (setq evil-want-keybinding nil) ; required for evil-collection
  :config
  ;; TODO Override C-v to scroll instead of activating visual block
  ;; I'd like to toggle the visual type with a key
  ;; (general-def 'normal "C-v" 'scroll-up-command)
  ;; (general-def 'visual "V" 'evil-visual-block)
  (evil-mode))

(defun jrs/scroll-up ()
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center nil))

(defun jrs/scroll-down ()
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center nil))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

;; Used for better default keybinds in Emacs modes
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; (use-package evil-org ; Provides evil keybinds for org-mode
;;   :ensure t
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(general-spc "u" 'universal-argument)


(provide 'init-evil)
;;; init-evil.el ends here
