;;; init-evil.el --- Evil setup for modal editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/emacs-evil/evil
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
  (advice-add 'evil-search-next :after #'jrs/scroll-line-to-center)
  (advice-add 'evil-search-previous :after #'jrs/scroll-line-to-center)
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


(provide 'init-evil)
;;; init-evil.el ends here
