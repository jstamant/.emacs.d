;;; init-modeline.el --- Modeline setup and configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TODO implement doom-modeline (this is what spacemacs uses on graphical displays
;; and implement either sml or vim-powerline for non-graphical displays
;; TODO idk...I kinda preferred smart-mode-line.
;; I don't like how all the mode lighters are missing.
(use-package doom-modeline
  :straight t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 15))

(use-package nerd-icons
  :straight t)
  ;;:config
  ;;(nerd-icons-install-fonts t)) ; TODO only install if missing!

;; (use-package smart-mode-line
;;   :straight t
;;   :config
;;   (setq sml/theme 'dark)
;;   ;; Enable line numbering and column numbering
;;   (setq line-number-mode t)
;;   (setq column-number-mode t)
;;   ;; Enable smart mode-line (required)
;;   (sml/setup))


(provide 'init-modeline)
;;; init-modeline.el ends here
