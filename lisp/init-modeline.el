;;; init-modeline.el --- Modeline setup and configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TODO implement doom-modeline (this is what spacemacs uses on graphical displays
;; and implement either sml or vim-powerline for non-graphical displays
;; TODO idk...I kinda preferred smart-mode-line.
;; I don't like how all the mode lighters are missing.
(use-package doom-modeline
  :after nerd-icons
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 15))

(use-package nerd-icons
  :ensure t)


(provide 'init-modeline)
;;; init-modeline.el ends here
