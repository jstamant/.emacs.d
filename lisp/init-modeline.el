;;; init-modeline.el --- Modeline setup and configuration with doom-modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-icon t)
  (setq doom-modeline-modal t
        doom-modeline-modal-icon nil
        doom-modeline-modal-modern-icon t))

(use-package nerd-icons
  :ensure t)


(provide 'init-modeline)
;;; init-modeline.el ends here
