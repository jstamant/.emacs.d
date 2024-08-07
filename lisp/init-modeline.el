;;; init-modeline.el --- Modeline setup and configuration with doom-modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 21)
  (setq doom-modeline-icon t)
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-modal-modern-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package nerd-icons
  :ensure t)


(provide 'init-modeline)
;;; init-modeline.el ends here
