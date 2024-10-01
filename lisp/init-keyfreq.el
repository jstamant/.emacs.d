;;; init-keyfreq.el --- Keypress analytics via keyfreq -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))


(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
