;;; feature-direnv.el --- direnv/envrc configuration -*- lexical-binding: t -*-
;;; Commentary:

;; direnv integration provided by Steve Purcell's `envrc' package.

;;; Code:


(use-package envrc
  :straight t
  :defer t
  :hook (after-init . envrc-global-mode)
  :config
  (keymap-set mode-specific-map "d" (cons "direnv" envrc-command-map)))


(provide 'feature-direnv)
;;; feature-direnv.el ends here
