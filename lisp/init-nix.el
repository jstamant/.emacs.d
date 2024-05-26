;;; init-nix.el --- Mode settings for nix files -*- lexical-binding: t -*-
;;; Commentary:

;; All of this configuration was copied from nix-mode's github page

;;; Code:


(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


(provide 'init-nix)
;;; init-nix.el ends here
