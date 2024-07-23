;;; init-nix.el --- Mode settings for nix files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/oxalica/nil
(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(add-hook 'nix-mode-hook 'lsp-deferred)

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
