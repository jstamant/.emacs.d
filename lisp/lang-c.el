;;; lang-c.el --- C/C++ settings and setup -*- lexical-binding: t -*-
;;; Commentary:

;; clangd needs to be installed for LSP-use

;;; Code:


(use-package cc-mode
  :straight t
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4))


(provide 'lang-c)
;;; lang-c.el ends here
