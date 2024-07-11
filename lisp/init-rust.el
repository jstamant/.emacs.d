;;; init-rust.el --- Rust dev setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  ;; If I want to use rust-ts-mode, I'll need to setup the bindings from C-c C-c C-*
  ;; :mode ("\\.rs\\'" . rust-ts-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save nil))


(provide 'init-rust)
;;; init-rust.el ends here
