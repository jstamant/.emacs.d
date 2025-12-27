;;; lang-zig.el --- Zig settings and setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package zig-mode
  :straight t
  :defer t)

;; TODO require feature-tree-sitter when that's setup, and make sure tree-sitter-hl-mode is disabled?
;; (use-package zig-ts-mode
;;   :straight t
;;   :defer t
;;   :mode "\\.zig\\(?:\\.zon\\)?\\'"
;;   :init
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(zig-ts-mode . zig)))


(provide 'lang-zig)
;;; lang-zig.el ends here
