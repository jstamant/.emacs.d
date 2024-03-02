;;; init-tree-sitter.el --- Initialize tree-sitter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; TODO grammars need to be moved to the tree-sitter folder after they're compiled
;; TODO figure out deferring of tree-sitter-langs?
(use-package tree-sitter-langs
  :ensure t
  :diminish tree-sitter-mode)
  ;; :after tree-sitter)


(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
