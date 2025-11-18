;;; init-json.el --- JSON settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package json-mode
  :straight t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


(provide 'init-json)
;;; init-json.el ends here
