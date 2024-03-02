;;; init-javascript.el --- Setup for JavaScript and TypeScript -*- lexical-binding: t -*-

;;; Commentary:

;; There are many packages available for dealing with JavaScript and
;; TypeScript. From my testing, tsx-ts-mode works the best and is
;; included with Emacs 29. It's the only package that had proper
;; indentation out of the box.

;; TODO improve typescript-mode by implementing indentation from LSP

;; Both typescript-mode and typescript-ts-mode define a lot of modes.
;; Keep that in mind while troubleshooting.

;; typescript-language-server and typescript must be installed for using LSP:
;;   $ npm install -g typescript-language-server
;;   $ npm install -g typescript

;; TODO need to implement eslint and prettier

;;; Code:


(use-package typescript-ts-mode
  :ensure t
  :mode (("\\.tsx?\\'" . tsx-ts-mode)
         ("\\.jsx?\\'" . tsx-ts-mode))
  :hook (tsx-ts-mode . lsp-deferred)
  :init
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (setq typescript-indent-level 2))


(provide 'init-javascript)
;;; init-javascript.el ends here
