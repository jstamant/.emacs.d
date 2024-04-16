;;; init-lsp.el --- LSP setup -*- lexical-binding: t -*-

;;; Commentary:

;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/


;;; Code:


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-auto-execute-action nil) ;; Do not auto-execute single actions
  (setq lsp-completion-provider :none)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-indent-level))
  )

;; ;; optionally
;; (use-package consult-lsp :ensure t)
;; (use-package lsp-ui) ;; with this, you can see the code-actions, and re-enable auto-execute above
;; (use-package lsp-treemacs)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)


(provide 'init-lsp)
;;; init-lsp.el ends here
