;;; init-lsp.el --- LSP setup -*- lexical-binding: t -*-

;;; Commentary:

;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/


;;; Code:


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp-mode)
  (lsp-mode . lsp-enable-which-key-integration))

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))
;; (setq lsp-ui-doc-position 'bottom)
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-sideline-show-hover nil)


(provide 'init-lsp)
;;; init-lsp.el ends here
