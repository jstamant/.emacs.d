;;; init-lsp.el --- LSP setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; LSP SETTINGS
;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred) ; defer loading until one of these commands are executed
;;   ;;:after which-key
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (lsp-mode . lsp-enable-which-key-integration))
;; ;;(XXX-mode . lsp) ; for enabling lsp upon entering certain modes

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

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))
;; (setq lsp-ui-doc-position 'bottom)
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-sideline-show-hover nil)

;; (use-package lsp-treemacs
;;   :after lsp)

;; (use-package lsp-ivy)

(provide 'init-lsp)
;;; init-lsp.el ends here
