;;; feature-lsp.el --- LSP setup -*- lexical-binding: t -*-

;;; Commentary:

;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/

;;; Code:


(straight-use-package '(eglot :type built-in))
(defvar-keymap eglot-command-map
  :doc "Keymap for accessing commands related to LSP. Could also be renamed to
something like lsp-command-map."
  "c" 'eglot-code-actions
  "e" 'eglot
  "f" 'eglot-format
  "h" '("hints" . eglot-inlay-hints-mode)
  "q" 'eglot-shutdown
  "Q" 'eglot-shutdown-all
  "r" 'eglot-rename
  "R" 'eglot-reconnect)


(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-auto-execute-action nil) ;; Do not auto-execute single actions
  (setq lsp-completion-provider :none)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-keymap-prefix "C-c l") ;; Sets the keybind in lsp-mode-map
  :config
  (general-spc :keymap 'lsp-mode-map "l" (cons "lsp" lsp-command-map))
  ;; (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-indent-level))
  )

;; ;; optionally
;; (use-package consult-lsp :straight t)
;; (use-package lsp-ui) ;; with this, you can see the code-actions, and re-enable auto-execute above
;; (use-package lsp-treemacs)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)


(provide 'feature-lsp)
;;; feature-lsp.el ends here
