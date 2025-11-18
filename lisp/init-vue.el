;;; init-vue.el --- Editor setup for Vue using polymode -*- lexical-binding: t -*-
;;; Commentary:

;; `web-mode' will likely see performance issues if you use
;; `highlight-indent-guides-mode'

;;; Code:


(use-package web-mode :straight t)

(autoload 'web-mode "web-mode")
(define-derived-mode vue-mode web-mode "Vue")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(add-hook 'vue-mode-hook 'lsp-deferred)

(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-mode-alist '(vue-mode . prettier-javascript)))


(provide 'init-vue)
;;; init-vue.el ends here
