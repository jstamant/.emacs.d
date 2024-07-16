;;; init-vue.el --- Editor setup for Vue using polymode -*- lexical-binding: t -*-
;;; Commentary:

;; `web-mode' will likely see performance issues if you use
;; `highlight-indent-guides-mode'

;;; Code:


(with-eval-after-load 'web-mode
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))


;; Polymode splits the buffer into different modes, so this doesn't really work
;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-mode-alist '(vue-mode . prettier-javascript)))


(provide 'init-vue)
;;; init-vue.el ends here
