;;; init-vue.el --- Editor setup for Vue using polymode -*- lexical-binding: t -*-
;;; Commentary:

;; I'm avoiding `web-mode' as one of the poly-innermodes, as it
;; noticeably slows this polymode down

;; `web-mode' by itself works, but is slow

;; `vue-mode' works, but the script portion isn't highlighted, and the
;; html portion is slower than web-mode

;;; Code:


(use-package polymode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode)
  :config
  (define-innermode
   poly-vue-template-innermode
   :mode 'html-mode
   :head-matcher "^<[[:space:]]*\\(?:template\\)[[:space:]]*>"
   :tail-matcher "^</[[:space:]]*\\(?:template\\)[[:space:]]*>"
   :head-mode 'host
   :tail-mode 'host)
  (define-innermode
   poly-vue-script-innermode
   ;; :mode 'tsx-ts-mode
   :mode 'js-mode
   :head-matcher "<[[:space:]]*\\(?:script\\)[[:space:]]*>"
   :tail-matcher "</[[:space:]]*\\(?:script\\)[[:space:]]*>"
   :head-mode 'host
   :tail-mode 'host)
  (define-auto-innermode
   poly-vue-template-tag-lang-innermode
   :head-matcher "^<[[:space:]]*\\(?:template\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
   :tail-matcher "^</[[:space:]]*\\(?:template\\)[[:space:]]*>"
   :mode-matcher (cons  "^<[[:space:]]*\\(?:template\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
   :head-mode 'host
   :tail-mode 'host)
  (define-auto-innermode
   poly-vue-script-tag-lang-innermode
   :head-matcher "<[[:space:]]*\\(?:script\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>"
   :tail-matcher "</[[:space:]]*\\(?:script\\)[[:space:]]*>"
   :mode-matcher (cons  "<[[:space:]]*\\(?:script\\)[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
   :head-mode 'host
   :tail-mode 'host)
  (define-auto-innermode
   poly-vue-style-tag-lang-innermode
   :head-matcher "<[[:space:]]*\\(?:style\\)\\(?:scoped\\|[[:space:]]\\)*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"']*\\(?:scoped\\|[[:space:]]\\)*>"
   :tail-matcher "</[[:space:]]*\\(?:style\\)[[:space:]]*>"
   :mode-matcher (cons  "<[[:space:]]*\\(?:style\\)\\(?:scoped\\|[[:space:]]\\)*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"']\\(?:scoped\\|[[:space:]]\\)*>" 1)
   :head-mode 'host
   :tail-mode 'host)
  (define-innermode
   poly-vue-style-innermode
   :mode 'css-mode
   :head-matcher "<[[:space:]]*\\(?:style\\)[[:space:]]*\\(?:scoped\\|[[:space:]]\\)*>"
   :tail-matcher "</[[:space:]]*\\(?:style\\)[[:space:]]*>"
   :head-mode 'host
   :tail-mode 'host)

  (define-polymode
   vue-mode
   :hostmode 'poly-sgml-hostmode
   :innermodes
   '(poly-vue-template-tag-lang-innermode
     poly-vue-script-tag-lang-innermode
     poly-vue-style-tag-lang-innermode
     poly-vue-template-innermode
     poly-vue-script-innermode
     poly-vue-style-innermode)))


;; Polymode splits the buffer into different modes, so this doesn't really work
;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-mode-alist '(vue-mode . prettier-javascript)))


(provide 'init-vue)
;;; init-vue.el ends here
