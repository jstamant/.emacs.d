;;; init-completions.el --- Set up auto-completion system -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Saves previous minibuffer entries
(use-package savehist
  :ensure t
  :config
  (setq history-length 100)
  ;; Also make the kill-ring persistent
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (savehist-mode 1))


;; Saves last-visited point in buffers
(use-package saveplace
  :ensure t
  :config
  (save-place-mode 1))


;; Defines the style of completions in the minibuffer
;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides nil))


;; Enables rich annotations in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;; Vertico is for minibuffer completions only
;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))


;; `completing-read' via Consult
;; https://github.com/minad/consult
(use-package consult :ensure t)


(provide 'init-completions)
;;; init-completions.el ends here
