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


;; Embark is for context-sensitive minibuffer actions
;; https://github.com/oantolin/embark
(use-package embark
  :ensure t
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(with-eval-after-load 'embark
  (keymap-set help-map "B" 'embark-bindings)
  (keymap-set minibuffer-mode-map "C-." 'embark-act)
  (keymap-global-set "M-." 'embark-dwim)) ;; Good replacement for `xref-find-definitions'

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'init-completions)
;;; init-completions.el ends here
