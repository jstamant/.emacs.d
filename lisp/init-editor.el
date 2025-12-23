;;; init-editor.el --- Text editing setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(column-number-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(use-package highlight-indent-guides
  :straight t
  :diminish highlight-indent-guides-mode
  ;; This package has terrible performance in files with lots of indentation,
  ;; like in web-mode files
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-auto-character-face-perc 50))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)
;; Or...Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (setq-default show-trailing-whitespace t) ;; setq-default to enable globally
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Sensible formatting defaults. Enable `visual-line-mode' if you want
;; soft line-wrapping. `auto-fill-mode' for hard line-wrapping.
(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq require-final-newline t)
(setq sentence-end-double-space nil)

(use-package autorevert
  :diminish auto-revert-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-define-key "C-:" 'exchange-point-and-mark)

;; `comment-line' is my preferred comment command, and suits my usage
;;  better than `comment-dwim'
;; (general-translate-key "M-;" "C-x C-;")
;; (jrs/emacs-leader-keys
;;     ";" 'comment-line)
(jrs/emacs-leader-keys
    ";" 'comment-dwim)


(use-package expand-region
  :straight t
  :commands er/expand-region)

(general-define-key "C-;" 'er/expand-region)
(jrs/emacs-leader-keys
  "s" 'er/expand-region)

(defalias 'afm 'auto-fill-mode)

(keymap-set toggles-map "f" '("auto-fill" . abbrev-mode))
(keymap-set toggles-map "h" 'gobal-hl-line-mode)


(provide 'init-editor)
;;; init-editor.el ends here
