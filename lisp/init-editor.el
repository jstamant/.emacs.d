;;; init-editor.el --- Text editing setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(column-number-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(use-package highlight-indent-guides
  :ensure t
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

(use-package autorevert
  :diminish auto-revert-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set sentences to be identified by a period and a single space, instead of two spaces
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

(general-define-key "C-:" 'exchange-point-and-mark)

;; `comment-line' is my preferred comment command, and suits my usage
;;  better than `comment-dwim'
;; (general-translate-key "M-;" "C-x C-;")
;; (jrs/emacs-leader-keys
;;     ";" 'comment-line)
(jrs/emacs-leader-keys
    ";" 'comment-dwim)


(use-package expand-region
  :ensure t
  :defer t)

(general-define-key "C-;" 'er/expand-region)
(jrs/emacs-leader-keys
  "s" 'er/expand-region)

(defalias 'afm 'auto-fill-mode)

(keymap-set toggles-map "f" '("auto-fill" . abbrev-mode))
(keymap-set toggles-map "h" 'gobal-hl-line-mode)


(provide 'init-editor)
;;; init-editor.el ends here
