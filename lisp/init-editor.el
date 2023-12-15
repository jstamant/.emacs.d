;;; init-editor.el --- Text editing setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(column-number-mode 1)
;; TODO - add a toggle for this
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
;; Or...Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq show-trailing-whitespace t)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(provide 'init-editor)
;;; init-editor.el ends here