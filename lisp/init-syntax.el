;;; init-syntax.el --- Syntax-checking via flymake -*- lexical-binding: t -*-
;;; Commentary:

;; If you run into issues with Flymake, then maybe Flycheck may be a
;; better alternative.

;;; Code:


(use-package flymake
  :ensure t)

(with-eval-after-load 'flymake
  (jrs/emacs-leader-keys
    :keymaps 'flymake-mode-map
    "!" '(:ignore t :which-key "flymake")
    "!b" 'flymake-show-buffer-diagnostics
    "!c" 'flymake-start
    "!n" 'flymake-goto-next-error
    "!p" 'flymake-goto-prev-error
    "!P" 'flymake-show-project-diagnostics))


(provide 'init-syntax)
;;; init-syntax.el ends here
