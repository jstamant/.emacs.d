;;; init-sh.el --- Shell scripting settings -*- lexical-binding: t -*-
;;; Commentary:

;; My default shell is bash, so my settings are based on that.

;;; Code:


(use-package sh-script
  :ensure t
  ;; hooks for `sh-mode' are automatically autoloaded
  :defer t
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))


(provide 'init-sh)
;;; init-sh.el ends here
