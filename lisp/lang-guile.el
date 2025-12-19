;;; lang-guile.el --- Guile setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package geiser
  :straight t
  :init
  (setq geiser-debug-jump-to-debug nil))

(use-package geiser-guile
  :straight t)


(provide 'lang-guile)
;;; lang-guile.el ends here
