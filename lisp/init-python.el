;;; init-python.el --- Python programming setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python
  :straight t
  :defer t)

(setq python-indent-offset 4)

(add-hook 'python-mode-hook 'lsp-deferred)


(provide 'init-python)
;;; init-python.el ends here
