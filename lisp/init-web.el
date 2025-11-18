;;; init-web.el --- Web-related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package web-mode
  :straight t
  :mode
  "\\.css\\'"
  "\\.htaccess\\'"
  "\\.html?\\'"
  "\\.twig\\'"
  "\\.php\\'"
  "\\.xml\\'"
  :config
  ;; Make .html files recognize Go templates by default (for hugo)
  (setq web-mode-engines-alist '(("go" . "\\.html\\'")))
  ;; web-mode indentation settings
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


(provide 'init-web)
;;; init-web.el ends here
