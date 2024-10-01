;;; init-pkgbuild.el --- Settings for PKGBUILDs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package pkgbuild-mode
  :ensure t
  :defer t
  :config
  (setq pkgbuild-update-sums-on-save nil))

(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . pkgbuild-mode))


(provide 'init-pkgbuild)
;;; init-pkgbuild.el ends here
