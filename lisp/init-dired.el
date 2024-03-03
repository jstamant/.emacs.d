;;; init-dired.el --- Dired settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("TAB" . dired-find-file)))


(provide 'init-dired)
;;; init-dired.el ends here
