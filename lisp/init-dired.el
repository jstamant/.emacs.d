;;; init-dired.el --- Dired settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package dired
  :init
  (when (eq system-type 'darwin)
    (setq ls-lisp-use-insert-directory-program nil)) ;; Uses ls-lisp to prevent errors on Mac
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("TAB" . dired-find-file)))


(provide 'init-dired)
;;; init-dired.el ends here
