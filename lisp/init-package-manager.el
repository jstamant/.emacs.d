;;; init-package-manager.el --- Package management setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Install and configure straight
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version) ; TODO is this line required?
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package
(straight-use-package '(use-package :type built-in))
(use-package diminish :straight t)


(provide 'init-package-manager)
;;; init-package-manager.el ends here
