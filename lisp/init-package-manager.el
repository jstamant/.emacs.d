;;; init-package-manager.el --- Package management setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Install any additional use-package dependencies
(use-package diminish :ensure t)


(provide 'init-package-manager)
;;; init-package-manager.el ends here
