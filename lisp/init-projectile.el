;;; init-projectile.el --- Project navigation via Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (let ((directory (cond ((eq system-type 'gnu/linux)
                          "~/programming")
                         ((eq system-type 'darwin)
                          "~/Developer"))))
    (when (file-directory-p directory)
      (setq projectile-project-search-path `(,directory))))
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-sort-order 'recentf) ;; Does not apply to 'alien' sort order, which is what I use
  :config
  (projectile-mode 1))

(jrs/emacs-extended-keys
  "p" '(:keymap projectile-command-map :package projectile :which-key "projectile"))

;; Need these to perform ag and ripgrep searches using projectile
(use-package ag
  :ensure t)
(use-package rg
  :ensure t)


(provide 'init-projectile)
;;; init-projectile.el ends here
