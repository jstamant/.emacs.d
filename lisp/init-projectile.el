;;; init-projectile.el --- Project navigation via Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (when (file-directory-p "~/programming")
    (setq projectile-project-search-path '("~/programming")))
  (setq projectile-switch-project-action #'projectile-dired)
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
