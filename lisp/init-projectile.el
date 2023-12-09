;;; init-projectile.el --- Project navigation via Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package projectile
  :straight t
  ;; :after '(ag rg) ; optionally depends on these two packages
  ;;:diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; TODO remove this binding, it's been moved to general
  :init
  (when (file-directory-p "~/programming")
    (setq projectile-project-search-path '("~/programming")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf) ;; Does not apply to 'alien' sort order, which is what I use
  :config
  (projectile-mode 1))

;; Need these to perform ag and ripgrep searches using projectile
(use-package ag
  :straight t)
(use-package rg
  :straight t)


(provide 'init-projectile)
;;; init-projectile.el ends here
