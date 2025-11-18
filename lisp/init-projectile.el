;;; init-projectile.el --- Project navigation via Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package projectile
  :straight t
  :defer t
  :diminish projectile-mode
  :bind-keymap ("C-x p" . projectile-command-map)
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

(general-spc
  "p" '(:keymap projectile-command-map :package projectile :which-key "projectile"))

;; Need these to perform ag and ripgrep searches using projectile
(use-package ag
  :straight t)
(use-package rg
  :straight t)


(provide 'init-projectile)
;;; init-projectile.el ends here
