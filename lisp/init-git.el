;;; init-git.el --- Git setup with magit and other utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))


;; Highlight changes in the fringe, aka "gutter"
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode)
  ;; These enable indicators for dired
  :hook ((dired-mode . diff-hl-margin-local-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


;; Provides editing modes for special git files
;; https://github.com/magit/git-modes
;; Automatically adds .gitignore, .gitconfig, .gitattributes to `auto-mode-alist'
(use-package git-modes :ensure t)


(provide 'init-git)
;;; init-git.el ends here
