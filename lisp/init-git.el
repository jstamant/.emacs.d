;;; init-git.el --- Git setup with magit and other utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package magit
  :ensure t
  :defer t
  :init
  (when (eq system-type 'windows-nt)
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe"))
  :config
  (setq magit-diff-refine-hunk 'all))

(jrs/emacs-extended-keys
  "g" 'magit-status)

(jrs/emacs-leader-keys
  "g" '(:ignore t :which-key "git")
  "gg" 'magit
  "gs" 'magit-status)

(general-spc
  "g" 'magit-status)


;; Highlight changes in the fringe, and navigate and/or stage them
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode)
  ;; These enable indicators for dired
  :hook ((dired-mode . diff-hl-margin-local-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Move 'diff-hl-command-map, which is originally on "C-x v"
  (general-swap-key nil 'diff-hl-mode-map "C-x G" "C-x v"))


;; Provides editing modes for special git files
;; https://github.com/magit/git-modes
;; Automatically adds .gitignore, .gitconfig, .gitattributes to `auto-mode-alist'
(use-package git-modes :ensure t)


(provide 'init-git)
;;; init-git.el ends here
