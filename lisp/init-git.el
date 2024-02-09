;;; init-git.el --- Git setup with magit and other utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TODO make magit window always open on the right
(use-package magit
  :ensure t
  :defer t
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

(jrs/emacs-extended-keys
  "g" 'magit-status)

(jrs/emacs-leader-keys
  "g" '(:ignore t :which-key "git")
  "gg" 'magit
  "gs" 'magit-status)

;; Highlight changes in the fringe, and navigate and/or stage them
;; TODO change diff-hl-command-map to a hydra - I don't like the original interface
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


(defun jrs/magit-commit-reshelve ()
  "Change the last commit's date using the date-picker.

Functionally, this calls `org-read-date' followed by
`magit-commit-reshelve' to modify the date.

This may better be achieved by advising magit-commit-reshelve in
the future."
  (interactive)
  (let ((date (org-read-date 'with-time nil nil "When would you like to set this commit?  " nil (format-time-string "%H:%M" (current-time)))))
    (magit-commit-reshelve date 'update-author)))


(provide 'init-git)
;;; init-git.el ends here
