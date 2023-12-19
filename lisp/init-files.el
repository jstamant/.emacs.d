;;; init-files.el --- File-specific actions and bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(jrs/emacs-leader-keys
  "f" '(:ignore t :which-key "files")
  "fs" 'save-buffer
  "ff" 'find-file
  "fv" 'find-alternate-file
  "fe" '(:ignore t :which-key "emacs-related files")
  "fed" '((lambda () (interactive) (find-file user-init-file)) :which-key "dotfile")
  "fee" '((lambda () (interactive) (find-file (expand-file-name "jrs-evil.el" user-lisp-directory))) :which-key "evil")
  "fek" '((lambda () (interactive) (find-file (expand-file-name "jrs-keybinds.el" user-lisp-directory))) :which-key "keybinds")
  "fes" 'scratch-buffer
  "fr" 'recentf-open-files
  "fd" 'dired
  "fj" 'dired-jump)


(provide 'init-files)
;;; init-files.el ends here
