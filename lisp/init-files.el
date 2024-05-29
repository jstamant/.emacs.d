;;; init-files.el --- File-specific actions and bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(recentf-mode 1)
(setq recentf-max-saved-items 50)

(defun sudo (filename)
  "Calls `find-file' with sudo privileges."
  (interactive "GSudo find-file: ")
  (find-file (concat "/sudo::" (expand-file-name filename))))

(defun sudo-edit ()
  "Opens the currently visited file with sudo privileges."
  (interactive)
  (find-alternate-file (concat "/sudo::" (expand-file-name buffer-file-name))))

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
  "fr" 'consult-recent-file
  "fd" 'dired
  "fj" 'dired-jump)


(provide 'init-files)
;;; init-files.el ends here
