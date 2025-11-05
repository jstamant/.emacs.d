;;; init-dired.el --- Dired settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil)
  (keymap-set dired-mode-map "C-s" 'dired-isearch-filenames)
  (keymap-set dired-mode-map "TAB" 'dired-find-file))

(setq system-move-file-to-trash t) ; Send deleted files to trash instead of destroying

;; Uses ls-lisp to prevent errors on Mac
(when (eq system-type 'darwin)
  (setq ls-lisp-use-insert-directory-program nil))


(provide 'init-dired)
;;; init-dired.el ends here
