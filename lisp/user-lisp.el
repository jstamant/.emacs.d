;;; user-lisp.el --- Custom lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun init ()
  "Shortcut for finding your Emacs configuration file.
Finds `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun reload ()
  "Shortcut for reloading your Emacs configuration.
This is great for when you're tinkering on your `user-init-file'"
  (interactive)
  (load user-init-file))


(provide 'user-lisp)
;;; user-lisp.el ends here
