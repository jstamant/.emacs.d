;;; init-tree-sitter.el --- Initialize tree-sitter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defun jrs/tree-sitter-langs-install-grammars ()
  "Custom function for installing the tree-sitter grammars found in
`tree-sitter-langs--dir'. Returns a short message on completion."
  (let ((tree-sitter-dir (concat user-emacs-directory "tree-sitter/"))
        (grammar-files (directory-files (concat tree-sitter-langs--dir "bin") nil ".*\\.[a-z]")))
    (make-directory tree-sitter-dir)
    (mapc (lambda (file)
            (let ((new-file-name (concat "libtree-sitter-" file)))
              (copy-file (concat tree-sitter-langs--dir "bin/" file)
                         (concat tree-sitter-dir new-file-name)
                         'override-files)))
          grammar-files)
    (copy-file (concat tree-sitter-langs--dir "bin/BUNDLE-VERSION")
               (concat tree-sitter-dir)
               'override-files)
    (concat "Copied grammar files to " tree-sitter-dir)))

(use-package tree-sitter-langs
  :ensure t
  :config
  ;; Automatically installs the tree-sitter grammars
  (if (not (file-exists-p (concat user-emacs-directory "tree-sitter/BUNDLE-VERSION")))
    (jrs/tree-sitter-langs-install-grammars)))


(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
