;;; init-tree-sitter.el --- Initialize tree-sitter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :config
  ;; Automatically update and install tree-sitter grammars
  (when (not (file-exists-p (concat user-emacs-directory "tree-sitter/BUNDLE-VERSION")))
    (tree-sitter-langs-install-latest-grammar
     'skip-if-installed
     tree-sitter-langs--os)
    (let ((tree-sitter-dir (concat user-emacs-directory "tree-sitter/"))
          (grammar-files (directory-files (concat tree-sitter-langs--dir "bin") nil ".*\\.[a-z]")))
      (mapc (lambda (file)
              (let ((new-file-name (concat "libtree-sitter-" file)))
                (copy-file (concat tree-sitter-langs--dir "bin/" file)
                           (concat tree-sitter-dir new-file-name)
                           'override-files)))
            grammar-files)
      (copy-file (concat tree-sitter-langs--dir "bin/BUNDLE-VERSION")
                 (concat tree-sitter-dir)
                 'override-files)
      (concat "Copied grammar files to " tree-sitter-dir))))


(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
