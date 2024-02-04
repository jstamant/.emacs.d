;;; init-mc.el --- Initialize multiple-cursors -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package multiple-cursors
  :ensure t)

;; TODO figure out how to defer the loading of Hydra and defhydras
(use-package hydra
  :ensure t
  :commands 'hydra-mc/body)

(jrs/emacs-extended-keys "m" 'hydra-mc/body)

(with-eval-after-load 'hydra
  (defhydra hydra-mc ()
    "MULTIPLE-CURSORS"
    ("a" mc/mark-all-like-this)
    ("b" mc/edit-beginnings-of-lines)
    ("e" mc/edit-lines)
    ("E" mc/edit-ends-of-lines)
    ("n" mc/mark-next-like-this-word)
    ("p" mc/mark-prev-like-this-word)
    ("r" mc/mark-all-in-region)))


(provide 'init-mc)
;;; init-mc.el ends here
