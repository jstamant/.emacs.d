;;; feature-multiple-cursors.el --- Initialize multiple-cursors -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package multiple-cursors
  :straight t
  :defer t)

(defvar-keymap mc-map
  :doc "Keymap for accessing commands related to multiple-cursors."
  ;; Mark
  "n" '("next like this" . mc/mark-next-like-this)
  "p" '("previous like this" . mc/mark-previous-like-this)
  "a" '("all like this" . mc/mark-all-like-this)
  "w" '("all words" . mc/mark-all-words-like-this)
  "s" '("all symbols" . mc/mark-all-symbols-like-this)
  "R" '("all in region" . mc/mark-all-in-region)
  ;; Skip
  "N" '("skip next" . mc/skip-to-next-like-this)
  "P" '("skip previous" . mc/skip-to-previous-like-this)
  "u" '("unmark next" . mc/unmark-next-like-this)
  "U" '("unmark previous" . mc/unmark-previous-like-this)
  ;; Edit
  "i" '("insert numbers" . mc/insert-numbers)
  "e" '("edit lines" . mc/edit-lines)
  "E" '("edit ends of lines" . mc/edit-ends-of-lines)
  "b" '("edit beginnings of lines" . mc/edit-beginnings-of-lines)
  "r" '("reverse regions" . mc/reverse-regions)
  "q" '("quit" . mc/keyboard-quit))


(provide 'feature-multiple-cursors)
;;; feature-multiple-cursors.el ends here
