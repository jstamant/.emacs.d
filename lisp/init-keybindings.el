;;; init-keybindings.el --- Set up generic keybindings and definers with general.el -*- lexical-binding: t -*-

;;; Commentary:

;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

;;; Code:


(defvar-keymap toggles-map
  :doc "Keymap for keys that toggle minor-modes and options.")


;; https://github.com/noctuid/general.el
(use-package general :ensure t)

(general-auto-unbind-keys)

(general-create-definer jrs/emacs-leader-keys
  :prefix "C-c")
(jrs/emacs-leader-keys
  "" '(:keymap mode-specific-map :which-key "<leader>")
  "t" '(:keymap toggles-map :which-key "toggles"))

(general-create-definer jrs/emacs-extended-keys
  :prefix "C-x")
(jrs/emacs-extended-keys
  "" '(:keymap ctl-x-map :which-key "extended commands"))


(general-create-definer general-spc
  :states '(normal motion visual)
  :prefix "SPC")
(general-spc "t" '(:keymap toggles-map :which-key "toggles"))


(provide 'init-keybindings)
;;; init-keybindings.el ends here
