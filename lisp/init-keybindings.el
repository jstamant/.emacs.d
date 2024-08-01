;;; init-keybindings.el --- Set up generic keybindings and definers with general.el -*- lexical-binding: t -*-

;;; Commentary:

;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

;;; Code:

;; https://github.com/noctuid/general.el
(use-package general :ensure t)


(general-create-definer jrs/emacs-leader-keys
  :prefix "C-c")
(jrs/emacs-leader-keys
  "" '(:keymap mode-specific-map :which-key "<leader>"))


(general-create-definer jrs/add-toggle-keys
  :prefix "C-c t")
(jrs/add-toggle-keys
 "" '(:ignore t :which-key "toggles"))


(general-create-definer jrs/emacs-extended-keys
  :prefix "C-x")
(jrs/emacs-extended-keys
  "" '(:keymap ctl-x-map :which-key "extended commands"))


(general-create-definer general-spc
  :states 'normal
  :prefix "SPC")


(provide 'init-keybindings)
;;; init-keybindings.el ends here
