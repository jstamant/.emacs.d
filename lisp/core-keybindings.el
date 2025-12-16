;;; core-keybindings.el --- Set up keybindings mechanisms -*- lexical-binding: t -*-


;; Remove the annoying suggestions for command abbreviations
(setq extended-command-suggest-shorter nil)

(defvar-keymap toggles-map
  :doc "Keymap for keys that toggle minor-modes and options.")


;; https://github.com/noctuid/general.el
(use-package general :straight t)

(general-auto-unbind-keys)

(general-create-definer jrs/emacs-leader-keys
  :prefix "C-c")
(jrs/emacs-leader-keys
  "" '(:keymap mode-specific-map :which-key "<leader>"))


(general-create-definer general-spc
  :states '(normal motion visual)
  :keymaps 'override
  :prefix "SPC")
(general-spc "t" '(:keymap toggles-map :which-key "toggles"))


(provide 'core-keybindings)
;;; core-keybindings.el ends here
