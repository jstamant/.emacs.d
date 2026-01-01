;;; user-keybindings.el -*- lexical-binding: t -*-
;;; Commentary:

;; This is where I set the majority of my global keybindings.

;;; Code:


;; C-x additions
(keymap-set ctl-x-map "c" 'calc)
(keymap-set ctl-x-map "v" 'view-mode)

;; My leader key C-c
(keymap-set mode-specific-map "e" (cons "eglot" eglot-command-map))
(keymap-set mode-specific-map "m" (cons "multiple-cursors" mc-map))

;; Help
(keymap-set help-map "M" 'man)

;; Toggles
(keymap-set mode-specific-map "t" (cons "toggles" toggles-map))
(keymap-set toggles-map "P" 'electric-pair-mode)


(provide 'user-keybindings)
;;; user-keybindings.el ends here
