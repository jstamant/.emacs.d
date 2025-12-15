;;; user-keybindings.el -*- lexical-binding: t -*-
;;; Commentary:

;; This is where I set the majority of my global keybindings.

;;; Code:


;; C-x additions
(keymap-set ctl-x-map "c" 'calc)
(keymap-set ctl-x-map "v" 'view-mode)

;; My leader key C-c
(keymap-set mode-specific-map "m" '("multiple-cursors" . mc-transient))

;; Help
(keymap-set help-map "M" 'man)

;; Toggles
(keymap-set toggles-map "P" 'electric-pair-mode)


(provide 'user-keybindings)
;;; user-keybindings.el ends here
