;;; init-windows.el --- Window-specific settings and keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(jrs/emacs-leader-keys
  "w" '(:ignore t :which-key "windows")
  "wd" 'delete-window
  "wh" 'windmove-left
  "wk" 'delete-window
  "wl" 'windmove-right
  "wo" 'other-window
  "ww" 'other-window
  "w0" 'delete-window
  "w1" 'delete-other-windows
  "w2" 'split-window-below
  "w3" 'split-window-right)

(general-spc
  "o" 'other-window
  "w" '(:ignore t :which-key "windows")
  "wd" 'delete-window
  "wh" 'windmove-left
  "wk" 'delete-window
  "wl" 'windmove-right
  "wo" 'other-window
  "ww" 'other-window
  "w0" 'delete-window
  "w1" 'delete-other-windows
  "w2" 'split-window-below
  "w3" 'split-window-right
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right)

(keymap-set global-map "M-o" 'other-window)

(use-package ace-window
  :ensure t)
(keymap-set global-map "M-O" 'ace-window)


(provide 'init-windows)
;;; init-windows.el ends here
