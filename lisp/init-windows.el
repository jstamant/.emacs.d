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

(provide 'init-windows)
;;; init-windows.el ends here
