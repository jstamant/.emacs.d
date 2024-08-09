;;; init-buffers.el --- Buffer-specific settings and keybinds -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(jrs/emacs-leader-keys
  "b" '(:ignore t :which-key "buffers")
  "bb" 'switch-to-buffer
  "bl" 'list-buffers
  "bd" 'kill-this-buffer
  "bk" 'kill-this-buffer)

(keymap-set ctl-x-map "k" 'kill-this-buffer)

(general-spc
  "b" 'switch-to-buffer
  "k" 'kill-this-buffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
