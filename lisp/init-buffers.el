;;; init-buffers.el --- Buffer-specific settings and keybinds -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO remove this?
;; (jrs/emacs-leader-keys
;;   "b" '(:ignore t :which-key "buffers")
;;   "bb" 'switch-to-buffer
;;   "bl" 'list-buffers
;;   "bd" 'kill-this-buffer ; TODO has to be changed to kill-current-buffer
;;   "bk" 'kill-this-buffer)

(keymap-set ctl-x-map "k" 'kill-current-buffer)
(keymap-set ctl-x-map "K" 'kill-buffer)


(provide 'init-buffers)
;;; init-buffers.el ends here
