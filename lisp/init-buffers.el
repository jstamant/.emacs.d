;;; init-buffers.el --- Buffer-specific settings and keybinds -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Do not confirm when killing a buffer with running process, just do it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

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
