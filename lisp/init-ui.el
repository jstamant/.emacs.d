;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Remove the default GNU startup message
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

;; Remove the message "When done with this frame, type C-x 5 0"
(setq server-client-instructions nil)

;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t) ; Disable the default Emacs startup screen
(setq use-dialog-box nil) ; Disables dialog boxes for mouse-driven actions

;; Sound settings
(setq ring-bell-function 'ignore) ; Turn off audible bell
(setq visible-bell t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Answer `yes-or-no-p' yes/no questions with a single key instead of typing out
(setq use-short-answers nil)


(provide 'init-ui)
;;; init-ui.el ends here
