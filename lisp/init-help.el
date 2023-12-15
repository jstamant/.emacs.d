;;; init-help.el --- Setup help-mode and related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package help-mode
  :bind-keymap
  ("C-c h" . help-map) ;; That's it! That's how you bind help-map to another key?? See below, figured it out
  :bind (:map help-mode-map
              ;; Some additional navigation bindings
              ;; TODO move these, as they are emacs keybindings, not really meant for evil mode
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("v" . scroll-up-command)
              ("V" . scroll-down-command)
              ("C-d" . scroll-up-command)))
;;(define-key evil-normal-state-map (kbd "SPC h") help-map) ;; Found it! This is how you do maps!


;; Helpful provides us with a help-mode that shows prettier and better organized
;; help content than the default help-mode
;; I don't like its navigation, though. It doesn't reuse the same window
;; (use-package helpful)
;; (use-package helpful
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))


(provide 'init-help)
;;; init-help.el ends here
