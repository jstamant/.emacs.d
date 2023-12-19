;;; init-help.el --- Setup help-mode and related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package help-mode
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

(jrs/emacs-leader-keys
  "h" '(:ignore t :which-key "help")
  "hd" '(:ignore t :which-key "describe")
  "hdm" 'describe-mode)

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

(use-package eldoc
  :diminish eldoc-mode)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))


(provide 'init-help)
;;; init-help.el ends here
