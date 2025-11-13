;;; init-help.el --- Setup help-mode and related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package help-mode
  :bind (:map help-mode-map
              ;; Some additional navigation bindings
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("v" . scroll-up-command)
              ("V" . scroll-down-command)
              ("C-d" . scroll-up-command)))

(jrs/emacs-leader-keys "h" '(:keymap help-map :package help :which-key "help"))
(general-spc "h" '(:keymap help-map :package help :which-key "help"))


;; Beautiful help buffers
;; https://github.com/Wilfred/helpful
(use-package helpful
  :ensure t
  ;; Seems to have autoloads on its commands
  :defer t
  :init
  (keymap-set help-map "f" 'helpful-callable)
  (keymap-set help-map "h" 'helpful-at-point)
  (keymap-set help-map "k" 'helpful-key)
  (keymap-set help-map "v" 'helpful-variable))


(use-package eldoc
  :diminish eldoc-mode)


;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


(provide 'init-help)
;;; init-help.el ends here
