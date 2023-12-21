;;; init-spelling.el --- Set up spell-check system with ispell/aspell -*- lexical-binding: t -*-

;;; Commentary:

;; This configuration is not very well implemented. Needs a
;; keybinding, and the aspell dependency needs to be managed, somehow.

;;; Code:


(use-package ispell
  :ensure t
  :config
  (setq ispell-program-name "/usr/bin/aspell")) ; Change default spell checking program from ispell to aspell

(defalias 'fsm 'flyspell-mode)

(jrs/add-toggle-keys "s" '(flyspell-mode :which-key "flyspell"))


(provide 'init-spelling)
;;; init-spelling.el ends here
