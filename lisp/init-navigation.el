;;; init-navigation.el --- Navigation setup using avy, hydras, and other keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Keep point position on screen when scrolling
(setq scroll-preserve-screen-position t)

(keymap-set evil-motion-state-map "g h" 'evil-beginning-of-line)
(keymap-set evil-motion-state-map "g l" 'evil-end-of-line)
(keymap-set evil-motion-state-map "g m" 'evil-first-non-blank)
(keymap-set evil-motion-state-map "g %" 'evil-percentage-of-line)

(use-package hydra
  :ensure t
  :commands 'hydra-navigation/body)

(keymap-set ctl-x-map "M" 'hydra-navigation/body)
(general-spc "n" 'hydra-navigation/body)

(with-eval-after-load 'hydra
  (defhydra hydra-navigation ()
    "NAVIGATION MODE"
    ("d" scroll-up-command)
    ("g" nil)
    ("h" backward-char)
    ("j" next-line)
    ("k" previous-line)
    ("l" forward-char)
    ("r" move-to-window-line-top-bottom)
    ("u" scroll-down-command)
    ;; For some reason, the recentering commands don't work
    ("z" recenter-top-bottom)))

(defun jrs/scroll-down-half (&optional ARG)
  "Scroll view down by half the window height.\n
Like `scroll-up-command', but modifies `next-screen-context-lines'
to scroll a half-window."
  (interactive)
  (let ((next-screen-context-lines (/ (window-height) 2)))
    (scroll-up-command ARG)))

(defun jrs/scroll-up-half ()
  "Scroll view up by half the window height.\n
See `jrs/scroll-down-half'."
  (interactive)
  (jrs/scroll-down-half '-))

(use-package avy
  :ensure t
  :bind (("C-\\" . avy-goto-char-timer)
         ("M-\\" . avy-goto-line))
  :init
  (general-def 'motion "gsj" 'avy-goto-line-below)
  (general-def 'motion "gsk" 'avy-goto-line-above)
  (general-def 'motion "gsl" 'avy-goto-line)
  (general-def 'motion "gss" 'avy-goto-char-2)
  (general-def 'motion "gs/" 'avy-goto-char-timer))


(provide 'init-navigation)
;;; init-navigation.el ends here
