;;; init-modal.el --- Modal editing setup -*- lexical-binding: t -*-

;;; Commentary:

;; 2025-11-14 ryo-modal and selected are decent packages, but the
;; lisp-fu is a little lacking, and there's some functionality like
;; :then and :first that I find unnecessary. I'd like to move to a
;; more generic package like bray, lithium, or modaled. But note that
;; these other packages are also a little weak on the lisp-fu.

;; TODO Move to another package like bray, as ryo-modal will likely
;; give me issues when I come to implement multiple cursors.
;; Apparently, ryo will add hashes to some commands and make it
;; difficult to maintain a list of allowed commands.

;; https://github.com/Kungsgeten/ryo-modal
;; https://github.com/Kungsgeten/selected.el

;; NOTE I should probably use boon-core.el from boon, it's a better
;; core for what I'm intending to do. I just need to figure out how to
;; have a keymap activate upon activating a region. I find that boon
;; is poorly documented in its tutorial, and there's too much dwimy
;; commands with the region operators, and they don't have a nice
;; which-key interface. This is probably a good candidate for forking
;; and improving. The repo is also quite dead. There's also an
;; annoying dependancy on swiper, I think, or the keybindings didn't
;; generate properly with (boon-mode). It also remaps some of Emacs'
;; most-used commands, like C-l and C-k, which I'd prefer to maintain.

;; https://github.com/jyp/boon

;; My own version of boon can be a great way to document my choices in
;; building a modal system that adheres to Emacs standards and
;; actually helps for RSI, unlike the common presecription of "bind
;; ctl to caps". It does, however, introduce some very sane
;; functionality to Emacs commands, like "n", which runs
;; `exchange-point-and-mark' if the region is active, or pops a mark
;; off the ring otherwise.

;; I like boon's division of labor between the hands. With Emacs pinky
;; RSI, having the navigation commands on the right helps take the
;; load off the left hand, unless you're using a lot of shifted
;; letters on the right, which requires the use of your left pinky.

;; And for some reason, not all commands in the keymap are explained!?
;; There's actually very little documentation to the reasoning behind
;; boon, other than the little write-up on division of tasks by hand,
;; and the mneumonics second. I actually believe that the mneumonics
;; shouldn't matter, as you'll get very familiar with your main
;; command interface. I think the mneumonics are more important for
;; the leader key and mode-specific-map (but not of ultimate
;; importance).

;; Kakoune uses shift for extending selection. The use of modifiers in
;; different modal systems is very diverse, and everyone's needs are
;; different. Very hard to choose one system because it may be great,
;; but not match your use of modifiers.

;;; Code:


;; FIXME Hmmm...I'm not liking this, I might need a module
;; system...These are required for using expand region and avy
(use-package expand-region
  :straight t
  :commands er/expand-region)
(use-package avy
  :straight t
  :commands avy-goto-word-1)

(use-package ryo-modal
  :straight t
  :commands ryo-modal-mode
  :bind ("M-SPC" . ryo-modal-mode)
  ;; :hook (prog-mode . ryo-modal-mode)
  :init
  (setq ryo-modal-cursor-color "red")
  (setq ryo-modal-default-cursor-color "white") ; TODO this should be according to the theme
  :config
  (ryo-modal-keys
   ;; NOTE any keys that simulate another chord are because modes like
   ;; org-mode or prog-mode bind other commands to them
   ("," ryo-modal-repeat)
   ("n" isearch-forward)
   ("f" ryo-modal-mode)
   ("d" delete-char)
   ("D" backward-delete-char-untabify)
   ("j" backward-char)
   ("g" keyboard-quit)
   ("/" undo)
   ("k" "M-b")
   ("l" "M-f")
   ("o" next-line)
   ("i" previous-line)
   ("." scroll-up-command)
   (">" scroll-up-line) ; can be replaced with ARG and scroll-up?
   ("," scroll-down-command)
   ("<" scroll-down-line) ; can be replaced with ARG and scroll-down?
   ("u" beginning-of-line)
   ("p" end-of-line)
   (";" forward-char)
   ("z" comment-line)
   ("q" "M-q") ; M-q is not `fill-paragraph' in prog-mode
   ("v" set-mark-command)
   ("y" yank)
   ("Y" yank-pop)
   ("s" er/expand-region)
   ("g" avy-goto-word-1)
   ;; (";" exchange-point-and-mark) ; NOTE not sure how useful this is in command mode
   ("a" execute-extended-command))
  (keymap-set ryo-modal-mode-map "x" ctl-x-map) ; TODO I'd love for this to be like meow's keypad mode. A package idea!? Or like boon's c-god key
  (keymap-set ryo-modal-mode-map "SPC" (cons "<leader>" mode-specific-map)) ; TODO make the "leader" text show up in the minibuffer using general.

  (ryo-modal-keys
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")))

(use-package selected
  :straight t
  ;; :hook (prog-mode . selected-minor-mode)
  :bind (:map selected-keymap
              ("d" . kill-region)
              ("c" . kill-ring-save)))
              ;; (";" . exchange-point-and-mark)))


(provide 'init-modal)
;;; init-modal.el ends here
