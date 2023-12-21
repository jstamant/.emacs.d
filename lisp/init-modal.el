;;; init-modal.el --- Modal editing setup using meow-core -*- lexical-binding: t -*-

;;; Commentary:

;; https://github.com/meow-edit/meow

;; I'm using meow here, because its integration with Emacs is excellent for
;; how unintrusive it is. Its keypad mode is excellent, as well.

;; As long as you don't bind any chorded keys in normal mode, you'll
;; be able to use emacs bindings everywhere, seemlessly.

;;; Code:



(use-package meow
  :ensure t
  :defer t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-expand-hint-counts nil)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("?" . meow-cheatsheet)))


(use-package general :ensure t)

(general-define-key
 :keymaps 'meow-normal-state-keymap
 "/" 'ignore                            ; 'meow-keypad-describe-key
 "0" 'ignore                            ; 'meow-expand-0
 "9" 'ignore                            ; 'meow-expand-9
 "8" 'ignore                            ; 'meow-expand-8
 "7" 'ignore                            ; 'meow-expand-7
 "6" 'ignore                            ; 'meow-expand-6
 "5" 'ignore                            ; 'meow-expand-5
 "4" 'ignore                            ; 'meow-expand-4
 "3" 'ignore                            ; 'meow-expand-3
 "2" 'ignore                            ; 'meow-expand-2
 "1" 'ignore                            ; 'meow-expand-1
 "-" 'ignore                            ; 'negative-argument
 ";" 'exchange-point-and-mark
 "," 'ignore                            ; 'meow-inner-of-thing
 "." 'ignore                            ; 'meow-bounds-of-thing
 "[" 'ignore                            ; 'meow-beginning-of-thing
 "]" 'ignore                            ; 'meow-end-of-thing
 "a" 'ignore
 "A" 'ignore
 "b" 'backward-word
 "B" 'backward-word
 "c" 'ignore                            ; 'meow-change
 "d" 'delete-char
 "D" 'backward-delete-char
 "e" 'ignore                            ; 'meow-next-word
 "E" 'ignore                            ; 'meow-next-symbol
 "f" 'ignore                            ; 'meow-find
 "g" 'ignore                            ; 'meow-cancel-selection
 "G" 'ignore                            ; 'meow-grab
 "h" 'backward-char
 "H" 'ignore
 "i" 'meow-insert
 "I" 'ignore
 "j" 'next-line
 "J" 'ignore
 "k" 'previous-line
 "K" 'ignore
 "l" 'forward-char
 "L" 'ignore
 "m" 'ignore                            ; 'meow-join
 "n" 'ignore                            ; 'meow-search
 "o" 'ignore                            ; 'meow-open-below
 "O" 'ignore                            ; 'meow-open-above
 "p" 'yank
 "q" 'ignore                            ; 'meow-quit
 "Q" 'ignore                            ; 'meow-goto-line
 "r" 'ignore                            ; 'meow-replace
 "R" 'ignore                            ; 'meow-swap-grab
 "s" 'ignore                            ; 'meow-kill
 "t" 'ignore                            ; 'meow-till
 "u" 'undo
 "U" 'ignore                            ; 'meow-undo-in-selection
 "v" 'ignore                            ; 'meow-visit
 "w" 'forward-word
 "W" 'forward-word
 "x" 'ignore                            ; 'meow-line
 "X" 'ignore                            ; 'meow-goto-line
 "y" 'ignore                            ; 'meow-save
 "Y" 'ignore                            ; 'meow-sync-grab
 "z" 'recenter-top-bottom
 "'" 'repeat
 "<escape>" 'ignore)

;; This adds bindings to the C-c <key> map, which is where meow's
;; keypad mode registers and looks for bindings
;; TODO make my emacs shortcuts work with meow for modal editing
;; (general-create-definer jrs/meow-leader-key
;;   :keymaps 'mode-specific-map)


(provide 'init-modal)
;;; init-modal.el ends here
