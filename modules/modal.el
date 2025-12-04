;;; init-modal.el --- Modal editing setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; FIXME Hmmm...I'm not liking this, I might need a module
;; system...These are required for using expand region and avy
(use-package expand-region
  :straight t
  :commands er/expand-region)
(use-package avy
  :straight t
  :commands avy-goto-word-1)

;; TODO pin to a commit! My most important package!



;; (use-package bray
;;   :straight t
;;   ;; :commands bray-mode ; FIXME what? this breaks my config??
;;   :config
;;   ;; Typical normal/insert states.
;;   (defvar my-bray-state-normal-map (make-keymap))
;;   (defvar my-bray-state-insert-map (make-keymap))
;;   (defvar my-bray-state-visual-map (make-keymap))

;;   (setq bray-state-default 'normal)
;;   (setq bray-state-definitions
;;         (list
;;          (list
;;           :id 'normal
;;           :cursor-type 'box
;;           :lighter "<N>"
;;           :keymaps (list '(t . my-bray-state-normal-map)))

;;          (list
;;           :id 'insert
;;           :cursor-type 'bar
;;           :lighter "<I>"
;;           :keymaps (list '(t . my-bray-state-insert-map))

;;           ;; Optional.
;;           :is-input t))

;;         (list
;;          :id 'visual
;;          :cursor-type 'box
;;          :lighter "<V>"
;;          :keymaps (list '(t . my-bray-state-visual-map)))

;;         ;; Optional, a quick way to mask insertion.
;;         (define-key my-bray-state-normal-map [remap self-insert-command] 'ignore)

;;         (define-key my-bray-state-normal-map (kbd "h") 'backward-char)
;;         (define-key my-bray-state-normal-map (kbd "j") 'next-line)
;;         (define-key my-bray-state-normal-map (kbd "k") 'previous-line)
;;         (define-key my-bray-state-normal-map (kbd "l") 'forward-char)

;;         (general-def my-bray-state-normal-map
;;                                         ;("," ryo-modal-repeat)
;;           "n" 'isearch-forward
;;           "f" (lambda () (interactive) (bray-state-stack-push 'insert))
;;           "d" 'delete-char
;;           "D" 'backward-delete-char-untabify
;;           "j" 'backward-char
;;           "g" 'keyboard-quit
;;           "/" 'undo
;;           "k" "M-b"
;;           "l" "M-f"
;;           "o" 'next-line
;;           "i" 'previous-line
;;           "." 'scroll-up-command
;;           ">" 'scroll-up-line ; can be replaced with ARG and scroll-up?
;;           "," 'scroll-down-command
;;           "<" 'scroll-down-line ; can be replaced with ARG and scroll-down?
;;           "u" 'beginning-of-line
;;           "p" 'end-of-line
;;           ";" 'forward-char
;;           "z" 'comment-line
;;           "q" "M-q" ; M-q is not `fill-paragraph' in prog-mode
;;           "v" 'set-mark-command
;;           "y" 'yank
;;           "Y" 'yank-pop
;;           "s" 'er/expand-region
;;           "g" 'avy-goto-word-1
;;           ;; (";" exchange-point-and-mark) ; NOTE not sure how useful this is in command mode
;;           "a" 'execute-extended-command
;;           "0" "M-0"
;;           "1" "M-1"
;;           "2" "M-2"
;;           "3" "M-3"
;;           "4" "M-4"
;;           "5" "M-5"
;;           "6" "M-6"
;;           "7" "M-7"
;;           "8" "M-8"
;;           "9" "M-9")
;;         ;; (keymap-set ryo-modal-mode-map "x" ctl-x-map) ; TODO I'd love for this to be like meow's keypad mode. A package idea!? Or like boon's c-god key
;;         ;; (keymap-set ryo-modal-mode-map "SPC" (cons "<leader>" mode-specific-map)) ; TODO make the "leader" text show up in the minibuffer using general.

;;         ;; NOTE any keys that simulate another chord are because modes like
;;         ;; org-mode or prog-mode bind other commands to them

;;         (keymap-set my-bray-state-insert-map "<escape>" 'bray-state-stack-pop))

;;   ;; TODO unused?
;;   (defun selected--on ()
;;     "Enable `selected-region-active-mode'."
;;     (bray-state-stack-push 'visual))

;;   ;; TODO unused?
;;   (defun selected--off ()
;;     "Disable bindings in `selected-keymap' temporarily."
;;     (interactive)
;;     (bray-state-stack-pop)))

;; ;; Enable bray for "typical" editing operation.
;; (add-hook
;;  'after-change-major-mode-hook
;;  (lambda ()
;;    (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
;;      (bray-mode)
;;      (selected-minor-mode))))

(use-package selected
  :straight t
  ;; :hook (prog-mode . selected-minor-mode)
  ;; :bind (:map selected-keymap
  ;; ("d" . kill-region)
  ;; ("c" . kill-ring-save)))
  ;; (";" . exchange-point-and-mark)))
  :config
  (general-def selected-keymap
    "d" 'kill-region
    "c" 'kill-ring-save))


(provide 'init-modal)
;;; init-modal.el ends here
