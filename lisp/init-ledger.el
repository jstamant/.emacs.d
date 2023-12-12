;;; init-ledger.el --- Ledger settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :config
  (setq ledger-default-date-format ledger-iso-date-format) ; YYYY-MM-DD
  (setq ledger-highlight-xact-under-point nil) ; Screen is less cluttered without xact highlighting
  (setq ledger-mode-should-check-version nil) ; Ignore checking the 'ledger' binary
  (setq ledger-clear-whole-transactions t) ; For reconciliation, clear whole transactions, doesn't work great. It would be nice to have this only set during reconcile-mode
  (add-to-list 'ledger-reports
               '("uncleared" "%(binary) -f %(ledger-file) reg --uncleared"))
  ;; Fix auto-completion for ledger accounts
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t))))

(use-package ledger-report
  :bind (:map ledger-report-mode-map
              ("n"   . next-line)
              ("p"   . previous-line)
              ("TAB" . ledger-report-visit-source)))

(defun ledger ()
  "Shortcut for finding my ledger file, and navigating to the end
of the buffer."
  (interactive)
  (find-file (concat drive-directory "reference/finances/finances.ledger"))
  (end-of-buffer))


(provide 'init-ledger)
;;; init-ledger.el ends here
