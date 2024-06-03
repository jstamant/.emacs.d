;;; init-abbrev.el --- Abbrev setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package abbrev
  :diminish abbrev-mode)

(jrs/add-toggle-keys "a" 'abbrev-mode)

;; Enable abbrev-mode everywhere by default
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)


;; Abbrev definitions

;; See `define-abbrev' for abbref definition arguments
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  `(
    ("atm"  "at the moment")
    ("dts"  ,(format-time-string "%Y-%m-%d")) ; YYYY-MM-DD (ISO 8601)
    ("td"   "TODO")
    ("todo" "TODO")
    ))

(write-abbrev-file)
(read-abbrev-file)


(provide 'init-abbrev)
;;; init-abbrev.el ends here
