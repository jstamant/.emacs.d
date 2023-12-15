;;; init-abbrev.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TODO add toggle for abbrev-mode

(use-package abbrev
  :diminish abbrev-mode)

;; Enable abbrev-mode everywhere by default
(setq-default abbrev-mode t)

;; Abbrev definitions

;; See `define-abbrev' for abbref definition arguments
(define-abbrev-table 'global-abbrev-table
  `(("td"   "TODO")
    ("todo" "TODO")
    ("dts"  ,(format-time-string "%Y-%m-%d")))) ; YYYY-MM-DD (ISO 8601)

;; Restores abbrevs from the abbrev_defs file
(read-abbrev-file)


(provide 'init-abbrev)
;;; init-abbrev.el ends here
