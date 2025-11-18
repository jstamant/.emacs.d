;;; init-formatter.el --- Formatter setup using apheleia and prettier -*- lexical-binding: t -*-
;;; Commentary:

;; Use command `prettier-prettify' to manually prettify a buffer.
;; Prettifying buffers on save can be added in the future.

;;; Code:


(use-package apheleia
  :straight t)

(use-package prettier
  :straight t)


(provide 'init-formatter)
;;; init-formatter.el ends here
