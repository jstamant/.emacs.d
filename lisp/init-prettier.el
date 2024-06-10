;;; init-prettier.el --- Prettier configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Use command `prettier-prettify' to manually prettify a buffer.
;; Prettifying buffers on save can be added in the future.

;; Additional formatters can be added in the future, as well (but then
;; the name of the file should be changed!)

;;; Code:

(use-package prettier
  :ensure t)

(provide 'init-prettier)
;;; init-prettier.el ends here
