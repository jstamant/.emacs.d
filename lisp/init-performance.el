;;; init-performance.el --- Performance adjustments -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)


(provide 'init-performance)
;;; init-performance.el ends here
