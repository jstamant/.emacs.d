;;; init-environment.el --- Environment setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq user-full-name "Justin St-Amant")

(defvar drive-directory
  (cond ((equal system-type 'gnu/linux)
         "~/drive/")
        ((equal system-type 'darwin)
         (cond ((file-directory-p "~/Google Drive/My Drive/")
                ;; This is when only one account is added
                "~/Google Drive/My Drive/")
               ((file-directory-p "~/jstamant24@gmail.com - Google Drive/My Drive/")
                ;; This is when more than one accounts are added
                "~/jstamant24@gmail.com - Google Drive/My Drive/")))
        ((equal system-type 'windows-nt)
         (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
           (concat userprofile "/Google Drive/"))))
  "The absolute path to the Google Drive directory under any operating system.")

(defvar onedrive-directory
  (when (eq system-type 'windows-nt)
      (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
        (concat userprofile "/OneDrive - Manitoba Hydro/")))
  "The absolute path to your work OneDrive directory. Only for Windows.")

(when (eq system-type 'windows-nt)
    (setenv "HOME" (getenv "USERPROFILE")))

(defun at-work ()
  "t if using a work computer."
  (equal "MH" (substring (system-name) 0 2)))
(defun at-home ()
  "t if using a non-work computer."
  (not (at-work)))


(provide 'init-environment)
;;; init-environment.el ends here
