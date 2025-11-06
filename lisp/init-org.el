;;; init-org.el --- Personal org-mode and org-roam configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org
  :ensure t
  :bind (:map org-mode-map
              ;; Next and previous errors for when using org-occur C-c /
              ("M-n" . next-error)
              ("M-p" . previous-error))
  :config
  (jstamant/init-org-miscellaneous)
  (jstamant/init-org-todo)
  (jstamant/init-org-agenda)
  (jstamant/init-org-capture))


(defun jstamant/init-org-miscellaneous ()
  "Set my miscellaneous/general org-mode settings."
  (setq org-log-done 'time) ; Log closing-time of tasks
  (put 'org-toggle-time-stamp-overlays 'disabled
       "I don't use timestamp overlays.\n
This command is usually invoked as an accident.\n")
  (setq org-refile-targets '((nil . (:level . 1))
                             (nil . (:tag . "project"))))
  (setq org-src-fontify-natively t) ; Fontify source blocks
  (set-face-foreground 'org-block (face-attribute 'default :foreground))
  (setq org-src-tab-acts-natively t) ; Allow proper indentation of code blocks
  (setq org-edit-src-content-indentation 0)

  ;; Aliases for finding your main org file, provided it is an agenda file
  (defalias 'g    'org-cycle-agenda-files)
  (defalias 'gtd  'org-cycle-agenda-files)
  (defalias 'work 'org-cycle-agenda-files)
  (defalias 'w    'org-cycle-agenda-files)

  (defalias 'org-link-open 'org-open-at-point) ; for findability

  ;; Settings for a clean view
  (setq org-adapt-indentation nil) ; Promotes and demotes headings like org used to

  ;; Set org-directory, agenda files, and main org files
  ;;Implement some kind of also do a function for 'at home' or 'at work' boolean
  ;; I should also bind org-cycle-agenda-files (C-' and C-,) globally
  (if (at-work)
      (progn
        (setq org-directory onedrive-directory)
        (add-to-list 'org-agenda-files (concat org-directory "work.org"))))
  (if (at-home)
      (progn
        (setq org-directory drive-directory)
        (add-to-list 'org-agenda-files (concat org-directory "gtd.org"))))
  ;; DISABLED - set agenda files via C-c [ org-agenda-file-to-front
  ;; (setq org-agenda-files '())
  ;; (dolist (file org-files)
  ;;   (if (file-exists-p (concat org-directory file))
  ;;       (add-to-list 'org-agenda-files (concat org-directory file))))

  ;; Disable tag inheritance, because I don't make use of it
  (setq org-use-tag-inheritance nil))


(defun jstamant/init-org-todo ()
  "Set my org-mode todo-related settings."
  ;; Set priorities to numeric priorities
  (setq org-highest-priority ?1)
  (setq org-lowest-priority ?5)
  (setq org-default-priority ?5)
  ;; Set priority faces to be color-coded like in Todoist
  ;; (setq org-priority-faces
  ;;       '((?1 . "orangered")
  ;;         (?2 . "darkorange")
  ;;         (?3 . "cornflowerblue")
  ;;         (?4 . "darkgray")))
  ;; Some general org-mode preferences and global options
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"
                    "|" "DONE(d)" "CANCELLED(c)"))))


(defun jstamant/init-org-agenda ()
  "Set all my org-agenda settings."
  (global-set-key "\C-ca" 'org-agenda)
  (setq org-deadline-warning-days 7)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-stuck-projects
        '("+project" ("TODO" "STARTED") nil ""))
  (setq org-agenda-custom-commands
        '(("d" "Daily report" ; This one is special, it's a "composite agenda"
           ((tags-todo
             "-TODO\"WAITING\""
             ((org-agenda-overriding-header "Prioritized todo list")))
            (todo
             "WAITING"
             ((org-agenda-overriding-header "Waiting list")))))
          ("u" "Unscheduled"
           ((tags-todo "-TODO=\"WAITING\"-SCHEDULED<>\"\"-CATEGORY=\"repetitives\""))
           ((org-agenda-sorting-strategy '(priority-down timestamp-up))
            (org-agenda-prefix-format " %-14:c")
            (org-agenda-overriding-header "Prioritized TODO list - unscheduled items")))
          ("x" "Agenda - upcoming" agenda ""
           ((org-agenda-span 30)))
          ("r" "TODO list (r)eport"
           ((tags-todo "-TODO=\"WAITING\"-CATEGORY=\"repetitives\""))
           ((org-agenda-sorting-strategy '(priority-down timestamp-up))
            (org-agenda-prefix-format " %-14:c")
            (org-agenda-overriding-header "Prioritized TODO list")))
          ("w" "WAITING items" todo "WAITING"
           ((org-agenda-overriding-header "WAITING list")
            (org-agenda-prefix-format " ")
            (org-agenda-sorting-strategy '((tsia-up)))))
          ("i" "Incomplete items" todo "STARTED"
           ((org-agenda-sorting-strategy '((todo tag-up)))))
          ("P" "List of active projects" tags "project"
           ((org-agenda-sorting-strategy '((tags alpha-up))))))))


(defun jstamant/init-org-capture ()
  "Set all my org-capture settings."
  (setq org-capture-templates
        `(("t" "TODO" entry
           (file+headline ,(car org-agenda-files) "Tasks")
           "* TODO %?\n%u")
          ("T" "TODO with clipboard" entry
           (file+headline ,(car org-agenda-files) "Tasks")
           "* TODO %?\n%u\n%c")
          ("c" "Collect" entry
           (file+headline ,(car org-agenda-files) "In")
           "* %^{Brief description}\n%u%?")
          ("J" "Journal entry - with prompts" plain
           (file+olp+datetree "reference/journal.org")
           "TEXT\n\nWhat did I accomplish?\n- \n\nWhat did I learn?\n- \n\nWhat am I grateful for?\n- \n\n")
          ("j" "Journal entry - plain" plain
           (file+olp+datetree "reference/journal.org") ""))))


;; TODO move roam into its own module
;; https://www.orgroam.com/manual.html
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (concat drive-directory "/org-roam"))
  :config
  (org-roam-db-autosync-mode))

(jrs/emacs-leader-keys
  "n" '(:ignore t :which-key "org-roam node")
  "na" 'org-roam-alias-add
  "nc" 'org-roam-node-capture
  "nf" 'org-roam-node-find
  "ni" 'org-roam-node-insert
  "nI" 'org-id-get-create
  "nl" '(org-roam-buffer-toggle :which-key "links")
  "ns" 'org-roam-db-sync)
(general-spc
  "r" '(:ignore t :which-key "roam")
  "rc" 'org-roam-node-capture
  "rf" 'org-roam-node-find
  "ri" 'org-roam-node-insert
  "rl" '("links" . org-roam-buffer-toggle))


(provide 'init-org)
;;; init-org.el ends here
