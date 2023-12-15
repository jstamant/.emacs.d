;;; init-dashboard.el --- Initial dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package dashboard
  :ensure t)

(setq dashboard-startup-banner 'ascii)
(setq dashboard-banner-ascii "\
███████╗███╗   ███╗ █████╗  ██████╗███████╗
██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
█████╗  ██╔████╔██║███████║██║     ███████╗
██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")

(setq dashboard-banner-logo-title "Welcome to Emacs!")
(setq dashboard-set-navigator t)
(setq dashboard-navigator-buttons
      `(((,(nerd-icons-octicon "nf-oct-mark_github")
          "GitHub"
          "Open GitHub profile"
          (lambda (&rest _) (browse-url "https://github.com/jstamant/")))
         (,(nerd-icons-faicon "nf-fa-linkedin_square")
          "LinkedIn"
          "Open LinkedIn profile"
          (lambda (&rest _) (browse-url "homepage"))))
        (("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error)
         ("?" "" "?/h" #'show-help nil "<" ">"))))
(setq dashboard-center-content nil)
(setq dashboard-show-shortcuts t)
;;(setq dashboard-icon-type 'nerd-icons) ; 2023-12-15 currently works better with 'all-the-icons
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons nil)
(setq dashboard-page-separator "

")
(setq dashboard-items
      '((recents  . 5)
        (projects . 5)
        (bookmarks . 5)
        (registers . 5)))

(dashboard-setup-startup-hook)


(provide 'init-dashboard)
;;; init-dashboard.el ends here
