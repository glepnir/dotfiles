;; hardcore-dashboard.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-dashboard()
 ;; dashboard
  (use-package dashboard
    :ensure t
    :diminish (dashboard-mode page-break-lines-mode)
    :config
    (defvar homepage-url "https://github.com/hardcoreplayers/hardcoremacs")
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "HardCorEmacs"
          dashboard-center-content t
          dashboard-startup-banner (or hardcore-logo 'official)
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (projects . 5))

          dashboard-set-init-info t
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "file-directory")
                                    (registers . "database"))
          dashboard-set-navigator t
          dashboard-set-footer t
          dashboard-footer (format "Powered by HardCorePlayers, %s" (format-time-string "%Y"))
          dashboard-navigator-buttons
          `(((,(when (display-graphic-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url browse-homepage)))
             (,(when (display-graphic-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-session))))))
      (defun browse-homepage ()
        "Browse the github page of Emacs."
        (interactive)
        (browse-url homepage-url))
      (defun restore-session ()
        "Restore last session."
        (interactive)
        (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
            (error
            (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
            (persp-switch-to-buffer persp-special-last-buffer))))
      (defun dashboard-goto-recent-files ()
        "Go to recent files."
        (interactive)
        (funcall (local-key-binding "r")))

        (defun dashboard-goto-projects ()
            "Go to projects."
            (interactive)
            (funcall (local-key-binding "p")))

    (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      "g" 'dashboard-refresh-buffer
      "}" 'dashboard-next-section
      "{" 'dashboard-previous-section
      "p" 'dashboard-goto-projects
      "r" 'dashboard-goto-recent-files
      "H" 'browse-homepage
      "R" 'restore-session)))
)

(provide 'hardcore-dashboard)


;;; hardcore-dashboard.el ends here
