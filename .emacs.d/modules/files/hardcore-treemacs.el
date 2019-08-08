;; hardcore-treemacs.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-treemacs ()

    (use-package treemacs
    :ensure t
    :defer 1
    :init
    (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :commands (treemacs-select-window treemacs--window-number-ten
               treemacs-current-visibility)
    :init
    (hardcore/set-leader-keys
        "ft"    'treemacs
        "fB"    'treemacs-bookmark
        "fT"    'treemacs-find-file
        "f M-t" 'treemacs-find-tag
        "pt"    'hardcore/treemacs-project-toggle)
    (which-key-add-major-mode-key-based-replacements 'treemacs-mode
        "c"         "treemacs-create"
        "o"         "treemacs-visit-node"
        "oa"        "treemacs-visit-node-ace"
        "t"         "treemacs-toggles"
        "y"         "treemacs-copy"
        "C-c C-p"   "treemacs-projects"
        "C-c C-p c" "treemacs-projects-collapse")

    :config
    (progn
        (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-display-in-side-window        t
            treemacs-eldoc-display                 t
            treemacs-file-event-delay              5000
            treemacs-file-follow-delay             0.2
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         nil
            treemacs-max-git-entries               5000
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-position                      'left
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-desc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-width                         35)

        ;; The default width and height of the icons is 22 pixels. If you are
        ;; using a Hi-DPI display, uncomment this to double the icon size.
        ;;(treemacs-resize-icons 44)

        (treemacs-follow-mode t)
        (treemacs-filewatch-mode t)
        (treemacs-fringe-indicator-mode t)
        (pcase (cons (not (null (executable-find "git")))
                    (not (null (treemacs--find-python3))))
            (`(t . t)
            (treemacs-git-mode 'deferred))
            (`(t . _)
            (treemacs-git-mode 'simple))))

    (use-package treemacs-evil
        :after treemacs evil
        :ensure t)

    (use-package treemacs-projectile
        :after treemacs projectile
        :ensure t)

    (use-package treemacs-icons-dired
        :after treemacs dired
        :ensure t
        :config (treemacs-icons-dired-mode))

    (use-package treemacs-magit
        :after treemacs magit
        :ensure t)
    )

    )

(defun hardcore/treemacs-project-toggle ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-project-root))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))

(provide 'hardcore-treemacs)


;;; hardcore-treemacs.el ends here
