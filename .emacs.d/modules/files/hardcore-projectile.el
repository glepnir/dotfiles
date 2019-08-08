;; hardcore-projectile.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-projectile ()
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-cache-file (concat hardcore-cache-directory "projectile.cache")
        projectile-known-projects-file (concat hardcore-cache-directory "projectile-bookmarks.eld"))

  :config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))
    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  )

(provide 'hardcore-projectile)

;;; hardcore-projectile.el ends here
