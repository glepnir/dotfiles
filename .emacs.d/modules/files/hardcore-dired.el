;; hardcore-dired.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-dired()
(use-package dired
  :ensure nil
  :defer t
  :config
  ;; Show directory first
  ;;  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (cond
   (hardcore-mac
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))))

  ;; Extra Dired functionality
  (use-package dired-aux
    :defer t
    :ensure nil)
  (use-package dired-x
    :ensure nil
    :defer t
    :diminish dired-omit-mode
    :init (setq dired-omit-mode t)
    :config
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))


(use-package all-the-icons-dired
  :defer t
  :diminish all-the-icons-dired-mode
  :after (dired all-the-icons)
  :hook ((dired-mode . all-the-icons-dired-mode)
	 (ranger-mode . all-the-icons-dired-mode)))

  )

(provide 'hardcore-dired)

;;; hardcore-dired.el ends here
