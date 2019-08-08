;; core-load-paths.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defconst hardcore-core-directory (expand-file-name (concat user-emacs-directory "core/"))
  "hardcore core directory.")

(defconst hardcore-modules-directory (expand-file-name (concat user-emacs-directory "modules"))
  "hardcore modules directory.")

(defconst hardcore-lang-directory (expand-file-name (concat user-emacs-directory "lang/"))
  "hardcore lang directory.")

(defconst hardcore-utils-directory (expand-file-name (concat user-emacs-directory "utils/"))
  "hardcore utils directory.")

;; load files which in folder
(defun load-files(dir)
    (let* (
        (default-directory dir)
        (orig-load-path load-path))
    (setq load-path (cons dir nil))
    (normal-top-level-add-subdirs-to-load-path)
    (nconc load-path orig-load-path))
)

(mapc 'load-files `(,hardcore-core-directory,hardcore-modules-directory,hardcore-lang-directory,hardcore-utils-directory))


;;; core-load-paths.el ends here
