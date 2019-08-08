;; hardcore-mswindows.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:


(defun hardcore/init-mswindows ()
  (global-hl-line-mode -1)
  (setq inhibit-compacting-font-caches t))

(provide 'hardcore-mswindows)

;;; hardcore-mswindows.el ends here
