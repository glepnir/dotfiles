;; hardcore-langs.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'hardcore-markdown)
(require 'hardcore-web)
(require 'hardcore-javascript)
(require 'hardcore-go)
(require 'hardcore-misclang)

(defun hardcore/init-langs ()
  (hardcore/init-markdown)
  (hardcore/init-web)
  (hardcore/init-javascript)
  (hardcore/init-go)
  (hardcore/init-misclang)
)

(provide 'hardcore-langs)

;;; hardcore-langs.el ends here
