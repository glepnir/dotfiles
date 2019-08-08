;; hardcore-base.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'hardcore-evil)
(require 'hardcore-buffers)
(require 'hardcore-windows)
(require 'hardcore-ivy)
(require 'hardcore-flycheck)
(require 'hardcore-projectile)
(require 'hardcore-emacs)
(require 'hardcore-vcs)
(require 'hardcore-treemacs)
(require 'hardcore-osx)
(require 'hardcore-mswindows)
(require 'hardcore-misc)
(require 'hardcore-company)
(require 'hardcore-lsp)
(require 'hardcore-org)
(require 'hardcore-dashboard)
(require 'hardcore-dired)
(require 'hardcore-highlight)
(require 'hardcore-eshell)
(require 'hardcore-yasnippet)
(require 'hardcore-ui)
(require 'hardcore-modeline)


(defun hardcore/init-modules ()
  (hardcore/init-ui)
  (hardcore/init-evil-base)
  (hardcore/init-dashboard)
  (hardcore/init-buffers)
  (hardcore/init-windows)
  (hardcore/init-ivy)
  (hardcore/init-flycheck)
  (hardcore/init-lsp)
  (hardcore/init-org)
  (hardcore/init-projectile)
  (hardcore/init-emacs)
  (hardcore/init-vcs)
  (hardcore/init-treemacs)
  (hardcore/init-dired)
  (hardcore/init-company)
  (hardcore/init-yasnippet)
  (hardcore/init-misc)
  (hardcore/init-highlight)
  (hardcore/init-eshell)
  (hardcore/init-modeline)
  (when (eq system-type 'darwin)
    (hardcore/init-osx))
  (when (eq system-type 'windows-nt)
    (hardcore/init-mswindows)))

(provide 'hardcore-modules)

;;; hardcore-base.el ends here
