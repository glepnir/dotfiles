;; hardcore-markdown.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(defun hardcore/init-markdown ()
  (use-package markdown-mode
    :ensure t
    :defer t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . gfm-mode))
    :init (setq markdown-command "marked"))
  (add-hook 'gfm-mode-hook 'turn-on-auto-fill)
  (add-hook 'gfm-mode-hook
            (lambda() (set-fill-column 80))))

(provide 'hardcore-markdown)

;;; hardcore-markdown.el ends here
