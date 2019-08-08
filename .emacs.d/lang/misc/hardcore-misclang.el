;; hardcore-misclang.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-misclang ()

  (use-package yaml-mode
    :ensure t
    :defer t
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

  (use-package json-mode
    :ensure t
    :defer t)
  )

(provide 'hardcore-misclang)

;;; hardcore-misclang.el ends here
