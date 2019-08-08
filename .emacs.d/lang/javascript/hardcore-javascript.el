;; hardcore-javascript.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(defun hardcore/init-javascript ()
  (use-package js2-mode
    :ensure t
    :defer t
    :mode "\\.js\\'"
    :interpreter "node"
    :config
      (hardcore/declare-prefix-for-mode 'js2-mode "mh" "documentation")
      (hardcore/declare-prefix-for-mode 'js2-mode "mg" "goto")
      (hardcore/declare-prefix-for-mode 'js2-mode "mr" "refactor")
      (hardcore/declare-prefix-for-mode 'js2-mode "mz" "folding")
      ;; key bindings
      (hardcore/set-leader-keys-for-major-mode 'js2-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))

  (use-package rjsx-mode
    :ensure t
    :defer t
    :mode "\\.jsx\\'"
    :config
      (hardcore/declare-prefix-for-mode 'rjsx-mode "mh" "documentation")
      (hardcore/declare-prefix-for-mode 'rjsx-mode "mg" "goto")
      (hardcore/declare-prefix-for-mode 'rjsx-mode "mr" "refactor")
      (hardcore/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
      ;; key bindings
      (hardcore/set-leader-keys-for-major-mode 'rjsx-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments)))


(provide 'hardcore-javascript)
;;; hardcore-javascript.el ends here
