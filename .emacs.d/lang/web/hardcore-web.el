;; hardcore-web.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-web ()
  (use-package web-mode
    :ensure t
    :defer t
    :config
    (progn
      (hardcore/declare-prefix-for-mode 'web-mode "me" "errors")
      (hardcore/declare-prefix-for-mode 'web-mode "mg" "goto")
      (hardcore/declare-prefix-for-mode 'web-mode "mh" "dom")
      (hardcore/declare-prefix-for-mode 'web-mode "mr" "refactor")
      (hardcore/set-leader-keys-for-major-mode 'web-mode
        "eh" 'web-mode-dom-errors-show
        "gb" 'web-mode-element-beginning
        "gc" 'web-mode-element-child
        "gp" 'web-mode-element-parent
        "gs" 'web-mode-element-sibling-next
        "hp" 'web-mode-dom-xpath
        "rc" 'web-mode-element-clone
        "rd" 'web-mode-element-vanish
        "rk" 'web-mode-element-kill
        "rr" 'web-mode-element-rename
        "rw" 'web-mode-element-wrap
        "z" 'web-mode-fold-or-unfold
        ))
    :mode
    (("\\.phtml\\'"      . web-mode)
     ("\\.tpl\\.php\\'"  . web-mode)
     ("\\.twig\\'"       . web-mode)
     ("\\.html\\'"       . web-mode)
     ("\\.htm\\'"        . web-mode)
     ("\\.[gj]sp\\'"     . web-mode)
     ("\\.as[cp]x?\\'"   . web-mode)
     ("\\.eex\\'"        . web-mode)
     ("\\.erb\\'"        . web-mode)
     ("\\.mustache\\'"   . web-mode)
     ("\\.handlebars\\'" . web-mode)
     ("\\.hbs\\'"        . web-mode)
     ("\\.eco\\'"        . web-mode)
     ("\\.ejs\\'"        . web-mode)
     ("\\.djhtml\\'" . web-mode))))

(provide 'hardcore-web)

;;; hardcore-web.el ends here
