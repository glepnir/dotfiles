;; hardcore-osx.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-osx ()
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier  'meta
           ;;mac-command-modifier 'hyper
           mac-right-command-modifier 'control)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-w") 'evil-delete-buffer)
  (global-set-key (kbd "s-v") 'yank)
  ;; OSX Home/End to line start end
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line)
  ;; Use the OS X Emoji font for Emoticons
  (when (and hardcore-mac (fboundp 'set-fontset-font))
    (set-fontset-font "fontset-default"
                        '(#x1F600 . #x1F64F)
                        (font-spec :name "Apple Color Emoji") nil 'prepend))
 (use-package counsel-osx-app
        :ensure t
        :defer t
        :if hardcore-mac
        :init
        (hardcore/set-leader-keys "aa" 'counsel-osx-app))
 (use-package exec-path-from-shell
  :ensure t
  :defer t
  :if hardcore-mac
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH" "GOROOT"))
  (setq exec-path-from-shell-arguments '("-l"))
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))
(use-package reveal-in-osx-finder
  :ensure t
  :defer t
  :if hardcore-mac
  :commands reveal-in-osx-finder
  :init
  (hardcore/set-leader-keys "br" 'reveal-in-osx-finder))

)

(provide 'hardcore-osx)

;;; hardcore-osx.el ends here
