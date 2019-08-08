;; core-hardcore.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'core-variables)
(require 'core-keybindings)

(defun hardcore/init ()
  (hardcore/init-use-package)
  (hardcore/init-better)
  (hardcore/init-diminish)
  (hardcore/init-bind-map)
  (hardcore/init-evil)
  (hardcore/init-which-key)
  (require 'core-general)
  (hardcore/init-general))

(defun hardcore/init-diminish ()
  (use-package
    diminish
    :ensure t))

(defun hardcore/init-use-package ()
  (setq package-archives'(("melpa" . "http://elpa.emacs-china.org/melpa/")
                          ("gnu" . "http://elpa.emacs-china.org/gnu/")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile
  (require 'use-package))
  (setq use-package-always-ensure t)
  (use-package use-package-ensure-system-package)

)

(defun hardcore/init-bind-map ()
  (use-package
    bind-map)
  (require 'bind-map)
  (bind-map hardcore-default-map
    :prefix-cmd hardcore-cmds
    :evil-keys (config-command-key)
    :override-minor-modes t
    :override-mode-name hardcore-leader-override-mode))

(defun hardcore/init-evil ()
  (use-package evil
    :ensure t
    :hook (after-init . evil-mode)
    :config
    (setq evil-default-state 'normal)
    (setq evil-magic t
            evil-echo-state t
            evil-indent-convert-tabs t
            evil-ex-search-vim-style-regexp t
            evil-ex-substitute-global t
            evil-ex-visual-char-range t  ; column range for ex commands
            evil-insert-skip-empty-lines t
            ;; more vim-like behavior
            evil-symbol-word-search t
            ;; don't activate mark on shift-click
            shift-select-mode nil
            evil-cross-lines t
            evil-move-cursor-back t ;; Move back the cursor one position when exiting insert mode
            evil-esc-delay 0
            evil-mode-line-format 'after)
        ;; evil cursor color
    (setq
            ;; cursor appearance
            evil-default-cursor '+evil-default-cursor
            evil-normal-state-cursor 'box
            evil-emacs-state-cursor  '(box +evil-emacs-cursor)
            evil-insert-state-cursor 'bar
            evil-visual-state-cursor 'hollow))

)

(defun hardcore/init-which-key ()
  (use-package
    which-key
    :ensure t
    :diminish which-key-mode)
  (which-key-mode)
  (setq which-key-idle-delay config-which-key-delay)
  (let ((new-descriptions '(("hardcore//*\\(.+\\)" . "\\1"))))
    (dolist (nd new-descriptions)
      (push (cons (cons nil (concat "\\`" (car nd) "\\'"))
		  (cons nil (cdr nd))) which-key-replacement-alist)))
  (hardcore/init-which-key--default-prefixes))

(defun hardcore/init-which-key--default-prefixes ()

  (which-key-add-key-based-replacements
     config-leader-key '("root" . "HardCoremacs root"))
  ;; Rename the entry for M-0 in the SPC h k Top-level bindings,
  ;; and for 0 in the SPC- Spacemacs root
  (push '(("\\(.*\\)0" . "winum-select-window-0-or-10") .
          ("\\10" . "select window 0 or 10"))
        which-key-replacement-alist)

  ;; Rename the entry for M-1 in the SPC h k Top-level bindings,
  ;; and for 1 in the SPC- Spacemacs root, to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
          ("\\11..9" . "select window 1..9"))
        which-key-replacement-alist)

  ;; Hide the entries for M-[2-9] in the SPC h k Top-level bindings,
  ;; and for [2-9] in the SPC- Spacemacs root
  (push '((nil . "winum-select-window-[2-9]") . t)
        which-key-replacement-alist)

  (setq prefixes '(("a" "apps")
                   ("f" "file operation")
                   ("c" "comment")
                    ("s" "search ")
                    ("t" "themes")
                    ("b" "buffer")
                    ("p" "projectile")
                    ("q" "quit")
                    ("g" "git")
                    ("j" "jump")
                    ("w" "windows")
                    ("e" "errors")))
  (mapc (lambda (x)
	  (apply #'hardcore/declare-prefix x)) prefixes))

(defun hardcore/init-better ()
  (use-package
    better-defaults))


(provide 'core-hardcore)

;;; core-hardcore.el ends here
