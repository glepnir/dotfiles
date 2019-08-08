;; hardcore-evil.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-evil-base ()
  (hardcore/init-evil-escape)
  (hardcore/init-evil-iedit)
  (hardcore/init-evil-keybindings)
  (use-package
    evil-surround
    :ensure t
    :config (global-evil-surround-mode 1))

  (use-package evil-nerd-commenter
    :ensure t
    :after evil
    :init
    (hardcore/set-leader-keys "ci" 'evilnc-comment-or-uncomment-lines
                           "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                           "cp" 'evilnc-comment-or-uncomment-paragraphs
                           "cy" 'evilnc-copy-and-comment-operator))
  (use-package mwim
    :ensure t)

  (use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

  (use-package evil-mc
  :ensure t
  :after evil
  :diminish evil-mc-mode "ⓜ"
  :init
  (defun hardcore/toggle-evil-mc ()
    (interactive)
    (if evil-mc-mode
        (progn
          (evil-mc-undo-all-cursors)
          (evil-mc-mode -1)
          (message "evil mc mode disabled"))
      (progn
        (evil-mc-mode 1)
        (message "evil mc mode enabled"))))
  (hardcore/set-leader-keys "tm" #'hardcore/toggle-evil-mc)
  (defun hardcore/reset-evil-mc-key-map ()
    (let ((keys '(("ma" . evil-mc-make-all-cursors)
                  ("mu" . evil-mc-undo-all-cursors)
                  ("ms" . evil-mc-pause-cursors)
                  ("mr" . evil-mc-resume-cursors)
                  ("mf" . evil-mc-make-and-goto-first-cursor)
                  ("mb" . evil-mc-make-and-goto-last-cursor)
                  ("mh" . evil-mc-make-cursor-here)
                  ("mn" . evil-mc-skip-and-goto-next-match)
                  ("mp" . evil-mc-skip-and-goto-prev-match)
                  ("C-n" . evil-mc-make-and-goto-next-match)
                  ("C-p" . evil-mc-make-and-goto-prev-match)
                  )))
      (dolist (key-data keys)
        ;; (evil-define-key 'normal 'evil-mc-key-map (kbd (car key-data)) (cdr key-data))
        (evil-define-key 'visual 'evil-mc-key-map (kbd (car key-data)) (cdr key-data)))))
  :config
  (hardcore/reset-evil-mc-key-map))

  )

(defun hardcore/init-evil-escape ()
  (use-package
    evil-escape
    :ensure t
    :diminish 'evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode)
  ;; esc should escape everything possible
  (require 'evil)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit))

(defun hardcore/init-evil-iedit ()
  (use-package
    evil-iedit-state
    :defer t
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init (progn
	    (setq iedit-current-symbol-default t iedit-only-at-symbol-boundaries t
		  iedit-toggle-key-default nil)
	    (hardcore/set-leader-keys "se" 'evil-iedit-state/iedit-mode))
    :config (define-key evil-iedit-state-map (kbd config-leader-key) hardcore-default-map)))

(defun hardcore/init-evil-keybindings()
  ;; evil insert keybindings
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  ;; evil normal keibindings
    (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
    ;; evil visual state keybinds
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
  )
(provide 'hardcore-evil)

;;; hardcore-evil.el ends here
