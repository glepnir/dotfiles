;; hardcore-misc.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-misc()

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish "ⓔ"
  :hook (after-init . global-eldoc-mode))

(use-package hydra :ensure t)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Automatic parenthesis pairing
; (use-package elec-pair
;   :ensure nil
;   :hook (after-init . electric-pair-mode)
;   :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package smartparens
  :diminish "Ⓢ"
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  (sp-with-modes
      '(go-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-size     0.2)
  :config
  (hardcore/set-leader-keys "il" 'imenu-list-smart-toggle)
)

(use-package anzu
  :defer t
  :ensure t
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))
;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "elpa"
                     "bookmarks"
                     "undo-tree-hist"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun hardcore/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun hardcore/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (hardcore/recentf-save-list-silence hardcore/recentf-cleanup-silence)))

;; FIXME:  keep the diff window
(make-variable-buffer-local 'undo-tree-visualizer-diff)
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(locate-user-emacs-file "undo-tree-hist/")))))


  ;; Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :ensure t
  :hook (after-init . avy-setup-default)
  :init
  (hardcore/set-leader-keys
   "jc" 'avy-goto-char-2
   "jw" 'avy-goto-word-or-subword-1
   "jl" 'avy-goto-line
   "jp" 'hardcore/goto-match-parent)
  :config (setq avy-background t))

;;;###autoload
(defun hardcore/goto-match-parent ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))
)


(provide 'hardcore-misc)

;;; hardcore-misc.el ends here
