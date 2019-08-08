;; hardcore-windows.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:


(defun hardcore/init-windows ()

  (define-key evil-window-map "F" 'make-frame)
  ;; init keys
  (hardcore/set-leader-keys
    "w/" 'hardcore/split-window-right-and-focus
    "w-" 'hardcore/split-window-below-and-focus
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'delete-window
    "wr" 'resize-window
    "wD" 'delete-other-windows
    "w TAB" 'hardcore/alternate-window
    "wF" 'make-frame
    "0" 'winum-select-window-0-or-10
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
;; resize window
(use-package resize-window
  :defer t
  :ensure t)

;; Numbered window shortcuts
  (use-package winum
    :ensure t
    :defer t
    :init
    (setq window-numbering-scope            'global
            winum-reverse-frame-list          nil
            winum-auto-assign-0-to-minibuffer t
            winum-format                      " %s "
            winum-auto-setup-mode-line        nil
            winum-mode-line-position          0
            winum-ignored-buffers             '(" *which-key*"))
     (winum-mode))


;;;###autoload
(defun hardcore/split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (hardcore/split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (hardcore/split-window-func-with-other-buffer 'split-window-horizontally))

;;;###autoload
(defun  hardcore/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'hardcore/toggle-delete-other-windows)

;;;###autoload
(defun hardcore/alternate-window ()
  (interactive)
  (let ( ;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window
      (user-error
       "Last window not found."))
    (select-window prev-window)))
;;;###autoload
(defun hardcore/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))
;;;###autoload
(defun hardcore/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

;;;###autoload
(defun hardcore/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'hardcore/disable-scroll-bars)

)
(provide 'hardcore-windows)

;;; hardcore-windows.el ends here
