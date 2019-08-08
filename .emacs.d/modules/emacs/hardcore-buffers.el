;; hardcore-buffers.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-buffers ()
  (hardcore/init-buffer-keybindings))

(defun hardcore/init-buffer-keybindings ()
  (hardcore/set-leader-keys "TAB"   'hardcore/alternate-buffer
                             "bb" 'ivy-switch-buffer
                            "bd"   'hardcore/kill-this-buffer
                            "be"   'hardcore/safe-erase-buffer
                            "b C-d" 'hardcore/kill-matching-buffers-rudely
                            "bn"    'next-buffer
                            "bm"    'hardcore/kill-other-buffers
                            "bN"    'hardcore/new-empty-buffer
                            "bP"    'hardcore/copy-clipboard-to-whole-buffer
                            "bp"    'previous-buffer
                            "bY"    'hardcore/copy-whole-buffer-to-clipboard
                            "bs"   'save-buffer
                            "bg"   'hardcore/revert-buffer-no-confirm
                            "bD"   'hardcore/delete-current-buffer-file
                            "bR"   'revert-buffer))
;;;###autoload
(defun hardcore/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))
;;;###autoload
(defun hardcore/alternate-buffer
    (&optional
     window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer (cl-find-if (lambda (buffer)
				    (not (eq buffer current-buffer)))
				  (mapcar #'car (window-prev-buffers window))))))

;;;###autoload
(defun hardcore/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;;;###autoload
(defun hardcore/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;;;###autoload
(defun hardcore/kill-this-buffer
    (&optional
     arg)
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal
	 '(4)
	 arg)
	(kill-buffer-and-window)
      (kill-buffer))))
;;;###autoload
(defun hardcore/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;;;###autoload
(defun hardcore/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min)
			    (point-max)))

;;;###autoload
(defun hardcore/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun hardcore/delete-current-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename
		  (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
	(delete-file filename t)
	(kill-buffer buffer)
	(call-interactively #'projectile-invalidate-cache)
	(message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun hardcore/kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun hardcore/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(provide 'hardcore-buffers)

;;; hardcore-buffers.el ends here
