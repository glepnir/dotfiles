;; hardcore-emacs.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-emacs ()

  ;; init whitespace cleanup for prog modes
  (use-package
    whitespace-cleanup-mode
    :ensure t
    :init (add-hook 'prog-mode-hook #'whitespace-cleanup-mode)
    :diminish 'whitespace-cleanup-mode)

  ;; init expand region
  (use-package
    expand-region
    :ensure t
    :config (hardcore/set-leader-keys "v" 'er/expand-region))

  ;; init flyspell
  (use-package
    flyspell
    :ensure t
    :config (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    :diminish 'flyspell-mode)
  (use-package
    flyspell-popup
    :ensure t
    :config (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct))
  (define-key popup-menu-keymap (kbd "C-j") 'popup-next)
  (define-key popup-menu-keymap (kbd "C-k") 'popup-previous)
  (define-key popup-menu-keymap (kbd "C-l") 'popup-select)

  ;; recent files
  (recentf-mode 1)
  (setq-default recent-save-file "~/.emacs.d/recentf")

  (use-package
    restart-emacs
    :ensure t)

  (hardcore/set-leader-keys "qr" 'hardcore/restart-emacs "qq" 'hardcore/prompt-kill-emacs))

(defun hardcore/prompt-kill-emacs ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;###autoload
(defun hardcore/restart-emacs ()
  (interactive)
  (restart-emacs))

;;;###autoload
(defun hardcore/find-file-vsplit (file)
  "find file in vertical split"
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(hardcore//display-in-split (split-side . right)))))

;;;###autoload
(defun hardcore/find-file-split (file)
  "find file in horizontal split"
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(hardcore//display-in-split (split-side . below)))))

;;;###autoload
;; find file functions in split
(defun hardcore//display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))


(provide 'hardcore-emacs)

;;; hardcore-emacs.el ends here
