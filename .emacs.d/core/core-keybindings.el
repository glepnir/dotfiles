;; core-keybindings.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar hardcore-default-map (make-sparse-keymap)
  "Base keymap for all hardcore leader key commands.")

(defvar hardcore/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defun hardcore/declare-prefix (prefix name &optional long-name)
  (let* ((command name)
	 (full-prefix (concat config-leader-key " " prefix)))
    ;; define the prefix command only if it does not already exist
    (unless long-name
      (setq long-name name))
    (which-key-declare-prefixes full-prefix (cons name long-name))))

(put 'hardcore/declare-prefix 'lisp-indent-function 'defun)

(defun hardcore/declare-prefix-for-mode (mode prefix name &optional long-name)
  (let  ((command (intern (concat (symbol-name mode) name)))
	 (full-prefix (concat config-leader-key " " prefix))
	 (is-major-mode-prefix (string-prefix-p "m" prefix))
	 (major-mode-prefix (concat config-major-mode-leader-key " " (substring prefix 1))))
    (unless long-name
      (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode full-prefix prefix-name)
      (when (and is-major-mode-prefix
		 config-major-mode-leader-key)
	(which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name)))))

(put 'hardcore/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun hardcore/set-leader-keys (key def &rest bindings)
  (while key (define-key hardcore-default-map (kbd key) def)
	 (setq key (pop bindings) def (pop bindings))))

(put 'hardcore/set-leader-keys 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key 'hardcore/set-leader-keys)

(defun hardcore//init-leader-mode-map (mode map &optional minor)
  (let* ((prefix (intern (format "%s-prefix" map)))
	 (leader1 ",")
	 (leader2 "SPC m")
	 (leaders (delq nil (list leader1 leader2))))
    (or (boundp prefix)
	(progn (eval `(bind-map ,map
			:prefix-cmd ,prefix ,(if minor

						 :minor-modes
					       :major-modes)
			(,mode)
			:evil-keys ,leaders
			:evil-states (normal motion visual evilified)))
	       (boundp prefix)))))

(defun hardcore/set-leader-keys-for-major-mode (mode key def &rest bindings)
  (let* ((map (intern (format "hardcore-%s-map" mode))))
    (when (hardcore//init-leader-mode-map mode map)
      (while key (define-key (symbol-value map)
		   (kbd key) def)
	     (setq key (pop bindings) def (pop bindings))))))

(put 'hardcore/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key-for-mode 'hardcore/set-leader-keys-for-major-mode)

(provide 'core-keybindings)

;;; core-keybindings.el ends here
