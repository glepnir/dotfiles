;; hardcore-go.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(defun hardcore/init-go()
  ;; refer link: https://emacs-china.org/t/golang/6973
(defun hardcore/go-auto-comment ()
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))
    (cl-mapcan
     (lambda(item)
       (cl-mapcan
        (if (string= (car item) "func")
            'hardcore/go-func-comment
          'hardcore/go-type-comment)
        (cdr item)))
     items)))

(defun hardcore/go-add-comment (func point)
  (save-excursion
    (goto-char point)
    (forward-line -1)
    (when (not (looking-at (concat "// " func)))
      (end-of-line) (newline-and-indent)
      (insert (concat "// " func " ..")))))

(defun hardcore/go-func-comment (f)
  (let ((func (car f)))
    (if (and (string-prefix-p "(" func)
             (string-match "[)] \\(.*\\)[(]\\(.*\\)[)]\\(.*\\)$" func))
        (hardcore/go-add-comment (match-string 1 func) (cdr f))
      (if (string-match "\\(.*\\)[(]\\(.*\\)[)]\\(.*\\)$" func)
          (hardcore/go-add-comment (match-string 1 func) (cdr f))
        (hardcore/go-add-comment (car f) (cdr f))))))

(defun hardcore/go-type-comment (f)
  (hardcore/go-add-comment (car f) (cdr f)))
   (use-package go-mode
    :mode "\\.go\\'"
    :config
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook (lambda () (setq indent-tabs-mode 1)
                              (setq tab-width 4))))
   (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook #'hardcore/revert-buffer-no-confirm)
  ; (hardcore/declare-prefix-for-mode 'go-mode "mi" "imports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (hardcore/set-leader-keys-for-major-mode 'go-mode
                                        "gc" #'hardcore/go-auto-comment
                                        "ga" 'go-coverage)

(defun hardcore/go-test-current-test-verbose()
  "Add -v flag to go test command."
  (interactive)
  (setq go-test-verbose t)
  (funcall 'go-test-current-test)
  (setq go-test-verbose nil))

(use-package gotest
  :ensure t
  :after go-mode
  :config
  (hardcore/set-leader-keys-for-major-mode 'go-mode
                                        "gr" 'go-run
                                        "gb" 'go-test-current-benchmark
                                        "gt" 'go-test-current-test
                                        "gv" 'hardcore/go-test-current-test-verbose
                                        "gf" 'go-test-current-file
                                        "gp" 'go-test-current-project))
)

(provide 'hardcore-go)
;;; hardcore-go.el ends here
