;; core-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-general ()

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; font
(set-face-attribute 'default nil
            :family config-font-family
            :height config-font-height)

;; always hightlight current line
(global-hl-line-mode t)
;; stop blinking!!
(blink-cursor-mode 0)
;; 复制粘贴
(setq select-enable-primary t)
(setq select-enable-clipboard t)
  ;; 禁止显示警告提示
(setq visible-bell nil)
;; 关闭警告提示音
(setq ring-bell-function 'ignore)
;; close rgb
(setq ns-use-srgb-colorspace nil)
;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
;; 去除全屏时的黑边
(setq frame-resize-pixelwise t)
;; 简化yes-or-no 输入
(defalias 'yes-or-no-p 'y-or-n-p)
;; 关闭备份功能
(setq make-backup-files nil)
;; 关闭自动保存模式
(setq auto-save-list-file-prefix nil)
;; 当使用emacs时触发垃圾回收
(add-hook 'focus-out-hook #'garbage-collect)
;; 不生成 #filename# 临时文件
(setq auto-save-default nil)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; remove tool-bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
;; remove scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
;; remove menu
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
;; disable startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))
;; 关闭像素滚动
(setq mac-mouse-wheel-smooth-scroll nil)
;;删除时移到回收站
(setq delete-by-moving-to-trash t)
;; 设置光标形状
(setq-default cursor-type '(bar . 3))
;; 设置光标颜色
; (add-to-list 'default-frame-alist '(cursor-color . "green"))
;; 禁止光标闪烁
(blink-cursor-mode -1)
;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; parenthesis
(show-paren-mode 1)
;; fix server restart prompt when close emacs
; (setq  kill-emacs-hook (cons 'lsp--global-teardown (delete 'lsp--global-teardown kill-emacs-hook)))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" hardcore-user-name "-"
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; Keep cursor at end of lines. Require line-move-visual is nil.
(setq track-eol t)
(setq line-move-visual nil)

;; 打开文件时不再创建新的frame
(when (boundp 'ns-pop-up-frames)
(setq ns-pop-up-frames nil))
;; 启动时窗口最大化
(add-hook 'after-init-hook 'toggle-frame-maximized)
(when hardcore-mac
   ;; 打开抗锯齿
    (setq mac-allow-anti-aliasing t)
    ;; natural title bar
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-hook 'after-load-theme-hook
                (lambda ()
                (setcdr (assq 'ns-appearance default-frame-alist)
                        (frame-parameter nil 'background-mode)))))
;; Control use of local variables in files you visit.
;; :safe means set the safe variables, and ignore the rest.
(setq enable-local-variables :safe)

)

(provide 'core-general)

;;; core-ui.el ends here
