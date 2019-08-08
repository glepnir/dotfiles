;; hardcore-lsp.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(defun hardcore/init-lsp ()
  (use-package lsp-mode
  :diminish lsp-mode
  :hook
  (prog-mode . lsp-deferred)
  :custom
  (lsp-prefer-flymake nil)
  :config
   (require 'lsp-clients))
  ;; LSP UI tools
  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-border   "orange")
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    :preface
    (defun hardcore/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :hook
    (lsp-mode . lsp-ui-mode))

  ;; Lsp completion
  (use-package company-lsp
    :ensure t
    :after (lsp-mode company)
    :commands company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
    (company-lsp-async t)
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t))

  (add-hook 'python-mode-hook #'lsp-deferred)
  (hardcore//lsp-keys-for-mode 'python-mode)

  (add-hook 'go-mode-hook #'lsp-deferred)
  (hardcore//lsp-keys-for-mode 'go-mode)

  (add-hook 'js2-mode-hook #'lsp-deferred)
  (hardcore//lsp-keys-for-mode 'js2-mode)

  (add-hook 'rjsx-mode-hook #'lsp-deferred)
  (hardcore//lsp-keys-for-mode 'rjsx-mode)

  (add-hook 'powershell-mode-hook #'lsp-deferred)
  (hardcore//lsp-keys-for-mode 'powershell-mode)


  ;;Debug
  (use-package dap-mode
    :diminish
    :functions dap-hydra/nil
    :bind (:map lsp-mode-map
                ("<f5>" . dap-debug)
                ("M-<f5>" . dap-hydra))
    :config
    (dap-mode 1)
    (dap-ui-mode 1)
    :hook ((after-init . dap-mode)
          (dap-mode . dap-ui-mode)
          (dap-session-created . (lambda (&_rest) (dap-hydra)))
          (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
          (go-mode . (lambda () (require 'dap-go)))
          ((js-mode js2-mode) . (lambda () (require 'dap-chrome))))
    )

 )


(defun hardcore//lsp-keys-for-mode (mode)
  (hardcore//lsp-leader-keys-for-mode mode)
)


(defun hardcore//lsp-leader-keys-for-mode (mode)
  (define-key evil-normal-state-map [remap evil-goto-definition] 'lsp-ui-peek-find-definitions)
  (define-key evil-normal-state-map "gi" 'lsp-ui-peek-find-implementation)
  (define-key evil-normal-state-map "gi" 'lsp-ui-peek-find-implementation)
  (define-key evil-normal-state-map (kbd "C-c n") 'lsp-rename)
  (define-key evil-normal-state-map "gt" 'lsp-find-type-definition)
  (define-key evil-normal-state-map "gr" 'lsp-ui-peek-find-references)
  (define-key evil-normal-state-map (kbd "C-c d") 'hardcore/toggle-lsp-ui-doc)
  (define-key evil-normal-state-map (kbd "C-c m") 'lsp-ui-imenu)
  (define-key evil-normal-state-map (kbd "C-c e") 'lsp-ui-sideline-mode)
  (define-key evil-normal-state-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (hardcore/set-leader-keys-for-major-mode mode
    ;; format
    "lb" #'lsp-format-buffer
    "lr" #'lsp-format-region))

(provide 'hardcore-lsp)

;;; hardcore-lsp.el ends here
