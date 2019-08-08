;; hardcore-flycheck.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:


(defun hardcore/init-flycheck ()
  (use-package flycheck
    :defer t
    :diminish flycheck-mode "ⓕ"
    :commands (hydra-flycheck/body)
    :hook (after-init . global-flycheck-mode)
    :init
    (defhydra hydra-flycheck (:color red
                                   :hint nil)
    "
    ^
    ^Flycheck^        ^Errors^          ^Checker^
    ^────────^────────^──────^──────────^───────^───────────
    _q_ quit          _c_ check         _s_ select
    _v_ verify setup  _n_ next          _d_ disable
    _m_ manual        _p_ previous      _?_ describe
                    _l_ list
    ^^                  ^^                  ^^
    "
    ("q" nil exit: t)
    ("c" flycheck-buffer exit: t)
    ("d" flycheck-disable-checker exit: t)
    ("l" flycheck-list-errors exit: t)
    ("m" flycheck-manual exit: t)
    ("n" flycheck-next-error exit: t)
    ("p" flycheck-previous-error exit: t)
    ("s" flycheck-select-checker exit: t)
    ("v" flycheck-verify-setup exit: t)
    ("?" flycheck-describe-checker exit: t)))
    :config
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    ; (setq flycheck-highlighting-mode 'symbols)
    ; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ; (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))


  ;; key bindings
  (hardcore/set-leader-keys
        "el" 'flycheck-list-errors
        "ec" 'flycheck-clear
        "eh" 'flycheck-describe-checker
        "ee" 'flycheck-explain-error-at-point
        "es" 'flycheck-select-checker
        "eS" 'flycheck-set-checker-executable
        "ep" 'flycheck-previous-error
        "en" 'flycheck-next-error
        "ev" 'flycheck-verify-setup
        "eg" 'avy-flycheck-goto-error)
  ;; Define fringe indicator / warning levels
    (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info)

;; Jump to and fix syntax errors via `avy'
(use-package avy-flycheck
  :ensure t
  :after (avy flycheck)
  :init
  (avy-flycheck-setup))

(use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config (add-to-list 'flycheck-posframe-inhibit-functions
                            #'(lambda () (bound-and-true-p company-backend))))

(use-package flycheck-pos-tip
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

)

(provide 'hardcore-flycheck)

;;; hardcore-flycheck.el ends here
