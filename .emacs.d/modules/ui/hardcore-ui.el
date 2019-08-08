(defun hardcore/init-ui()

  (use-package all-the-icons
    :demand t)
  (use-package doom-themes
    :ensure t
    :defer t
    :if (eq hardcore-theme-selected 'doom-one)
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
     (load-theme 'doom-one t)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  (use-package doom-themes
    :ensure t
    :if (eq hardcore-theme-selected 'doom-gruvbox)
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
     (load-theme 'doom-gruvbox t)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))


    (use-package monokai-theme
    :ensure t
    :if (eq hardcore-theme-selected 'monokai)
    :init
    (defun load-monokai-theme()
        "Load Monokai theme and customize faces."
        (load-theme 'monokai t)

        ;; FIXME: https://github.com/oneKelvinSmith/monokai-emacs/issues/73
        (with-eval-after-load 'flycheck
        (custom-set-faces
        '(flycheck-error ((t (:underline (:style wave :color "#F92672")))))
        '(flycheck-warning ((t (:underline (:style wave :color "#FD971F")))))
        '(flycheck-info ((t (:underline (:style wave :color "#66D9EF")))))))

        (set-face-background 'tooltip "#FEFBD5")
        (when (boundp 'pos-tip-background-color)
        (setq pos-tip-background-color "#FEFBD5")))

    (add-hook 'after-init-hook 'load-monokai-theme))



   (use-package spacemacs-theme
    :defer t
    :ensure t
    :if (eq hardcore-theme-selected 'spacemacs-dark)
    :init
    (load-theme 'spacemacs-dark t)
    (setq spacemacs-theme-org-agenda-height nil)
    (setq spacemacs-theme-org-height nil))

    (use-package gruvbox-theme
    :ensure t
    :if (eq hardcore-theme-selected 'gruvbox)
    :config
    (load-theme 'gruvbox-dark-hard t))

;; fringe 美化
;; left fringe with 4 pixel ,right fringe width:8 pixel
(if (fboundp 'fringe-mode) (fringe-mode '(4 . 8)))
    ;; 设置visual line fringe bitmap
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b01110000
     #b00010000
     #b00010000
     #b00000000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000
     #b00001000
     #b00001000
     #b00001110
     #b00000000
     #b00000000
     #b00000000
     #b00000000])
(set-fringe-bitmap-face 'right-curly-arrow 'warning)
(set-fringe-bitmap-face 'left-curly-arrow 'warning)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))
(add-hook 'after-init-hook 'turn-on-visual-line-mode)

)


(provide 'hardcore-ui)
