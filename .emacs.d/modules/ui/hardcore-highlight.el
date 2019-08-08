;; hardcore-highlight.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-highlight()

  (use-package hide-mode-line
    :hook
    ((imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

  (use-package
    highlight-parentheses
    :ensure t
    :diminish 'highlight-parentheses-mode
    :config
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

  (use-package rainbow-mode
    :diminish
    :defines helpful-mode-map
    :functions my-rainbow-colorize-match
    :commands (rainbow-x-color-luminance rainbow-colorize-match)
    :hook ((css-mode scss-mode less-css-mode) . rainbow-mode)
    :config
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
        (let* ((match (or match 0))
            (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                    "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defadvice rainbow-turn-off (after clear-overlays activate)
        (remove-overlays (point-min) (point-max) 'ovrainbow t)))

  ;; rainbow delimiters
  (use-package
    rainbow-delimiters
    :ensure t
    :diminish 'rainbow-delimiters-mode
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

  ;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t)
  (setq beacon-color "yellow")
  (setq beacon-size 80)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

  ;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))


(use-package symbol-overlay
  :ensure t
  :defer t
  :diminish symbol-overlay-mode "ⓢ"
  :bind (:map symbol-overlay-mode-map
              ("C-p" . symbol-overlay-jump-prev)
              ("C-n" . symbol-overlay-jump-next))
  :init
  (setq symbol-overlay-idle-time 0.1)
  (hardcore/set-leader-keys "ss" 'symbol-overlay-mode))
  :config
    ;; Disable symbol highlighting while selecting
    (defadvice set-mark (after disable-symbol-overlay activate)
        (symbol-overlay-mode -1))
    (defadvice deactivate-mark (after enable-symbol-overlay activate)
        (symbol-overlay-mode 1))


;; Highlight indentions
(when (display-graphic-p)
  (use-package highlight-indent-guides
    :diminish
    :commands highlight-indent-guides--highlighter-default
    :functions my-indent-guides-for-all-but-first-column
    :hook (prog-mode . highlight-indent-guides-mode)
    :init (setq highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'top)
    :config
    ;; Don't display indentations while editing with `company'
    (with-eval-after-load 'company
      (add-hook 'company-completion-started-hook
                (lambda (&rest _)
                  "Trun off indentation highlighting."
                  (when highlight-indent-guides-mode
                    (highlight-indent-guides-mode -1))))
      (add-hook 'company-after-completion-hook
                (lambda (&rest _)
                  "Trun on indentation highlighting."
                  (when (and (derived-mode-p 'prog-mode)
                             (not highlight-indent-guides-mode))
                    (highlight-indent-guides-mode 1)))))

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
        (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str)))))))))
 ;; Highlight uncommitted changes
(use-package diff-hl
  :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
  :commands diff-hl-magit-post-refresh
  :functions  my-diff-hl-fringe-bmp-function
  :custom-face (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if hardcore-mac #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

)

(provide 'hardcore-highlight)

;;; hardcore-highlight.el ends here
