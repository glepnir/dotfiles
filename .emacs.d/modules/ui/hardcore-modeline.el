(defun hardcore/init-modeline()
  ;;;###autload
(defun hardcore/maybe-alltheicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-alltheicon args)))

;;;###autload
(defun hardcore/maybe-faicon-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

;;;###autload
(defun hardcore/maybe-file-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-icon-for-file args)))

;;;###autoload
(defun shorten-directory (dir max-length)
  "Setup a directory(`DIR') `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(use-package spaceline
  :ensure t
  :if (eq hardcore-modeline-selected 'spaceline)
  :config
  (require 'spaceline-config)
  ;; let spaceline handle auzu info in modeline
  (setq anzu-cons-mode-line-p nil)
  (setq powerline-default-separator 'rounded)
  ;; get the nice-looking unicode numbers of window-numbering-mode
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t)
  ;; To get the mode-line highlight to change color depending on the evil state
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; turn off buffer size info segment
  (spaceline-toggle-buffer-size-on)
  (spaceline-toggle-remote-host-on)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-input-method-on)
  (spaceline-toggle-buffer-encoding-abbrev-on)
  ;; configure the separator between the minor modes
  (setq spaceline-minor-modes-separator "")
  ;; define version control segment
  (spaceline-define-segment version-control
    "Version control information."
    (when vc-mode
      (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
        (powerline-raw
         (concat
          (hardcore/maybe-alltheicon "git" :face 'warning :v-adjust -0.02)
          " "
          branch)))))

  ;; define buffer id segment
  (spaceline-define-segment buffer-id
    "Shorten buufer fileanme."
    (when (buffer-file-name)
      (concat
       (hardcore/maybe-file-icon  (file-name-nondirectory (buffer-file-name)) :face 'all-the-icons-purple :height 0.9 :v-adjust -0.1)
       " "
       ; (shorten-directory default-directory 5)
       (file-relative-name buffer-file-name))))

  (use-package nyan-mode
    :ensure t
    :if (eq hardcore-modeline-selected 'spaceline)
    :config
    (setq nyan-animate-nyancat nil)
    (nyan-mode t)
    (spaceline-toggle-nyan-cat-on))
  ;; hide the current position in the buffer as a percentage
  (spaceline-toggle-buffer-position-off)
  ;; shows the currently visible part of the buffer.
  (spaceline-toggle-hud-on)
  (setq powerline-height 23)
  (spaceline-spacemacs-theme))

(hardcore//doom-modeline)
  )

(defun hardcore//doom-modeline()

(use-package doom-modeline
      :ensure nil
      :if (eq hardcore-modeline-selected 'doom-modeline)
      :hook (after-init . doom-modeline-mode))


(use-package nyan-mode
   :ensure nil
   :if (eq hardcore-modeline-selected 'doom-modeline)
   :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))


  )

(provide 'hardcore-modeline)
