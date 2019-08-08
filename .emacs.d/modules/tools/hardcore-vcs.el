;; hardcore-git.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-vcs()
  ;;;###autload
(defun git-get-current-file-relative-path ()
  "Get current file relative path."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;;;###autload
(defun hardcore/git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (hardcore/revert-buffer-no-confirm)
      (message "DONE! git checkout %s" filename))))

;;;###autload
(defun hardcore/git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))


(use-package magit
  :defer t
  :commands (magit-status magit-init magit-file-log magit-blame-mode)
  :bind
  ("C-x g" . magit-status)
   :hook
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpulled-from-upstream-or-recent
                          'magit-insert-unpulled-from-upstream
                          'replace)
  :init
  (hardcore/set-leader-keys
   "ga" 'hardcore/git-add-current-file
   "gc" 'hardcore/git-checkout-current-file
   "gb" 'maigt-branch
   "gB" 'maigt-blame-mode
   "gd" 'magit-diff-buffer-file
   "gl" 'magit-log-buffer-file
   "go" 'magit-file-log
   "gi" 'magit-init
   "gm" 'magit-branch-manager
   "gL" 'magit-list-repositories
   "gr" 'magit-reflog
   "gs" 'magit-status
   "gS" 'magit-stage-file
   "gt" 'magit-tag
   "gU" 'magit-unstage-file
   "gv" 'vc-annotate)
  :config
  (setq magit-revision-show-gravatars t
        magit-diff-refine-hunk t)
  ;; `git-commit-mode'
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

(use-package forge
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after magit
  :init
  (setq forge-database-file (concat hardcore-cache-directory "forge/forge-database.sqlite")))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")
  (define-key magit-todos-section-map "j" nil)
  (magit-todos-mode +1))


(use-package evil-magit
  :ensure t
  :after (evil magit)
  :init
  (setq evil-magit-want-horizontal-movement nil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :defer t
  :after magit
  :diminish magit-gitflow-mode
  :bind (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?G "GitFlow" #'magit-gitflow-popup ?!))

;;; Pop up last commit information of current line
(use-package git-messenger
  :defer t
  :commands (git-messenger:copy-message git-messenger:popup-message)
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t)
  (setq git-messenger:show-detail t)
  (hardcore/set-leader-keys "gm"  'git-messenger:popup-message)
  :config
  (define-key git-messenger-map [escape] 'git-messenger:popup-close))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :commands (hydra-git-timemachine/body)
  :init
  (hardcore/set-leader-keys "gt" #'hydra-git-timemachine/body)
  ;; (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
  (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                               (call-interactively 'git-timemachine))
                                             :post (git-timemachine-quit)
                                             :color pink ;; toggle :foreign-keys run
                                             :hint nil)
    "
[_p_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit\n
"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil exit: t)))

;; Git modes
(use-package gitconfig-mode
  :defer t
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/_gitconfig\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :defer t
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package git-link
  :defer t
  :config
  (hardcore/set-leader-keys "gl" 'git-link-commit)
  (setq git-link-open-in-browser t))

(use-package smerge
  :ensure nil
  ;; :defer t
  :commands (smerge-mode)
  :init
  (defhydra hydra-smerge-mode (:hint nil
                                     :pre (smerge-mode 1)
                                     ;; Disable `smerge-mode' when quitting hydra if
                                     ;; no merge conflicts remain.
                                     :post (smerge-auto-leave))
    "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue))
  (hardcore/set-leader-keys "gr" #'hydra-smerge-mode/body)
  (defun hardcore/enable-smerge-mode-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1)
        (when (featurep 'hydra)
          (hydra-smerge-mode/body))
        )))
  (add-hook 'find-file-hook #'hardcore/enable-smerge-mode-maybe)
  )

;; Highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode diff-hl-next-hunk diff-hl-previous-hunk
                          hydra-diff-hl/body)
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (custom-set-faces
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-delete ((t (:background "#ee6363")))))
  (defhydra hydra-diff-hl (:color pink
                                  :hint nil)
    "
[_p_] previous hunk [_n_] next hunk [_r_] revert hunk [_q_] quit\n
"
    ("p" diff-hl-previous-hunk)
    ("n" diff-hl-next-hunk)
    ("r" diff-hl-revert-hunk)
    ("q" nil exit: t))
  (hardcore/set-leader-keys "gh" #'hydra-diff-hl/body)
  :config
  (diff-hl-flydiff-mode 1))

)
(provide 'hardcore-vcs)


;;; hardcore-git.el ends here
