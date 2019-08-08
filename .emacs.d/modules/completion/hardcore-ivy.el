;; hardcore-ivy.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-ivy()
  (use-package ivy
    :config
    (ivy-mode 1)
    )
  (hardcore/ivy-init-counsel)
  (hardcore//ivy-hjkl-navigation hardcore-editing-style)

)

(defvar hardcore--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("w" find-file-other-window "other window")
    ("v" hardcore/find-file-vsplit "in vertical split")
    ("s" hardcore/find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" hardcore/delete-file-confirm "delete file")
    ("r" hardcore/rename-file "rename file"))
  "Default ivy actions for files.")

(defun hardcore/ivy-init-counsel()
    (use-package counsel
    :diminish ivy-mode counsel-mode
    :defines (projectile-completion-system
                magit-completing-read-function)
    :functions (my-ivy-fly-time-travel
                my-swiper-toggle-counsel-rg
                my-swiper-toggle-rg-dwim)
    :commands (ivy--format-function-generic
                ivy--add-face)
    :hook ((after-init . ivy-mode)
            (ivy-mode . counsel-mode))
    :config
    (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
        ;; set additional ivy actions
    ;; custom actions for recentf
    (ivy-set-actions
        'counsel-recentf
        hardcore--ivy-file-actions)
    (ivy-set-actions
        'counsel-find-file
        hardcore--ivy-file-actions)
     (counsel-mode 1)
    :init
    (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

    (setq ivy-use-selectable-prompt t
            ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
            ivy-height 10
            ivy-count-format "(%d/%d) "
            ivy-on-del-error-function nil
            ivy-initial-inputs-alist nil)

    (defun my-ivy-format-function-arrow (cands)
        "Transform CANDS into a string for minibuffer."
        (ivy--format-function-generic
        (lambda (str)
        (concat (if (display-graphic-p)
                    (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                    ">")
                (propertize " " 'display `(space :align-to 2))
                (ivy--add-face str 'ivy-current-match)))
        (lambda (str)
        (concat (propertize " " 'display `(space :align-to 2)) str))
        cands
        "\n"))
    (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))

    (setq swiper-action-recenter t)

    (setq counsel-find-file-at-point t
            counsel-yank-pop-separator "\n────────\n")

    ;; Use faster search tool: ripgrep (rg)
    (when (executable-find "rg")
        (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s"))
    :config
    (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands '(query-replace-regexp
                                    flush-lines
                                    keep-lines
                                    ivy-read
                                    swiper
                                    swiper-backward
                                    swiper-all
                                    swiper-isearch
                                    swiper-isearch-backward
                                    counsel-grep-or-swiper
                                    counsel-grep-or-swiper-backward
                                    counsel-grep
                                    counsel-ag
                                    counsel-rg))

    (defun my-ivy-fly-back-to-present ()
        ;; (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
        (cond ((and (memq last-command my-ivy-fly-commands)
                    (equal (this-command-keys-vector) (kbd "M-p")))
            ;; repeat one time to get straight to the first history item
            (setq unread-command-events
                    (append unread-command-events
                            (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command '(self-insert-command
                                        yank
                                        ivy-yank-word
                                        counsel-yank-pop))
                (equal (this-command-keys-vector) (kbd "M-n")))
            (delete-region (point)
                            (point-max)))))

    (defun my-ivy-fly-time-travel ()
        (when (memq this-command my-ivy-fly-commands)
        (let* ((kbd (kbd "M-n"))
                (cmd (key-binding kbd))
                (future (and cmd
                            (with-temp-buffer
                                (when (ignore-errors
                                        (call-interactively cmd) t)
                                (buffer-string))))))
            (when future
            (save-excursion
                (insert (propertize (replace-regexp-in-string
                                    "\\\\_<" ""
                                    (replace-regexp-in-string
                                    "\\\\_>" ""
                                    future))
                                    'face 'shadow)))
            (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

    ;; Improve search experience of `swiper'
    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
        "Toggle `counsel-rg' with current swiper input."
        (interactive)
        (let ((text (replace-regexp-in-string
                    "\n" ""
                    (replace-regexp-in-string
                    "\\\\_<" ""
                    (replace-regexp-in-string
                    "\\\\_>" ""
                    (replace-regexp-in-string "^.*Swiper: " ""
                                                (thing-at-point 'line t)))))))
        (ivy-quit-and-run
            (counsel-rg text default-directory))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)

    (with-eval-after-load 'rg
        (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with current swiper input."
        (interactive)
        (ivy-quit-and-run (rg-dwim default-directory)))
        (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
        (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
        (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
        (setq magit-completing-read-function 'ivy-completing-read))

    ;; Enhance M-x
    (use-package amx
        :init (setq amx-history-length 20))

    ;; Better sorting and filtering
    (use-package prescient
        :commands prescient-persist-mode
        :init
        (setq prescient-filter-method '(literal regexp initialism fuzzy))
        (prescient-persist-mode 1))

    (use-package ivy-prescient
        :commands ivy-prescient-re-builder
        :preface
        (defun ivy-prescient-non-fuzzy (str)
        (let ((prescient-filter-method '(literal regexp)))
            (ivy-prescient-re-builder str)))
        :init
        (setq ivy-prescient-enable-filtering t
            ivy-prescient-retain-classic-highlighting t
            ivy-re-builders-alist '(
                                    (counsel-rg . ivy-prescient-non-fuzzy)
                                    (counsel-ag . ivy-prescient-non-fuzzy)
                                    (counsel-grep . ivy-prescient-non-fuzzy)
                                    (swiper . ivy-prescient-non-fuzzy)
                                    (swiper-isearch . ivy-prescient-non-fuzzy)
                                    (swiper-all . ivy-prescient-non-fuzzy)
                                    (t . ivy-prescient-re-builder)))
        (ivy-prescient-mode 1))

    ;; Additional key bindings for Ivy
    (use-package ivy-hydra
        :bind (:map ivy-minibuffer-map
            ("M-o" . ivy-dispatching-done-hydra)))

    ;; Ivy integration for Projectile
    (use-package counsel-projectile
        :init
        (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
        (setq projectile-switch-project-action 'counsel-projectile-find-file)
        (counsel-projectile-mode 1))


    ;; Integrate yasnippet
    (use-package ivy-yasnippet
        :commands ivy-yasnippet--preview
        :bind ("C-c C-y" . ivy-yasnippet)
        :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

    ;; Select from xref candidates with Ivy
    (use-package ivy-xref
        :init
        (when (boundp 'xref-show-definitions-function)
        (setq xref-show-definitions-function #'ivy-xref-show-defs))
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

    ;; Correcting words with flyspell via Ivy
    (use-package flyspell-correct-ivy
        :after flyspell
        :bind (:map flyspell-mode-map
            ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))


    ;; Display world clock using Ivy
    (use-package counsel-world-clock
        :bind (:map counsel-mode-map
            ("C-c c k" . counsel-world-clock)))

    ;; Tramp ivy interface
    (use-package counsel-tramp
        :bind (:map counsel-mode-map
            ("C-c c v" . counsel-tramp)))

    ;; Support pinyin in Ivy
    ;; Input prefix ':' to match pinyin
    ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
    ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
    (use-package pinyinlib
        :functions (ivy--regex-plus ivy-prescient-non-fuzzy)
        :commands pinyinlib-build-regexp-string
        :preface
        (defun ivy--regex-pinyin (str)
        "The regex builder wrapper to support pinyin."
        (or (pinyin-to-utf8 str)
            (ivy-prescient-non-fuzzy str)
            (ivy--regex-plus str)))
        (defun my-pinyinlib-build-regexp-string (str)
        "Build a pinyin regexp sequence from STR."
        (cond ((equal str ".*") ".*")
                (t (pinyinlib-build-regexp-string str t))))
        (defun my-pinyin-regexp-helper (str)
        "Construct pinyin regexp for STR."
        (cond ((equal str " ") ".*")
                ((equal str "") nil)
                (t str)))
        (defun pinyin-to-utf8 (str)
        (cond ((equal 0 (length str)) nil)
                ((equal (substring str 0 1) "!")
                (mapconcat 'my-pinyinlib-build-regexp-string
                            (remove nil (mapcar 'my-pinyin-regexp-helper
                                                (split-string
                                                (replace-regexp-in-string "!" "" str ) "")))
                            ""))
                (t nil)))
        :init
        (dolist (fn '(swiper swiper-isearch swiper-all counsel-ag counsel-grep))
        (setf (alist-get fn ivy-re-builders-alist) #'ivy--regex-pinyin))))

    ;; More friendly display transformer for Ivy
    (use-package ivy-rich
    :defines (all-the-icons-icon-alist
                all-the-icons-dir-icon-alist
                bookmark-alist)
    :functions (all-the-icons-icon-for-file
                all-the-icons-icon-for-mode
                all-the-icons-icon-family
                all-the-icons-faicon
                all-the-icons-octicon
                all-the-icons-material
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-dir-is-submodule
                my-ivy-rich-bookmark-type)
    :commands (ivy-rich-bookmark-filename
                ivy-rich-bookmark-type)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
        (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
        "Display buffer icons in `ivy-rich'."
        (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
                (buffer-file-name (buffer-file-name buffer))
                (major-mode (buffer-local-value 'major-mode buffer))
                (icon (if (and buffer-file-name
                                (all-the-icons-auto-mode-match?))
                        (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                        (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
            (if (symbolp icon)
                (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
        "Display file icons in `ivy-rich'."
        (when (display-graphic-p)
        (let* ((path (file-local-name (concat ivy--directory candidate)))
                (file (file-name-nondirectory path))
                (icon (cond
                        ((file-directory-p path)
                        (cond
                        ((and (fboundp 'tramp-tramp-file-p)
                                (tramp-tramp-file-p default-directory))
                        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                        ((file-symlink-p path)
                        (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                        ((all-the-icons-dir-is-submodule path)
                        (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                        ((file-exists-p (format "%s/.git" path))
                        (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                        (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                            (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                        ((string-match "^/.*:$" path)
                        (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                        ((not (string-empty-p file))
                        (all-the-icons-icon-for-file file :v-adjust -0.05)))))
            (if (symbolp icon)
                (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-dir-icon (_candidate)
        "Display directory icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

    (defun ivy-rich-function-icon (_candidate)
        "Display function icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
        "Display variable icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
        "Display symbol icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
        "Display theme icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-keybinding-icon (_candidate)
        "Display keybindings icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-library-icon (_candidate)
        "Display library icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
        "Display package icons in `ivy-rich'."
        (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

    (when (display-graphic-p)
        (defun my-ivy-rich-bookmark-type (candidate)
        (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
            (cond ((null filename)
                (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
                ((not (file-exists-p filename))
                (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
        (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type))
    :hook ((ivy-mode . ivy-rich-mode)
            (ivy-rich-mode . (lambda ()
                                (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)

    ;; Setting tab size to 1, to insert tabs as delimiters
    (add-hook 'minibuffer-setup-hook
                (lambda ()
                (setq tab-width 1)))

    (setq ivy-rich-display-transformers-list
            '(ivy-switch-buffer
            (:columns
            ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))
            :delimiter "\t")
            ivy-switch-buffer-other-window
            (:columns
            ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))
            :delimiter "\t")
            counsel-switch-buffer
            (:columns
            ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))
            :delimiter "\t")
            counsel-switch-buffer-other-window
            (:columns
            ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))
            :delimiter "\t")
            persp-switch-to-buffer
            (:columns
            ((ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
            :predicate
            (lambda (cand) (get-buffer cand))
            :delimiter "\t")
            counsel-M-x
            (:columns
            ((ivy-rich-function-icon)
                (counsel-M-x-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
            ((ivy-rich-function-icon)
                (counsel-describe-function-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
            ((ivy-rich-variable-icon)
                (counsel-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-apropos
            (:columns
            ((ivy-rich-symbol-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-info-lookup-symbol
            (:columns
            ((ivy-rich-symbol-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-descbinds
            (:columns
            ((ivy-rich-keybinding-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-find-file
            (:columns
            ((ivy-rich-file-icon)
                (ivy-read-file-transformer))
            :delimiter "\t")
            counsel-file-jump
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-dired
            (:columns
            ((ivy-rich-file-icon)
                (ivy-read-file-transformer))
            :delimiter "\t")
            counsel-dired-jump
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-fzf
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-git
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-recentf
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate (:width 0.8))
                (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
            :delimiter "\t")
            counsel-bookmark
            (:columns
            ((ivy-rich-bookmark-type)
                (ivy-rich-bookmark-name (:width 40))
                (ivy-rich-bookmark-info))
            :delimiter "\t")
            counsel-package
            (:columns
            ((ivy-rich-package-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-find-library
            (:columns
            ((ivy-rich-library-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-load-library
            (:columns
            ((ivy-rich-library-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-load-theme
            (:columns
            ((ivy-rich-theme-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-projectile-switch-project
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t")
            counsel-projectile-find-file
            (:columns
            ((ivy-rich-file-icon)
                (counsel-projectile-find-file-transformer))
            :delimiter "\t")
            counsel-projectile-find-dir
            (:columns
            ((ivy-rich-dir-icon)
                (counsel-projectile-find-dir-transformer))
            :delimiter "\t")
            treemacs-projectile
            (:columns
            ((ivy-rich-file-icon)
                (ivy-rich-candidate))
            :delimiter "\t"))))
            (hardcore/ivy-init-keybindings)
 )
(defun hardcore/ivy-init-keybindings()
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
  (hardcore/set-leader-keys
   "SPC" 'counsel-M-x
    ;; files
    "ff" 'counsel-find-file
    "fL" 'counsel-locate
    "fr" 'counsel-rencentf
    "fl" 'counsel-find-library
    ;; help
    "?"  'counsel-descbinds
    ;; insert
    "iu" 'counsel-unicode-char
    ;; jump
    ;; register/ring
    "ry" 'counsel-yank-pop
    "ii" 'counsel-imenu
    "io" 'counsel-outline
    ;; themes
    "tl" 'counsel-load-theme
    ;; projectile
    "p SPC" 'counsel-projectile
    "pb"    'counsel-projectile-switch-to-buffer
    "pd"    'counsel-projectile-find-dir
    "pp"    'counsel-projectile-switch-project
    "pf"    'counsel-projectile-find-file
    "pr"    'counsel-recentf

    ;; searuch
    "gp" 'counsel-git-grep
    "gg" 'counsel-git
    "sr" 'counsel-rg
    "sa" 'counsel-ag
    "sf" 'counsel-fzf)
  )
;; Ivy

(defun hardcore//ivy-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    ;; Move C-h to C-S-h
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>")
      'minibuffer-keyboard-quit))
   (t
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
    (define-hardcore ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-h") nil)
    (define-key ivy-minibuffer-map (kbd "C-l") nil))))


(provide 'hardcore-ivy)

;;; hardcore-ivy.el ends here
