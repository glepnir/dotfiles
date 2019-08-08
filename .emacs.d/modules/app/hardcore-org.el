;; hardcore-org.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-org ()

  (use-package htmlize
    :ensure t)

  (use-package org-preview-html
    :ensure t)

  (hardcore/init-org-evil)
  (hardcore/init-org-bullets)
  (hardcore/init-org-agenda)
  (hardcore/init-org-capture)
  (hardcore/init-org-export)
  (hardcore/init-org-download)
  (hardcore/init-org-key-bindings)

  ;; Some appearance tweaks
  (setq org-hide-emphasis-markers t)
  (setq org-fontify-whole-heading-line t)
  (setq org-src-fontify-natively t)

  ;; Paragraph filling
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook
            (lambda() (set-fill-column 80))))

(defun hardcore/org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY. PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

(defun hardcore/init-org-evil ()
  ; evil key bindings
  (use-package evil-org
    :ensure t
    :after org
    :diminish 'evil-org-mode
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(navigation))))))

(defun hardcore/init-org-bullets ()
  ; unicode bullets for headings
  (use-package org-bullets
    :ensure t
    :after org
    :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(defun hardcore/init-org-agenda ()
  ;; Org directories
  (setq org-agenda-files
        '("~/Org/"))
  ;; Default show all of the things
  (setq org-startup-folded nil)
  ;; Exports don't have TOC or numbering by default
  (setq org-export-with-toc nil)
  (setq org-export-with-creator nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-sub-superscripts nil)
  ;; Indent by default
  (setq org-startup-indented t)
  ;;open agenda in current window
  ;;(setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)
  ;;show me tasks scheduled or due in next fortnight
  (setq org-agenda-span (quote fortnight))
  ;; Agenda Settings
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "")
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (hardcore/org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline)))))))))))

(defun hardcore/init-org-capture ()
  ;; Org capture setup
  (setq org-default-notes-file "~/Org/inbox.org")
  (setq org-refile-targets '((nil :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  (setq org-agenda-default-appointment-duration 60))

(defun hardcore/init-org-export ()
  ;; LaTeX Export
  (with-eval-after-load 'ox-latex
    (setq org-export-latex-listings t)
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))
    (add-to-list 'org-latex-packages-alist '("" "fullpage"))
    (add-to-list 'org-latex-packages-alist '("" "libertine")))

(defun hardcore/init-org-download ()
  (use-package org-download
    :ensure t)
  ;; Use org attachements for org-download
  (setq org-download-method 'attach)
  ;; Screenshot on Mac
  (if (equal system-type 'darwin)
      (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s")))

(defun hardcore/init-org-key-bindings ()
  (dolist (prefix '(
                        ("mb" . "babel")
                        ("mC" . "clocks")
                        ("md" . "dates")
                        ("me" . "export")
                        ("mf" . "feeds")
                        ("mi" . "insert")
                        ("miD" . "download")
                        ("mm" . "more")
                        ("ms" . "trees/subtrees")
                        ("mT" . "toggles")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")
                        ("mx" . "text")
                        ))
      (hardcore/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (hardcore/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture

        ;; Clock
        ;; These keybindings should match those under the "aoC" prefix (below)
        "Cc" 'org-clock-cancel
        "Cd" 'org-clock-display
        "Ce" 'org-evaluate-time-range
        "Cg" 'org-clock-goto
        "Ci" 'org-clock-in
        "CI" 'org-clock-in-last
        "Cj" 'hardcore/org-clock-jump-to-current-clock
        "Co" 'org-clock-out
        "CR" 'org-clock-report
        "Cr" 'org-resolve-clocks

        "dd" 'org-deadline
        "ds" 'org-schedule
        "dt" 'org-time-stamp
        "dT" 'org-time-stamp-inactive
        "ee" 'org-export-dispatch
        "fi" 'org-feed-goto-inbox
        "fu" 'org-feed-update-all

        "a" 'org-agenda

        "p" 'org-priority

        "Tc" 'org-toggle-checkbox
        "Te" 'org-toggle-pretty-entities
        "Ti" 'org-toggle-inline-images
        "Tl" 'org-toggle-link-display
        "Tt" 'org-show-todo-tree
        "TT" 'org-todo
        "TV" 'space-doc-mode
        "Tx" 'org-toggle-latex-fragment

        ;; More cycling options (timestamps, headlines, items, properties)
        "L" 'org-shiftright
        "H" 'org-shiftleft
        "J" 'org-shiftdown
        "K" 'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "sa" 'org-toggle-archive-tag
        "sA" 'org-archive-subtree
        "sb" 'org-tree-to-indirect-buffer
        "sh" 'org-promote-subtree
        "sj" 'org-move-subtree-down
        "sk" 'org-move-subtree-up
        "sl" 'org-demote-subtree
        "sn" 'org-narrow-to-subtree
        "sN" 'widen
        "sr" 'org-refile
        "ss" 'org-sparse-tree
        "sS" 'org-sort

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "th" 'org-table-previous-field
        "tH" 'org-table-move-column-left
        "tic" 'org-table-insert-column
        "tih" 'org-table-insert-hline
        "tiH" 'org-table-hline-and-move
        "tir" 'org-table-insert-row
        "tI" 'org-table-import
        "tj" 'org-table-next-row
        "tJ" 'org-table-move-row-down
        "tK" 'org-table-move-row-up
        "tl" 'org-table-next-field
        "tL" 'org-table-move-column-right
        "tn" 'org-table-create
        "tN" 'org-table-create-with-table.el
        "tr" 'org-table-recalculate
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Source blocks / org-babel
        "bp"     'org-babel-previous-src-block
        "bn"     'org-babel-next-src-block
        "be"     'org-babel-execute-maybe
        "bo"     'org-babel-open-src-block-result
        "bv"     'org-babel-expand-src-block
        "bu"     'org-babel-goto-src-block-head
        "bg"     'org-babel-goto-named-src-block
        "br"     'org-babel-goto-named-result
        "bb"     'org-babel-execute-buffer
        "bs"     'org-babel-execute-subtree
        "bd"     'org-babel-demarcate-block
        "bt"     'org-babel-tangle
        "bf"     'org-babel-tangle-file
        "bc"     'org-babel-check-src-block
        "bj"     'org-babel-insert-header-arg
        "bl"     'org-babel-load-in-session
        "bi"     'org-babel-lob-ingest
        "bI"     'org-babel-view-src-block-info
        "bz"     'org-babel-switch-to-session
        "bZ"     'org-babel-switch-to-session-with-code
        "ba"     'org-babel-sha1-hash
        "bx"     'org-babel-do-key-sequence-in-edit-buffer
        "b."     'hardcore/org-babel-transient-state/body
        ;; Multi-purpose keys
        "," 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "-" 'org-ctrl-c-minus
        "#" 'org-update-statistics-cookies
        "RET"   'org-ctrl-c-ret
        "M-RET" 'org-meta-return
        ;; attachments
        "A" 'org-attach
        ;; insertion
        "ib" 'org-insert-structure-template
        "id" 'org-insert-drawer
        "ie" 'org-set-effort
        "if" 'org-footnote-new
        "ih" 'org-insert-heading
        "iH" 'org-insert-heading-after-current
        "iK" 'hardcore/insert-keybinding-org
        "il" 'org-insert-link
        "in" 'org-add-note
        "ip" 'org-set-property
        "is" 'org-insert-subheading
        "it" 'org-set-tags-command
        "xo" 'org-open-at-point)

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (hardcore/declare-prefix "ao" "org")
      (hardcore/declare-prefix "aof" "feeds")
      (hardcore/declare-prefix "aoC" "clock")
      (hardcore/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
        "aoc" 'org-capture
        "aoe" 'org-store-agenda-views
        "aofi" 'org-feed-goto-inbox
        "aofu" 'org-feed-update-all

        ;; Clock
        ;; These keybindings should match those under the "mC" prefix (above)
        "aoCc" 'org-clock-cancel
        "aoCg" 'org-clock-goto
        "aoCi" 'org-clock-in
        "aoCI" 'org-clock-in-last
        "aoCj" 'hardcore/org-clock-jump-to-current-clock
        "aoCo" 'org-clock-out
        "aoCr" 'org-resolve-clocks

        "aol" 'org-store-link
        "aom" 'org-tags-view
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))
)

(provide 'hardcore-org)

;;; hardcore-org.el ends here
