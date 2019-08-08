;; core-config.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar config-scratch-mode 'text-mode)

(defvar config-leader-key "SPC")

(defvar config-major-mode-leader-key ",")

(defvar config-ex-command-key ":")

(defvar config-command-key "SPC")

(defvar config-which-key-delay 0.2)

(defconst hardcore-mac
  (eq system-type 'darwin)
  "Running on a Mac system?")

(defconst hardcore-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "HardCoremacs storage area for persistent files")

(defgroup hardcore nil
  "hardcore customizations."
  :group 'starter-kit
  :prefix 'hardcore-)

(defvar hardcore-editing-style 'vim)

;; font settings
;; use fonts that should be available on each platform...
;; ...tired of installing fonts across machines :-)

;; Our defaults (Linux or anything else...)
(defvar config-font-family "Monospace")
(defvar config-font-height 120)

;; On OSX use Menlo and draw bigger (am using retina screens scaled small)
(when (eq system-type 'darwin)
  (setq config-font-family "Monaco")
  (setq config-font-height 140))
;; On Windows use Consolas (win 8 and above?)
(when (eq system-type 'windows-nt)
  (setq config-font-family "Consolas"))

(defgroup hardocre-customize-group nil
  "Variables used in my emacs config.")

(defcustom hardcore-user-name "Hardcore"
  "Default username."
  :type 'string
  :group 'hardcore-customize-group)

(defcustom hardcore-logo (expand-file-name "assets/logo.png" user-emacs-directory)
  "Set HardCore logo. nil means official logo."
  :group 'hardcore-customize-group
  :type 'string)

(defcustom hardcore-theme-selected 'monokai
  "Set color theme."
  :type '(choice
          (const :tag "monokai theme" monokai)
          (const :tag "doom one theme" doom-one)
          (const :tag "doom gruvbox theme" doom-gruvbox)
          (const :tag "spacemacs theme" spacemacs)
          (const :tag "gruvbox theme" gruvbox))
  :group 'hardcore-customize-group)

(defcustom hardcore-modeline-selected 'spaceline
  "set modeline"
  :type '(choice
          (const :tag "spaceline" spaceline)
          (const :tag "doom-modeline" doom-modeline))
  :group 'hardcore-customize-group)

(provide 'core-variables)

;;; core-config.el ends here
