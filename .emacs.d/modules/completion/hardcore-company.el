;; hardcore-company.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 hardcoreplayers

;; Author: hardcoreplayers
;; URL: https://github.com/hardcoreplayers/hardcoremacs

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun hardcore/init-company()
  (use-package company
  :diminish company-mode "ⓒ"
  :defer t
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  :hook
  (after-init . global-company-mode)
  ((go-mode) . (lambda () (set (make-local-variable 'company-backends)
                            '((company-yasnippet
                               company-lsp
                               company-files
                               ;; company-dabbrev-code
                               )))))
  :config
  ;; using child frame
  (use-package company-posframe
    :defer t
    :after company-mode
    :hook (company-mode . company-posframe-mode))
  ;; Show pretty icons
   (use-package company-box
        :diminish
        :functions (my-company-box-icons--elisp)
        :commands (company-box--get-color
                    company-box--resolve-colors
                    company-box--add-icon
                    company-box--apply-color
                    company-box-icons--elisp)
        :hook (company-mode . company-box-mode)
        :init
        (setq   company-box-icons-alist 'company-box-icons-all-the-icons
               company-box-backends-colors nil
                company-box-show-single-candidate t
                company-box-max-candidates 50
                company-box-doc-delay 0.5)

        ;; Prettify icons
        (defun my-company-box-icons--elisp (candidate)
            (when (derived-mode-p 'emacs-lisp-mode)
            (let ((sym (intern candidate)))
                (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))
        (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

        (with-eval-after-load 'all-the-icons
            (declare-function all-the-icons-faicon 'all-the-icons)
            (declare-function all-the-icons-material 'all-the-icons)
            (declare-function all-the-icons-material 'all-the-icons)
            (declare-function all-the-icons-octicon 'all-the-icons)
            (setq company-box-icons-all-the-icons
                `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
                    (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
                    (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                    (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                    (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                    (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
                    (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
                    (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                    (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                    (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                    (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
                    (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
                    (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                    (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                    (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
                    (Snippet . ,(all-the-icons-material "code" :height 0.9 :v-adjust -0.2))
                    (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
                    (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
                    (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
                    (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
                    (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                    (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
                    (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                    (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
                    (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
                    (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
                    (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))

    ;; Show quick tooltip
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :custom (company-quickhelp-delay 0.8))))
(provide 'hardcore-company)

;;; hardcore-company.el ends here
