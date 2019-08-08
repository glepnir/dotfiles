(defun hardcore/init-yasnippet()

(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode))

)

(provide 'hardcore-yasnippet)
