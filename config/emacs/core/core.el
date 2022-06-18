;;; core.el -*- lexical-binding: t; -*-

(defconst local-dir (concat user-emacs-directory "~/.local/"))
(defconst cache-dir (concat local-dir "cache/"))

;;; Native Compilation support (http://akrl.sdf.org/gccemacs.html)
(when (fboundp 'native-comp-available-p)
  ;; Don't store eln files in ~/.emacs.d/eln-cache (they are likely to be purged
  ;; when upgrading Doom).
  (add-to-list 'native-comp-eln-load-path (concat cache-dir "eln/")))

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, howeveor
(setq inhibit-compacting-font-caches t)

;; confi font
 (set-face-attribute 'default nil
                     :family "Operator Mono Lig"  :height 150 :weight 'Light)

(provide 'core)
;;; core.el ends here
