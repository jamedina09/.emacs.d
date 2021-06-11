;;; early-init.el
;;
;; Obtained from: https://github.com/MatthewZMD/.emacs.d

;; DeferGC
(setq gc-cons-threshold 100000000)
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
;; Remove tool bar an scroll bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; -DisableUnnecessaryInterface

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
