;;; package --- Summary
;;; init.el ---

;;; Commentary:
;; This configuration sets up Emacs with basic package management.
;; It installs and configures 'use-package' to handle further package installations.
;; 
;; Additional Setup:
;; - To use C-SPC as set-mark-command on macOS, modify at the macOS level:
;;   System Preferences > Keyboard > Shortcuts > Input Sources > 
;;   Select the previous input source and uncheck it.
;;
;; Inspired by https://github.com/MatthewZMD/.emacs.d#org9bf5ed1

;;; Code:

;; Adjust garbage collection threshold to speed up startup
(setq gc-cons-threshold 100000000)

;; Initialize package management
(require 'package)

;; Set package archives, using MELPA and GNU repositories
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; Uncomment the following lines for alternative mirrors
                         ;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
                         ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ))

;; Initialize package sources
;; (package-initialize) ;; Optional in Emacs 27+ (remove if on a newer version)

;; Install 'use-package' if it's not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load and configure 'use-package'
(eval-when-compile
  (require 'use-package))

(provide 'init) ;; Optional: Provides the 'init' feature

;;; init.el ends here
