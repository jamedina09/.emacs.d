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

(setq initial-major-mode 'text-mode)

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

;;----------------------------------------------------------------------------
;; Personal information
;;----------------------------------------------------------------------------
(setq user-full-name "J.A. Medina-Vega")
(setq user-mail-address "jamedina09@gmail.com")

;;----------------------------------------------------------------------------
;; Theme: doom-themes
;;----------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t     ;; If nil, bold is universally disabled
        doom-themes-enable-italic t   ;; If nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-colors")

  ;; Load Doom Dracula theme by default
  (load-theme 'doom-dracula t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;----------------------------------------------------------------------------
;; Kill general login buffers
;;----------------------------------------------------------------------------
;; Makes *scratch* empty.
(setq initial-scratch-message nil)

;; Removes *Messages* buffer log from showing permanently
(setq-default message-log-max nil)

;; Optionally kill *Messages* buffer
(kill-buffer "*Messages*")

;;----------------------------------------------------------------------------
;; Interface and General Tweaks
;;----------------------------------------------------------------------------

;; Background transparency
(set-frame-parameter nil 'alpha-background 100) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

;; Define the home directory
(cd (getenv "HOME"))
(message "Current dir: %s" (pwd))
(message "Current buffer: %s" (buffer-name))

;; Disable unnecessary UI elements
(menu-bar-mode -1)         ;; Disable menu bar
(tool-bar-mode -1)         ;; Disable tool bar
;; (set-scroll-bar-mode nil)  ;; Disable scroll bar

;; Set UTF-8 as the default encoding system
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Set initial window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 90))

;; Use 'y' or 'n' instead of 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable region overwrite when selection is active
(delete-selection-mode t)

;; Highlight matching parentheses
(show-paren-mode t)

;; Disable lockfiles (no creation of `.#` files)
(setq create-lockfiles nil)

;; Visual tweaks
(setq visible-bell t)                      ;; Enable visual bell
(setq ring-bell-function 'ignore)          ;; Disable audio bell

;; Suppress startup screen
(setq inhibit-startup-screen t)

;; Confirm before quitting Emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Customize title bar
(setq-default frame-title-format '("" user-login-name "@" system-name " - %b"))

;; Make the fringe narrower
;; (fringe-mode 1)

;; Automatically trim trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)  ;; Ensure files end with a newline

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; Enable line and column numbers in the mode line
(setq column-number-mode t)
(setq line-number-mode t)

;; Show line numbers globally using display-line-numbers-mode (faster than linum-mode)
(global-display-line-numbers-mode t)

;; Customize the appearance of the line numbers
(setq display-line-numbers-type 'relative)  ;; Use 't' for absolute, 'relative' for relative line numbers
(setq display-line-numbers "%d ")

;; Highlight the current line
(global-hl-line-mode t)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)          ;; Disables suspend-frame (C-z)
(global-set-key (kbd "M-z") nil)          ;; Disables zap-to-char (M-z)
(global-set-key (kbd "M-m") nil)          ;; Disables back-to-indentation (M-m)
(global-set-key (kbd "C-x C-z") nil)      ;; Disables minimize window (C-x C-z)
(global-set-key (kbd "M-/") nil)          ;; Disables dabbrev-expand (M-/)

;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; Move up/down paragraph
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "M-<up>") #'backward-paragraph)

;;----------------------------------------------------------------------------
;; Backups
;;----------------------------------------------------------------------------

;; Define backup directory
(defconst my-backup-dir
  (expand-file-name "backups" user-emacs-directory))

;; Ensure the backup directory exists
(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir t))

(setq make-backup-files t               ;; Enable backups
      version-control t                 ;; Enable versioning of backups
      backup-by-copying t               ;; Use copying to preserve symlinks
      backup-directory-alist `(("." . ,my-backup-dir)) ;; Store backups in defined directory
      kept-new-versions 6               ;; Number of newest versions to keep
      kept-old-versions 2               ;; Number of oldest versions to keep
      delete-old-versions t             ;; Automatically delete excess backups
      vc-make-backup-files t            ;; Backup files under version control
      auto-save-default nil)            ;; Disable auto-save (#file#)

;; Optional: Redirect backups to a specific location (e.g., Google Drive)
(setq backup-directory-alist '(("." . "~/GDrive_Personal/EMACS_BACKUPS/")))

;; (setq auto-save-default t
;;      auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;; ;;----------------------------------------------------------------------------
;; ;; Time-stamp
;; ;;----------------------------------------------------------------------------
;; ;; When there is a "Time-stamp: <>" in the first 10 lines of the file,
;; ;; Emacs will write time-stamp information there when saving the file.
;; (setq time-stamp-active t              ;; Enable time-stamp updating
;;       time-stamp-line-limit 10         ;; Check the first 10 lines for Time-stamp: <>
;;       time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %L") ;; Date format

;; (add-hook 'write-file-functions 'time-stamp) ;; Update the time-stamp before saving

;;----------------------------------------------------------------------------
;; Use ibuffer instead of normal buffer list
;;----------------------------------------------------------------------------
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 10 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

;; Group buffers by major mode or directory
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs" (mode . emacs-lisp-mode))
         ("Org" (mode . org-mode))
         ("Dired" (mode . dired-mode))
         ("Magit" (mode . magit-status-mode))
         ("Python" (mode . python-mode))
         ("Others" nil))))

;; Set default filter groups to make it easier to switch between different buffer sets.
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Adjust ibuffer columns to fit your workflow
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 20 -1 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 20 -1)
              " " filename)))

;;;;----------------------------------------------------------------------------
;;;; Dashboard
;;;;----------------------------------------------------------------------------
;;(use-package dashboard
;;  :ensure t
;;  :init
;;  (dashboard-setup-startup-hook)
;;  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;  ;; Set the title
;;  (setq dashboard-banner-logo-title "")
;;  ;; Set the banner
;;  (setq dashboard-startup-banner 'official)
;;  ;; Value can be
;;  ;; 'official which displays the official emacs logo
;;  ;; 'logo which displays an alternative emacs logo
;;  ;; 1, 2 or 3 which displays one of the text banners
;;  ;; "path/to/your/image.png" which displays whatever image you would prefer;
;;  ;; Content is not centered by default. To center, set
;;  (setq dashboard-center-content t)
;;  ;; To disable shortcut "jump" indicators for each section, set
;;  ;;(setq dashboard-show-shortcuts nil)
;;(setq dashboard-items '((recents . 10)
;;                        (projects . 5)
;;                        (bookmarks . 5)
;;                        (agenda . 5)))
;;  ;; To add icons to the widget headings and their items:
;;  (setq dashboard-set-heading-icons t)
;;  (setq dashboard-set-file-icons t)
;;  ;; A randomly selected footnote will be displayed. To disable it:
;;  (setq dashboard-set-footer nil)
;;  ;; horizontal lines
;;  (setq dashboard-page-separator "\n\f\n")
;;  )

;;;;----------------------------------------------------------------------------
;;;; Page break lines
;;;;----------------------------------------------------------------------------
;;(use-package page-break-lines
;;  :ensure t
;;  :config
;;  (setq global-page-break-lines-mode t)
;;  (set-fontset-font "fontset-default"
;;                    (cons page-break-lines-char page-break-lines-char)
;;                    (face-attribute 'default :family)))

;; (custom-set-faces
;;  '(page-break-lines
;;    ((t (:foreground "light gray" :background "dark gray" :weight bold)))))

(global-set-key (kbd "C-c p") 'page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Dired
;;----------------------------------------------------------------------------
;; Configure built-in Dired
(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

;;----------------------------------------------------------------------------
;; Dired Subtree
;;----------------------------------------------------------------------------
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;;----------------------------------------------------------------------------
;; Dired Hide Dotfiles
;;----------------------------------------------------------------------------
(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;;----------------------------------------------------------------------------
;; Dired K
;;----------------------------------------------------------------------------
(use-package dired-k
  :ensure t
  :defer t
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;;----------------------------------------------------------------------------
;; transpose-frame
;;----------------------------------------------------------------------------
(use-package transpose-frame
  :ensure t
  :bind (("C-c t" . transpose-frame)
         ("C-c f" . rotate-frame)))

;;----------------------------------------------------------------------------
;; switch-window
;;----------------------------------------------------------------------------
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

;;----------------------------------------------------------------------------
;; osx-trash
;;----------------------------------------------------------------------------
;; in macos delete-by...-to-trash does not work without this
;; install: brew install trash
(use-package osx-trash
  :ensure t
  :init
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

;;----------------------------------------------------------------------------
;; goto-line-preview
;;----------------------------------------------------------------------------
(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;;----------------------------------------------------------------------------
;; exec-path-from-shell
;;----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;;----------------------------------------------------------------------------
;; all the icons
;;----------------------------------------------------------------------------
;; For this package to work best, you need to install the resource fonts
;; included in the package. M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; If you experience a slow down in performance when rendering multiple icons
;; simultaneously, you can try setting the following variable
;; (setq inhibit-compacting-font-caches t)

;; ;;----------------------------------------------------------------------------
;; ;; Projectile
;; ;;----------------------------------------------------------------------------
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   :bind (:map projectile-mode-map
;;               ("C-c p" . projectile-command-map))
;;   :config
;;   (setq projectile-sort-order 'recently-active))

;;----------------------------------------------------------------------------
;; Dimmer
;;----------------------------------------------------------------------------
(use-package dimmer
  :ensure t
  :init
  (dimmer-mode)
  :custom
  (dimmer-fraction 0.2))

;;----------------------------------------------------------------------------
;; smartparens
;;----------------------------------------------------------------------------
(use-package smartparens
  :ensure t
  :hook ((prog-mode) . smartparens-mode)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))

;;----------------------------------------------------------------------------
;; Doom-line
;;----------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 7)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-lsp t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-file-name-style 'auto) ;;truncate-upto-root
  :init (doom-modeline-mode 1))
;;This package requires the fonts included with all-the-icons to be installed.
;;Run M-x all-the-icons-install-fonts to do so.

;;----------------------------------------------------------------------------
;; Ivy and Dependencies
;;----------------------------------------------------------------------------

;; Install and configure Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)   ; Bind M-x to counsel-M-x
         ("C-s" . swiper)        ; Bind C-s to swiper for search
         ("C-x C-f" . counsel-find-file) ; Bind C-x C-f to counsel-find-file
         ("M-y" . counsel-yank-pop) ; Bind M-y to counsel-yank-pop
         ("C-h f" . counsel-describe-function) ; Bind C-h f to counsel-describe-function
         ("C-h v" . counsel-describe-variable)) ; Bind C-h v to counsel-describe-variable
  :config
  (use-package smex :ensure t)) ; Ensure smex is installed

;; Install and configure Ivy
(use-package ivy
  :ensure t
  :defer 0.1
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy--regex-ignore-order t)
  :config (ivy-mode))

;; Install and configure Ivy-Rich
(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbrev)
  :init (ivy-rich-mode 1))

;; Install and configure Swiper
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; ;;----------------------------------------------------------------------------
;; ;; Counsel projectile
;; ;;----------------------------------------------------------------------------
;; ;; Install and configure Counsel Projectile
;; (use-package counsel-projectile
;;   :ensure t
;;   :after (counsel projectile) ; Ensure counsel and projectile are loaded first
;;   :config
;;   (counsel-projectile-mode)) ; Enable counsel-projectile integration

;; ;; Bind `C-c p p` to `projectile-switch-project`
;; (bind-key "C-c p p" 'projectile-switch-project projectile-mode-map)

;;----------------------------------------------------------------------------
;; all the icons
;;----------------------------------------------------------------------------
;; Install and configure all-the-icons for Ivy-based completions
(use-package all-the-icons-ivy
  :ensure t
  :init
  (all-the-icons-ivy-setup))  ; Initialize all-the-icons-ivy

;; Install and configure all-the-icons-ivy-rich for richer Ivy completions
(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1))  ; Enable all-the-icons-ivy-rich

;; Install and configure all-the-icons-dired for dired buffers
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))  ; Enable all-the-icons-dired in dired-mode

;; Install and configure all-the-icons-ibuffer for ibuffer buffers
(use-package all-the-icons-ibuffer
  :ensure t
  :init
  (all-the-icons-ibuffer-mode 1))  ; Enable all-the-icons-ibuffer

;; M-x all-the-icons-install-fonts


;;;;----------------------------------------------------------------------------
;;;; Company-mode
;;;;----------------------------------------------------------------------------
;;;; Install and configure company-mode
;;(use-package company
;;  :ensure t
;;  :init
;;  (global-company-mode)  ; Enable company-mode globally
;;  :custom
;;  ;; Set the minimum prefix length to trigger suggestions
;;  (company-minimum-prefix-length 2)
;;  ;; Number of candidates to display at once
;;  (company-tooltip-limit 20)
;;  ;; Delay before suggestions pop up
;;  (company-idle-delay 0.2)
;;  ;; Delay after typing before suggestions are shown
;;  (company-tooltip-idle-delay 0.2)
;;  ;; Set the company backends for completion
;;  (company-backends '((company-files
;;                       company-keywords
;;                       company-capf
;;                       company-dabbrev-code
;;                       company-dabbrev)))
;;  :bind
;;  ;; Key bindings for company-mode
;;  (:map company-active-map
;;        ("<tab>" . company-complete)
;;        ("<backtab>" . company-select-previous))
;;  :config
;;  ;; Customize appearance
;;  (setq company-tooltip-align-annotations t
;;        company-tooltip-flip-when-above t))

;;;;----------------------------------------------------------------------------
;;;; yasnippet
;;;;----------------------------------------------------------------------------
;;(use-package yasnippet
;;  :ensure t
;;  :config
;;  (setq yas-verbosity 1                      ;; Reduce verbosity
;;        yas-wrap-around-region t)            ;; Enable wrapping around region
;;  (with-eval-after-load 'yasnippet
;;    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))  ;; Set snippet directories
;;  (yas-reload-all)                           ;; Reload all snippets
;;  (yas-global-mode))                        ;; Enable yasnippet globally
;;
;;(use-package yasnippet-snippets
;;  :ensure t)
;;
;;;; Custom Snippet Directory:
;;;; (add-to-list 'yas-snippet-dirs "/Users/MedinaJA/.emacs.d/snippets")
;;
;;;; (add-hook 'python-mode-hook #'yas-minor-mode)
;;
;;(with-eval-after-load 'yasnippet
;;  (define-key yas-minor-mode-map (kbd "TAB") #'yas-expand))

;;;;----------------------------------------------------------------------------
;;;; Magit
;;;;----------------------------------------------------------------------------
;;;; Magit Setup
;;(use-package magit
;;  :ensure t
;;  :bind (("C-x g" . magit-status)
;;         ("C-c C-g l" . magit-file-log)
;;         ("C-c C-g s" . magit-status)
;;         ("C-c C-g c" . magit-commit)
;;         ("C-c C-g p" . magit-push)
;;         ("C-c C-g f" . magit-fetch))
;;  :config
;;  (setq magit-diff-refine-hunk 'all)   ;; Highlight all changes in a hunk
;;  (setq magit-commit-arguments '("--no-edit"))  ;; Default commit arguments
;;  ;; Bind `magit-dispatch` to `C-x M-g` for additional command access
;;  (bind-key "C-x M-g" 'magit-dispatch magit-mode-map))
;;
;;;; Optional Extensions
;;(use-package forge
;;  :ensure t
;;  :after magit)
;;
;;(use-package magit-delta
;;  :ensure t
;;  :after magit
;;  :config
;;  (magit-delta-mode))

;;----------------------------------------------------------------------------
;; diff-hl
;;----------------------------------------------------------------------------
;; Diff-hl Setup
(use-package diff-hl
  :ensure t
  :init
  ;; Highlight changes to the current file in the fringe
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'org-mode-hook #'diff-hl-mode)  ;; For org-mode
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; Update highlighting with Magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  ;; Use margin mode if the fringe is unavailable
  (diff-hl-margin-mode)
  ;; Customize appearance of diff indicators
  (custom-set-faces
   '(diff-hl-change ((t (:background "light blue" :foreground "blue"))))
   '(diff-hl-delete ((t (:background "light coral" :foreground "red"))))
   '(diff-hl-insert ((t (:background "light green" :foreground "green"))))))

;;----------------------------------------------------------------------------
;; popwin
;;----------------------------------------------------------------------------
(use-package popwin
  :ensure t
  :config
  (progn
    ;; Buffer configurations
    (push '("*Completions*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Messages*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*magit-commit*" :position bottom :height .3) popwin:special-display-config)
    (push '("COMMIT_EDITMSG" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*magit-diff*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*magit-edit-log*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*magit-process*" :position bottom :height .3) popwin:special-display-config)
    (push '("*shell*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*Flycheck errors*" :position bottom :height .3) popwin:special-display-config)
    (push '("*company-documentation*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*Org Select*" :position bottom :height .3) popwin:special-display-config)
    ;; (push '("*compilation*" :position right :width 80 :noselect t) popwin:special-display-config)
    (push '("*Calendar*" :position bottom :height .3) popwin:special-display-config)
    (push '("*undo-tree Diff*" :position bottom :height .3) popwin:special-display-config)

    ;; Activate popwin
    (popwin-mode 1)))

;;;;----------------------------------------------------------------------------
;;;; flycheck
;;;;----------------------------------------------------------------------------
;;(use-package flycheck
;;  :ensure t
;;  :defer t
;;  :hook ((prog-mode . flycheck-mode) ;; Enable Flycheck for all programming modes
;;         (ess-r-mode . flycheck-mode)) ;; Specifically for ESS R mode
;;  :custom
;;  (flycheck-check-syntax-automatically '(save mode-enable)) ;; Check on save and mode enable
;;  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)) ;; Customize error display
;;
;;;; Optionally add Flycheck Popup Tip for improved error display
;;(use-package flycheck-popup-tip
;;  :ensure t
;;  :after flycheck
;;  :config
;;  (flycheck-popup-tip-mode))
;;
;;;; Optionally integrate with lsp-mode
;;(use-package lsp-mode
;;  :ensure t
;;  :hook ((lsp-mode . flycheck-mode))
;;  :config
;;  (setq lsp-enable-symbol-highlighting nil))

;;----------------------------------------------------------------------------
;; Fix word - upcase - downcase region
;;----------------------------------------------------------------------------
(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)      ;; Uppercase the word or region
         ("M-l" . fix-word-downcase)   ;; Lowercase the word or region
         ("M-c" . fix-word-capitalize))) ;; Capitalize the word or region

;;;;----------------------------------------------------------------------------
;;;; Dictionary Setup
;;;;----------------------------------------------------------------------------
;;
;;;; brew install hunspell
;;
;;;; check dictionaries here:
;;;; https://extensions.libreoffice.org/en/extensions/show/english-dictionaries
;;
;;;; cd ~/Library/Spelling
;;;; make the directory if not available
;;;; wget https://extensions.libreoffice.org/assets/downloads/41/1722502287/dict-en-20240801_lo.oxt
;;;; unzip dict-en-20240801_lo.oxt
;;
;;(setenv "LANG" "en_US")  ;; Adjust as needed
;;;; path below is after you installed hunspell
;;(setq-default ispell-program-name "/usr/local/bin/hunspell")
;;
;;(with-eval-after-load "ispell"
;;  (setq ispell-really-hunspell t)
;;  (setq ispell-program-name "hunspell")
;;  (setq ispell-dictionary "en_US")
;;  (ispell-set-spellchecker-params)
;;  (ispell-hunspell-add-multi-dic "en_US"))
;;
;;;; Spell checking should now work with M-x ispell
;;
;;;; Enable flyspell in more modes if needed
;;(dolist (hook '(text-mode-hook
;;                org-mode-hook
;;                prog-mode-hook))  ;; Add other hooks as needed
;;  (add-hook hook 'flyspell-mode))
;;
;;;; Customize flyspell mouse bindings
;;(eval-after-load "flyspell"
;;  '(progn
;;     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;----------------------------------------------------------------------------
;; Undo-tree
;;----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-visibility t) ; Additional option
  :bind
  (("C-x u" . undo-tree-visualize)    ; Key binding to open visualizer
   ("C-x r" . undo-tree-redo)))       ; Key binding for redo (example)

;;----------------------------------------------------------------------------
;; Which-key
;;----------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order)
  (which-key-allow-imprecise-window-fit t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  ;; Conditionally enable which-key based on `ivy-mode`
  (defun my/conditionally-enable-which-key ()
    "Enable which-key only if Ivy is not active."
    (unless (bound-and-true-p ivy-mode)
      (which-key-mode)))
  (add-hook 'emacs-startup-hook 'my/conditionally-enable-which-key))

;;----------------------------------------------------------------------------
;; Rainbow delimiters
;;----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :demand
  :init
  (progn
    ;; Enable in specific programming modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ;; Optional: Customize delimiter colors
    (custom-set-faces
     '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff5555"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "#50fa7b"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffb86c"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff79c6"))))
     ;; Add more depth faces as needed
    )))

;; ;;----------------------------------------------------------------------------
;; ;; Rainbow mode
;; ;;----------------------------------------------------------------------------
;; ;; Show Hex Color Codes
;; (use-package rainbow-mode
;;   :ensure t
;;   :commands rainbow-mode
;;   :hook (
;; 	 (prog-mode . rainbow-mode)
;; 	 (css-mode . rainbow-mode)
;; 	 (html-mode . rainbow-mode))
;;   :custom
;;   (rainbow-mode t))

;;----------------------------------------------------------------------------
;; Highlight-Indentation
;;----------------------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character-face ((t (:foreground "#bd93f9"))))
  (highlight-indent-guides-top-character-face ((t (:foreground "#ff5555" :weight bold)))))

;;----------------------------------------------------------------------------
;; Beacon
;;----------------------------------------------------------------------------
;; Highlight cursor when jumping window/buffer
(use-package beacon
  :ensure t
  :commands beacon-mode
  :init
  (beacon-mode 1)
  :config
  ;; Only flash on window/buffer changes...
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-window-scrolls nil)  ;; Disable scrolling blink
  (setq beacon-blink-when-point-moves t)
  (setq beacon-blink-duration 0.3)  ;; Increase blink duration
  (setq beacon-blink-delay 0.2)
  (setq beacon-size 30)  ;; Increase beacon size
  (custom-set-faces
   '(beacon ((t (:background "#ff5555" :foreground "#ffffff")))))  ;; Customize color
  )

;; Optionally, toggle beacon mode with a keybinding
(global-set-key (kbd "C-c b") #'beacon-mode)

;;;;----------------------------------------------------------------------------
;;;; Org-mode
;;;;----------------------------------------------------------------------------
;;;; Define keybinding for org-agenda
;;(define-key global-map "\C-ca" 'org-agenda)
;;
;;;; Define agenda files
;;(setq org-agenda-files (directory-files-recursively "~/GDrive_Personal/org/" "\\.org$"))
;;
;;;; Automatically add time when a TODO is done
;;(setq org-log-done 'time)
;;
;;;; Define TODO states
;;(setq org-todo-keywords
;;      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
;;
;;;; Function to skip subtrees with specific priority
;;(defun air-org-skip-subtree-if-priority (priority)
;;  "Skip an agenda subtree if it has a priority of PRIORITY.
;;
;;PRIORITY may be one of the characters ?A, ?B, or ?C."
;;  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;;        (pri-value (* 1000 (- org-lowest-priority priority)))
;;        (pri-current (org-get-priority (thing-at-point 'line t))))
;;    (if (= pri-value pri-current)
;;        subtree-end
;;      nil)))
;;
;;;; Function to skip subtrees with a STYLE property of "habit"
;;(defun air-org-skip-subtree-if-habit ()
;;  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
;;  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;    (if (string= (org-entry-get nil "STYLE") "habit")
;;        subtree-end
;;      nil)))
;;
;;;; Define custom agenda views
;;(setq org-agenda-custom-commands
;;      '(("d" "Daily agenda and all TODOs"
;;         ((tags "PRIORITY=\"A\""
;;                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
;;          (agenda "" ((org-agenda-ndays 1)))
;;          (alltodo ""
;;                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
;;                                                   (air-org-skip-subtree-if-priority ?A)
;;                                                   (org-agenda-skip-if nil '(scheduled deadline))))
;;                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
;;         ((org-agenda-compact-blocks nil))))) ;; Change to t if you want to remove the equal divisions

;;;;----------------------------------------------------------------------------
;;;; Org-journal
;;;;----------------------------------------------------------------------------
;;(use-package org-journal
;;  :ensure t
;;  :defer t
;;  :init
;;  ;; Change default prefix key; needs to be set before loading org-journal
;;  (setq org-journal-prefix-key "C-c j ")
;;  :config
;;  (setq org-journal-dir "~/GDrive_Personal/org/journal/"
;;        org-journal-date-format "%A, %d %B %Y"
;;	org-journal-file-format "%Y-%m-%d.org"
;;	org-journal-find-file 'find-file
;;	org-journal-file-type 'weekly
;;	org-journal-enable-agenda-integration 't)
;;  :bind
;;  (("C-c C-j" . org-journal-new-entry)
;;   ("C-c C-s" . org-journal-search)))
;;
;;;;----------------------------------------------------------------------------
;;;; Org-bullets
;;;;----------------------------------------------------------------------------
;;(use-package org-bullets
;;  :ensure t
;;  :hook
;;  (org-mode . (lambda () (org-bullets-mode 1)))
;;  :custom
;;  (org-bullets-bullet-list '("◉" "○" "✸" "✿"))) ;; Customize bullet symbols as desired

;;;;----------------------------------------------------------------------------
;;;; Additional Configurations
;;;;----------------------------------------------------------------------------
;;;; Set up Org Babel for code block execution
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((emacs-lisp . t)
;;   (python . t)
;;   (shell . t)
;;   (sql . t)))
;;
;;;; Use `org-super-agenda` if installed for more advanced agenda views
;;;; (use-package org-super-agenda
;;;;   :ensure t
;;;;   :config
;;;;   (org-super-agenda-mode))
;;
;;;; Adjust Org mode appearance and behavior
;;(setq org-hide-emphasis-markers t) ; Hide markup characters (e.g., *bold*)
;;(setq org-startup-indented t)      ; Indent headings
;;(setq org-ellipsis " ▾")           ; Customize ellipsis symbol
;;
;;;; Make sure flyspell mode is off in Org mode, as it can conflict with spell-checking
;;(dolist (hook '(text-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode -1))))
;;
;;;; Set up spell checking with Hunspell
;;(setq-default ispell-program-name "/usr/local/bin/hunspell")
;;(with-eval-after-load "ispell"
;;  (setq ispell-really-hunspell t)
;;  (setq ispell-dictionary "en_US")
;;  (ispell-set-spellchecker-params)
;;  (ispell-hunspell-add-multi-dic "en_US"))
;;
;;;; Define keybindings for Org mode
;;(define-key global-map (kbd "C-c a") 'org-agenda) ; Use C-c a for org-agenda

;;;;----------------------------------------------------------------------------
;;;; ESS (Emacs Speaks Statistics)
;;;;----------------------------------------------------------------------------
;;(use-package ess
;;  :ensure t
;;  :defer t
;;  :init
;;  (require 'ess-site)
;;  ;; Configure hooks for ESS
;;  (add-hook 'ess-r-mode-hook
;;            (lambda () (flycheck-mode t)))
;;  (add-hook 'ess-mode-hook
;;            (lambda ()
;;              (outline-minor-mode)
;;              (setq outline-regexp "^#.*----")
;;              (defun outline-level ()
;;                (cond ((looking-at "^#.*----") 1)
;;                      (t 1000)))
;;              (defun send-section-to-R ()
;;                (interactive)
;;                (let ((beg))
;;                  (if (outline-on-heading-p)
;;                      (beginning-of-line)
;;                    (outline-previous-visible-heading 1))
;;                  (setq beg (point))
;;                  (set-mark (point))
;;                  (outline-next-visible-heading 1)
;;                  (previous-line 1)
;;                  (end-of-line 1)
;;                  (ess-eval-region-or-function-or-paragraph-and-step)))
;;              (local-set-key (kbd "C-c h") 'outline-hide-body)
;;              (local-set-key (kbd "C-c s") 'outline-show-all)
;;              (local-set-key (kbd "C-c <left>") 'outline-hide-entry)
;;              (local-set-key (kbd "C-c <right>") 'outline-show-entry)
;;              (local-set-key (kbd "C-c <up>") 'outline-previous-heading)
;;              (local-set-key (kbd "C-c <down>") 'outline-next-heading)
;;              (local-set-key (kbd "C-c e") 'send-section-to-R)))
;;  :config
;;  ;; Key bindings
;;  (define-key ess-r-mode-map ";" #'ess-insert-assign)
;;  (define-key inferior-ess-r-mode-map ";" #'ess-insert-assign)
;;  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
;;
;;  ;; General settings
;;  (setq ess-use-eldoc 'script-only)
;;  (setq ess-history-directory "~/.cache")
;;  (setq inferior-ess-r-program "R")
;;  (setq ess-eval-visibly t)
;;  (setq ess-use-flymake nil) ;; disable Flymake
;;  (setq ess-use-auto-complete nil)
;;  (setq ess-use-company t)
;;  (setq ess-fancy-comments nil)
;;
;;  ;; Syntax highlighting settings
;;  (setq ess-R-font-lock-keywords
;;        '((ess-R-fl-keyword:keywords . t)
;;          (ess-R-fl-keyword:constants . t)
;;          (ess-R-fl-keyword:modifiers . t)
;;          (ess-R-fl-keyword:fun-defs . t)
;;          (ess-R-fl-keyword:assign-ops . t)
;;          (ess-R-fl-keyword:%op% . t)
;;          (ess-fl-keyword:fun-calls . t)
;;          (ess-fl-keyword:numbers . t)
;;          (ess-fl-keyword:operators . t)
;;          (ess-fl-keyword:delimiters . t)
;;          (ess-fl-keyword:= . t)
;;          (ess-R-fl-keyword:F&T . t)))
;;  (setq inferior-ess-r-font-lock-keywords
;;        '((ess-S-fl-keyword:prompt . t)
;;          (ess-R-fl-keyword:keywords . t)
;;          (ess-R-fl-keyword:constants . t)
;;          (ess-R-fl-keyword:modifiers . t)
;;          (ess-R-fl-keyword:messages . t)
;;          (ess-R-fl-keyword:fun-defs . t)
;;          (ess-R-fl-keyword:assign-ops . t)
;;          (ess-fl-keyword:matrix-labels . t)
;;          (ess-fl-keyword:fun-calls . t)
;;          (ess-fl-keyword:numbers . t)
;;          (ess-fl-keyword:operators . t)
;;          (ess-fl-keyword:delimiters . t)
;;          (ess-fl-keyword:= . t)
;;          (ess-R-fl-keyword:F&T . t)))
;;
;;  ;; Ensure locale is set
;;  (unless (getenv "LC_ALL") (setenv "LC_ALL" "en_US.UTF-8"))
;;
;;  ;; Customize ESS behavior
;;  (eval-after-load "ess-r-mode"
;;    '(progn
;;       (define-key ess-r-mode-map [(control return)] nil)
;;       (define-key ess-r-mode-map [(shift return)]
;;         'ess-eval-region-or-line-visibly-and-step)))
;;
;;  ;; Custom R pipe operator
;;  (defun then_R_operator ()
;;    "R - %>% operator or 'then' pipe operator"
;;    (interactive)
;;    (just-one-space 1)
;;    (insert "%>%")
;;    (reindent-then-newline-and-indent))
;;  (define-key ess-mode-map (kbd "C->") 'then_R_operator)
;;  (define-key inferior-ess-mode-map (kbd "C->") 'then_R_operator))
;;
;;;; ESS Configuration
;;;;(kill-buffer "*ESS*") ;; Uncomment if you want to kill any existing ESS buffers on startup
;;
;;(defun then_R_operator ()
;;  "Insert the R pipe operator '%>%', or the 'then' pipe operator.
;;   Automatically adds a space before the operator and indents the line."
;;  (interactive)
;;  (just-one-space 1)
;;  (insert "%>%")
;;  (reindent-then-newline-and-indent))
;;
;;;; Bind 'then_R_operator' to 'C->' in ESS modes
;;(define-key ess-mode-map (kbd "C->") 'then_R_operator)
;;(define-key inferior-ess-mode-map (kbd "C->") 'then_R_operator)
;;
;;;; Optional configuration for electric-operator
;;;; Uncomment to enable automatic spacing around operators
;;;; (use-package electric-operator
;;;;   :ensure t
;;;;   :after ess
;;;;   :hook ((ess-r-mode inferior-ess-r-mode) . electric-operator-mode)
;;;;   :custom
;;;;   (electric-operator-R-named-argument-style 'spaced)
;;;;   (electric-operator-add-rules-for-mode 'ess-r-mode
;;;;                                         (cons "*" nil)
;;;;                                         (cons "in" nil)))

;;;;----------------------------------------------------------------------------
;;;; lsp-mode
;;;;----------------------------------------------------------------------------
;;(use-package lsp-mode
;;  :ensure t
;;  :hook (
;;         ;; Enable LSP for ESS R mode and LaTeX mode
;;         ((ess-r-mode LaTeX-mode) . lsp)
;;         ;; Enable LSP which-key integration
;;         (lsp-mode . lsp-enable-which-key-integration)
;;         )
;;  :commands lsp
;;  :init
;;  (setq lsp-keymap-prefix "C-c l"               ;; Keymap prefix for LSP commands
;;        lsp-auto-guess-root nil                 ;; Do not auto-guess project root
;;        lsp-eldoc-enable-hover t                ;; Enable hover documentation
;;        lsp-enable-symbol-highlighting t        ;; Highlight symbols in the code
;;        lsp-enable-snippet t                    ;; Enable snippet completion
;;        lsp-file-watch-threshold nil            ;; No file watch threshold
;;        lsp-idle-delay 0.5                      ;; Idle delay before triggering LSP
;;        lsp-signature-render-documentation nil ;; Do not render documentation in signature help
;;        lsp-diagnostics-provider 'flycheck      ;; Use Flycheck for diagnostics
;;        lsp-prefer-flymake nil                  ;; Prefer Flycheck over Flymake
;;        lsp-signature-auto-activate t           ;; Automatically activate signature help
;;        lsp-completion-show-detail t            ;; Show details in completions
;;        lsp-completion-show-kind nil            ;; Do not show completion kind
;;        lsp-modeline-code-actions-enable nil    ;; Disable modeline code actions
;;        lsp-lens-enable nil                     ;; Disable lens features
;;        lsp-response-timeout 20                 ;; Set response timeout
;;        lsp-headerline-breadcrumb-enable nil)   ;; Disable headerline breadcrumbs
;;  ;; Adjust settings for macOS
;;  (when (string= system-type "darwin")
;;    (setq dired-use-ls-dired nil))
;;  (when (boundp 'read-process-output-max)
;;    (setq read-process-output-max (* 1024 1024))) ;; Increase process output max for Emacs 27+
;;  (setq lsp-log-io nil)) ;; Disable LSP I/O logging to avoid performance issues
;;
;;(use-package lsp-ui
;;  :ensure t
;;  :init
;;  (setq lsp-ui-doc-enable nil              ;; Disable documentation popup
;;        lsp-ui-sideline-show-code-actions nil ;; Disable sideline code actions
;;        lsp-ui-sideline-enable nil          ;; Disable sideline display
;;        lsp-ui-doc-show-with-cursor nil     ;; Do not show documentation with cursor
;;        lsp-ui-doc-show-with-mouse nil))    ;; Do not show documentation with mouse

;;;;----------------------------------------------------------------------------
;;;; Markdown-mode
;;;;----------------------------------------------------------------------------
;;(use-package markdown-mode
;;  :ensure t
;;  :defer t
;;  :mode (("\\.markdown\\'" . markdown-mode)
;;         ("\\.md\\'" . markdown-mode))
;;  :init
;;  ;; Set the markdown command to use for converting markdown files
;;  (setq markdown-command "markdown"))

;;;;----------------------------------------------------------------------------
;;;; Polymode - Poly R
;;;;----------------------------------------------------------------------------
;;(use-package poly-R
;;  :ensure t
;;  :defer t
;;  :mode (("\\.Rnw\\'" . poly-noweb+r-mode)
;;         ("\\.Rmd\\'" . poly-markdown+r-mode)
;;         ("\\.Snw\\'" . poly-noweb+r-mode)
;;         ("\\.rmd\\'" . poly-markdown+r-mode))
;;  :config
;;  ;; Wrap lines at column limit, but don't put hard returns in
;;  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
;;  ;; Enable Flyspell in markdown mode
;;  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))

;;;;----------------------------------------------------------------------------
;;;; Insert new chunk for Rmarkdown
;;;;----------------------------------------------------------------------------
;;(defun tws-insert-r-chunk (header)
;;  "Insert an R chunk in markdown mode. Necessary due to interactions between Polymode and yasnippet."
;;  (interactive "sHeader: ")
;;  (insert (concat "```{r " header "}\n\n```"))
;;  (forward-line -1))
;;
;;(global-set-key (kbd "C-c c") 'tws-insert-r-chunk)

;;;;----------------------------------------------------------------------------
;;;; Polymode Exporter Output File Format
;;;;----------------------------------------------------------------------------
;;(defcustom polymode-exporter-output-file-format "%s"
;;  "Format of the exported files.
;;%s is substituted with the current file name sans extension."
;;  :group 'polymode-export
;;  :type 'string)

;;;;----------------------------------------------------------------------------
;;;; Quarto Mode
;;;;----------------------------------------------------------------------------
;;(use-package quarto-mode
;;  :ensure t
;;  :defer t
;;  :mode (("\\.qmd\\'" . quarto-mode)
;;         ("\\.qmd\\'" . quarto-mode))  ;; Ensure qmd files open in quarto-mode
;;  :hook
;;  (quarto-mode . (lambda ()
;;                   (setq-local compile-command "quarto render %f")))
;;  :config
;;  ;; Optional: Set up custom key bindings
;;  (define-key quarto-mode-map (kbd "C-c C-r") 'quarto-render)
;;  (define-key quarto-mode-map (kbd "C-c C-p") 'quarto-preview)
;;  (define-key quarto-mode-map (kbd "C-c C-b") 'quarto-build)
;;  (define-key quarto-mode-map (kbd "C-c C-c") 'quarto-compile)
;;
;;  ;; Optional: Customize the rendering process
;;  (setq quarto-render-command "quarto render %s")
;;
;;  ;; Optional: Define how you want to preview documents
;;  (setq quarto-preview-command "quarto preview %s"))
;;
;;;; Optionally, set up Quarto command line tool integration
;;(when (executable-find "quarto")
;;  (setq quarto-command "quarto"))
;;
;;;; Optionally, you might want to add Quarto directory to your exec path
;;;; (add-to-list 'exec-path "/path/to/quarto")

;;;;----------------------------------------------------------------------------
;;;; Stan Mode
;;;;----------------------------------------------------------------------------
;;(use-package stan-mode
;;  :ensure t
;;  ;; Uncomment if directly loading from your development repo
;;  ;; :load-path "your-path/stan-mode/stan-mode"
;;  :mode ("\\.stan\\'" . stan-mode)
;;  :hook (stan-mode . stan-mode-setup)
;;  :config
;;  ;; Set the indentation offset for Stan code
;;  (setq stan-indentation-offset 2))
;;
;;;;----------------------------------------------------------------------------
;;;; Company-Stan
;;;;----------------------------------------------------------------------------
;;(use-package company-stan
;;  :ensure t
;;  ;; Uncomment if directly loading from your development repo
;;  ;; :load-path "your-path/stan-mode/company-stan/"
;;  :hook (stan-mode . company-stan-setup)
;;  :config
;;  ;; Whether to use fuzzy matching in company-stan
;;  (setq company-stan-fuzzy nil))
;;
;;;;----------------------------------------------------------------------------
;;;; Flycheck-Stan
;;;;----------------------------------------------------------------------------
;;(use-package flycheck-stan
;;  :ensure t
;;  :hook ((stan-mode . flycheck-stan-stanc2-setup)
;;         (stan-mode . flycheck-stan-stanc3-setup))
;;  :config
;;  ;; Path to the `stanc2` executable, defaults to `stanc2` if nil
;;  (setq flycheck-stanc-executable nil)
;;  ;; Path to the `stanc3` executable, defaults to `stanc3` if nil
;;  (setq flycheck-stanc3-executable nil))

;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------

(provide 'init) ;; Optional: Provides the 'init' feature
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(beacon highlight-indent-guides rainbow-delimiters which-key undo-tree fix-word popwin diff-hl all-the-icons-ibuffer all-the-icons-dired all-the-icons-ivy-rich all-the-icons-ivy smex ivy-rich counsel doom-modeline smartparens dimmer all-the-icons exec-path-from-shell goto-line-preview osx-trash switch-window transpose-frame dired-k dired-hide-dotfiles dired-subtree doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon ((t (:background "#ff5555" :foreground "#ffffff"))))
 '(diff-hl-change ((t (:background "light blue" :foreground "blue"))))
 '(diff-hl-delete ((t (:background "light coral" :foreground "red"))))
 '(diff-hl-insert ((t (:background "light green" :foreground "green"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff5555"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#50fa7b"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffb86c"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff79c6")))))
