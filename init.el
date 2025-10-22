;;; package --- Summary
;;; init.el ---

;;; Commentary:
;; This configuration sets up Emacs for terminal use with basic package management.
;; It installs and configures 'use-package' to handle further package installations.
;;
;; Additional Setup:
;; - To use C-SPC as set-mark-command on macOS, modify at the macOS level:
;;   System Preferences > Keyboard > Shortcuts > Input Sources >
;;   Select the previous input source and uncheck it.
;;
;; Inspired by https://github.com/MatthewZMD/.emacs.d#org9bf5ed1

;;; Code:

;;----------------------------------------------------------------------------
;; Performance Optimization
;;----------------------------------------------------------------------------
;; Set initial major mode to text-mode for faster startup
(setq initial-major-mode 'text-mode)

;; Increase garbage collection threshold during startup for better performance
;; This reduces the frequency of garbage collection during initialization
(setq gc-cons-threshold 100000000)

;;----------------------------------------------------------------------------
;; Package Management Setup
;;----------------------------------------------------------------------------
;; Initialize Emacs package system
(require 'package)

;; Set package archives - MELPA provides a wide range of community packages
;; GNU ELPA is the official GNU package repository
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; Uncomment the following lines for alternative mirrors
                         ;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
                         ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ))

;; Initialize package sources
;; Note: package-initialize is called automatically in Emacs 27+
;; (package-initialize) ;; Uncomment if using Emacs 26 or earlier

;; Install 'use-package' if not already installed
;; use-package simplifies package configuration and ensures packages are installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load and configure 'use-package' for managing other packages
(eval-when-compile
  (require 'use-package))

;;----------------------------------------------------------------------------
;; Personal Information
;;----------------------------------------------------------------------------
;; Set user identification for version control and other features
(setq user-full-name "J.A. Medina-Vega")
(setq user-mail-address "jamedina09@gmail.com")

;;----------------------------------------------------------------------------
;; Theme: doom-themes
;;----------------------------------------------------------------------------
;; Install and configure doom-themes for modern color schemes
;; Doom themes work well in both GUI and terminal
(use-package doom-themes
  :ensure t
  :config
  ;; Global theme settings
  (setq doom-themes-enable-bold t     ;; Enable bold fonts
        doom-themes-enable-italic t   ;; Enable italic fonts
        doom-themes-treemacs-theme "doom-colors")

  ;; Load Doom Dracula theme - dark theme that works well in terminal
  (load-theme 'doom-dracula t)

  ;; Improve org-mode's native fontification
  (doom-themes-org-config))

;;----------------------------------------------------------------------------
;; Clean Startup Buffers
;;----------------------------------------------------------------------------
;; Make *scratch* buffer empty on startup
(setq initial-scratch-message nil)

;; Disable message logging to reduce clutter
;; This prevents the *Messages* buffer from accumulating logs
(setq-default message-log-max nil)

;; Kill the *Messages* buffer at startup
(kill-buffer "*Messages*")

;;----------------------------------------------------------------------------
;; Interface and General Tweaks
;;----------------------------------------------------------------------------

;; GUI transparency setting - NOT NEEDED FOR TERMINAL EMACS
;; Commented out as transparency doesn't apply to terminal
;; (defun on-after-init ()
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "unspecified-bg" (selected-frame))))
;; (add-hook 'window-setup-hook 'on-after-init)

;; Set starting directory to user's home directory
(cd (getenv "HOME"))
(message "Current dir: %s" (pwd))
(message "Current buffer: %s" (buffer-name))

;;----------------------------------------------------------------------------
;; UI Elements Configuration
;;----------------------------------------------------------------------------
;; Disable menu bar - works in terminal
(menu-bar-mode -1)

;; GUI-ONLY FEATURES - Commented out for terminal use
;; Tool bar and scroll bar don't exist in terminal Emacs
;; (tool-bar-mode -1)         ;; Disable tool bar (GUI only)
;; (set-scroll-bar-mode nil)  ;; Disable scroll bar (GUI only)

;;----------------------------------------------------------------------------
;; Character Encoding
;;----------------------------------------------------------------------------
;; Set UTF-8 as the default encoding system for all operations
;; This ensures proper display of international characters
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;----------------------------------------------------------------------------
;; Window Size Configuration
;;----------------------------------------------------------------------------
;; GUI-ONLY - Window size settings don't apply to terminal
;; (add-to-list 'default-frame-alist '(height . 50))
;; (add-to-list 'default-frame-alist '(width . 90))

;;----------------------------------------------------------------------------
;; User Experience Improvements
;;----------------------------------------------------------------------------
;; Use shorter yes/no prompts - type 'y' or 'n' instead of 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable region overwrite - when text is selected, typing replaces it
(delete-selection-mode t)

;; Highlight matching parentheses - helps with code readability
(show-paren-mode t)

;; Disable lockfiles - prevents creation of .# files
;; Lockfiles can cause issues with some version control systems
(setq create-lockfiles nil)

;;----------------------------------------------------------------------------
;; Visual Feedback
;;----------------------------------------------------------------------------
;; Enable visual bell instead of audio beep
(setq visible-bell t)
;; Completely disable the bell
(setq ring-bell-function 'ignore)

;; Suppress the startup screen to go directly to scratch buffer
(setq inhibit-startup-screen t)

;; Confirm before quitting Emacs to prevent accidental exits
(setq confirm-kill-emacs 'yes-or-no-p)

;; Customize the title bar to show user, system, and buffer name
(setq-default frame-title-format '("" user-login-name "@" system-name " - %b"))

;;----------------------------------------------------------------------------
;; Fringe Configuration
;;----------------------------------------------------------------------------
;; GUI-ONLY - Fringe doesn't exist in terminal Emacs
;; Fringe is the narrow vertical region at the edge of GUI windows
;; (fringe-mode 1)

;;----------------------------------------------------------------------------
;; File Saving Behavior
;;----------------------------------------------------------------------------
;; Automatically remove trailing whitespace before saving
;; Keeps files clean and prevents unnecessary diffs
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ensure files end with a newline character
;; This is a POSIX requirement and prevents issues with some tools
(setq require-final-newline t)

;;----------------------------------------------------------------------------
;; Text Display Settings
;;----------------------------------------------------------------------------
;; Set default line wrap at 80 characters
;; Standard width for code readability
(setq-default fill-column 80)

;; Enable line and column numbers in the mode line
;; Shows current position in the file
(setq column-number-mode t)
(setq line-number-mode t)

;; Show line numbers globally - works in terminal
;; display-line-numbers-mode is faster than the older linum-mode
(global-display-line-numbers-mode t)

;; Use relative line numbers - useful for vim-style navigation
;; Change to 't' for absolute line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers "%d ")

;; Highlight the current line - works in terminal
;; Makes it easy to see where the cursor is
(global-hl-line-mode t)

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; Unbind potentially disruptive keys
(global-set-key (kbd "C-z") nil)          ;; Disables suspend-frame (C-z)
(global-set-key (kbd "M-z") nil)          ;; Disables zap-to-char (M-z)
(global-set-key (kbd "M-m") nil)          ;; Disables back-to-indentation (M-m)
(global-set-key (kbd "C-x C-z") nil)      ;; Disables minimize window (C-x C-z)
(global-set-key (kbd "M-/") nil)          ;; Disables dabbrev-expand (M-/)

;; Font size adjustment - works in terminal (changes text scale)
;; Browser-style zoom controls
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; Paragraph navigation with arrow keys
;; Useful for moving through code blocks quickly
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "M-<up>") #'backward-paragraph)

;;----------------------------------------------------------------------------
;; Backup Configuration
;;----------------------------------------------------------------------------

;; Define backup directory within Emacs configuration
(defconst my-backup-dir
  (expand-file-name "backups" user-emacs-directory))

;; Create backup directory if it doesn't exist
(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir t))

;; Configure backup behavior
(setq make-backup-files t               ;; Enable backups
      version-control t                 ;; Enable versioning of backups
      backup-by-copying t               ;; Use copying to preserve symlinks
      backup-directory-alist `(("." . ,my-backup-dir)) ;; Store backups in defined directory
      kept-new-versions 6               ;; Number of newest versions to keep
      kept-old-versions 2               ;; Number of oldest versions to keep
      delete-old-versions t             ;; Automatically delete excess backups
      vc-make-backup-files t            ;; Backup files under version control
      auto-save-default nil)            ;; Disable auto-save (#file#)

;; Optional: Redirect backups to cloud storage (e.g., Google Drive)
;; This overrides the above backup-directory-alist setting
(setq backup-directory-alist '(("." . "~/GDrive_Personal/EMACS_BACKUPS/")))

;; Alternative auto-save configuration (commented out)
;; (setq auto-save-default t
;;      auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;;----------------------------------------------------------------------------
;; Time-stamp Configuration (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Automatically updates "Time-stamp: <>" in the first 10 lines when saving
;; Uncomment if you want automatic timestamp updates
;; (setq time-stamp-active t
;;       time-stamp-line-limit 10
;;       time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %L")
;; (add-hook 'write-file-functions 'time-stamp)

;;----------------------------------------------------------------------------
;; Ibuffer - Enhanced Buffer List
;;----------------------------------------------------------------------------
;; ibuffer is a better alternative to the default buffer list
(use-package ibuffer
  :ensure nil  ;; Built-in package
  :bind ("C-x C-b" . ibuffer)
  :custom
  ;; Configure column display in ibuffer
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

;; Group buffers by major mode or directory for better organization
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs" (mode . emacs-lisp-mode))
         ("Org" (mode . org-mode))
         ("Dired" (mode . dired-mode))
         ("Magit" (mode . magit-status-mode))
         ("Python" (mode . python-mode))
         ("Others" nil))))

;; Automatically apply filter groups when opening ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Customize ibuffer column widths
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

;;----------------------------------------------------------------------------
;; Python Configuration
;;----------------------------------------------------------------------------
;; Use TAB for completion instead of C-M-i
(setq tab-always-indent 'complete)
;; Prevent Python mode from guessing indentation
(setq python-indent-guess-indent-offset nil)

;;----------------------------------------------------------------------------
;; Dashboard (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Dashboard provides a nice startup screen with recent files, projects, etc.
;; Commented out for minimal terminal setup
;; Uncomment if you want a graphical dashboard
;;(use-package dashboard
;;  :ensure t
;;  :init
;;  (dashboard-setup-startup-hook)
;;  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;  (setq dashboard-banner-logo-title "")
;;  (setq dashboard-startup-banner 'official)
;;  (setq dashboard-center-content t)
;;  (setq dashboard-items '((recents . 10)
;;                          (projects . 5)
;;                          (bookmarks . 5)
;;                          (agenda . 5)))
;;  (setq dashboard-set-heading-icons t)
;;  (setq dashboard-set-file-icons t)
;;  (setq dashboard-set-footer nil)
;;  (setq dashboard-page-separator "\n\f\n"))

;;----------------------------------------------------------------------------
;; Page Break Lines (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Displays page breaks as horizontal lines
;; Works better in GUI; commented out for terminal
;;(use-package page-break-lines
;;  :ensure t
;;  :config
;;  (setq global-page-break-lines-mode t)
;;  (set-fontset-font "fontset-default"
;;                    (cons page-break-lines-char page-break-lines-char)
;;                    (face-attribute 'default :family)))

;; Custom faces for page break lines (if enabled)
;; (custom-set-faces
;;  '(page-break-lines
;;    ((t (:foreground "light gray" :background "dark gray" :weight bold)))))

;; Keybinding to toggle page-break-lines-mode (if package is installed)
(global-set-key (kbd "C-c p") 'page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Dired - Directory Editor
;;----------------------------------------------------------------------------
;; Enhanced configuration for Emacs file manager
(use-package dired
  :ensure nil  ;; Built-in package
  :bind
  (("C-x C-j" . dired-jump)              ;; Jump to current file in dired
   ("C-x j" . dired-jump-other-window))  ;; Jump to dired in other window
  :custom
  ;; Always operate recursively on directories
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto-refresh dired buffers when files change
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)              ;; Don't show messages when reverting
  ;; Guess target directory for copy/move operations
  ;; If two dired windows are open, use the other as default target
  (dired-dwim-target t)
  ;; Always load the newest version of a file
  (load-prefer-newer t)
  ;; Auto-refresh settings
  (auto-revert-use-notify nil)
  (auto-revert-interval 3)               ;; Auto revert every 3 seconds
  :config
  ;; Enable global auto-revert for all buffers
  (global-auto-revert-mode t)
  ;; Allow reusing the same dired buffer instead of creating new ones
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  ;; Use alternate file to avoid buffer proliferation
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  ;; Go up directory with ^
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

;;----------------------------------------------------------------------------
;; Dired Subtree
;;----------------------------------------------------------------------------
;; Allows expanding/collapsing directories in dired
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)    ;; Insert/expand subdirectory
             (";" . dired-subtree-remove)))  ;; Remove/collapse subdirectory

;;----------------------------------------------------------------------------
;; Dired Hide Dotfiles
;;----------------------------------------------------------------------------
;; Toggle visibility of hidden files (dotfiles) in dired
(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)  ;; Enable by default
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))  ;; Toggle with H

;;----------------------------------------------------------------------------
;; Dired K
;;----------------------------------------------------------------------------
;; Git-aware dired with file status indicators
(use-package dired-k
  :ensure t
  :defer t
  :init
  ;; Run dired-k when opening dired
  (add-hook 'dired-initial-position-hook 'dired-k)
  ;; Update dired-k display after operations
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;;----------------------------------------------------------------------------
;; Transpose Frame
;;----------------------------------------------------------------------------
;; Rotate and transpose window layouts
(use-package transpose-frame
  :ensure t
  :bind (("C-c t" . transpose-frame)     ;; Transpose windows
         ("C-c f" . rotate-frame)))      ;; Rotate window layout

;;----------------------------------------------------------------------------
;; Switch Window
;;----------------------------------------------------------------------------
;; Better window switching with visual indicators
;; Shows numbers/letters for each window
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)                      ;; Override default C-x o
         ("C-x w" . switch-window-then-swap-buffer)))   ;; Switch and swap

;;----------------------------------------------------------------------------
;; OSX Trash
;;----------------------------------------------------------------------------
;; Proper trash integration for macOS
;; Requires: brew install trash
(use-package osx-trash
  :ensure t
  :init
  ;; Only enable on macOS
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  ;; Move deleted files to trash instead of permanent deletion
  (setq delete-by-moving-to-trash t))

;;----------------------------------------------------------------------------
;; Goto Line Preview
;;----------------------------------------------------------------------------
;; Preview line while typing line number
;; Better than default goto-line
(use-package goto-line-preview
  :ensure t
  :config
  ;; Replace default goto-line with preview version
  (global-set-key [remap goto-line] 'goto-line-preview))

;;----------------------------------------------------------------------------
;; Exec Path From Shell
;;----------------------------------------------------------------------------
;; Import environment variables from shell
;; Essential for macOS to get correct PATH
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))  ;; Only on macOS/Linux GUI
  :config
  (exec-path-from-shell-initialize))

;;----------------------------------------------------------------------------
;; All The Icons (COMMENTED OUT - GUI ONLY)
;;----------------------------------------------------------------------------
;; Icons for Ivy completions - doesn't work well in terminal
;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :init
;;   (all-the-icons-ivy-setup))

;; (use-package all-the-icons-ivy-rich
;;   :ensure t
;;   :init
;;   (all-the-icons-ivy-rich-mode 1))

;; (use-package all-the-icons-dired
;;   :ensure t
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :init
;;   (all-the-icons-ibuffer-mode 1))

;;----------------------------------------------------------------------------
;; Company Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Auto-completion framework
;; Uncomment if you want text completion while typing
;;(use-package company
;;  :ensure t
;;  :init
;;  (global-company-mode)  ;; Enable globally
;;  :custom
;;  (company-minimum-prefix-length 2)        ;; Start completion after 2 chars
;;  (company-tooltip-limit 20)               ;; Show up to 20 candidates
;;  (company-idle-delay 0.2)                 ;; Delay before showing popup
;;  (company-tooltip-idle-delay 0.2)
;;  (company-backends '((company-files       ;; File path completion
;;                       company-keywords    ;; Language keywords
;;                       company-capf        ;; Completion-at-point
;;                       company-dabbrev-code  ;; Code completion
;;                       company-dabbrev)))  ;; Dynamic abbreviation
;;  :bind
;;  (:map company-active-map
;;        ("<tab>" . company-complete)
;;        ("<backtab>" . company-select-previous))
;;  :config
;;  (setq company-tooltip-align-annotations t
;;        company-tooltip-flip-when-above t))

;;----------------------------------------------------------------------------
;; YASnippet (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Snippet/template system
;; Uncomment if you want code templates
;;(use-package yasnippet
;;  :ensure t
;;  :config
;;  (setq yas-verbosity 1
;;        yas-wrap-around-region t)
;;  (with-eval-after-load 'yasnippet
;;    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
;;  (yas-reload-all)
;;  (yas-global-mode))
;;
;;(use-package yasnippet-snippets
;;  :ensure t)

;;----------------------------------------------------------------------------
;; Magit (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Git interface for Emacs
;; Uncomment if you want powerful Git integration
;;(use-package magit
;;  :ensure t
;;  :bind (("C-x g" . magit-status)
;;         ("C-c C-g l" . magit-file-log)
;;         ("C-c C-g s" . magit-status)
;;         ("C-c C-g c" . magit-commit)
;;         ("C-c C-g p" . magit-push)
;;         ("C-c C-g f" . magit-fetch))
;;  :config
;;  (setq magit-diff-refine-hunk 'all)
;;  (setq magit-commit-arguments '("--no-edit"))
;;  (bind-key "C-x M-g" 'magit-dispatch magit-mode-map))

;; Forge - Work with Git forges (GitHub, GitLab, etc.)
;;(use-package forge
;;  :ensure t
;;  :after magit)

;; Magit Delta - Better diff display
;;(use-package magit-delta
;;  :ensure t
;;  :after magit
;;  :config
;;  (magit-delta-mode))

;;----------------------------------------------------------------------------
;; Diff-HL - Highlight Version Control Changes
;;----------------------------------------------------------------------------
;; Shows git changes in the margin/fringe
;; Configured for terminal use (uses margin instead of fringe)
(use-package diff-hl
  :ensure t
  :init
  ;; Enable in programming modes
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'org-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; Update diff-hl when using magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  ;; TERMINAL: Use margin mode instead of fringe (fringe unavailable in terminal)
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  ;; Customize colors for change indicators
  (custom-set-faces
   '(diff-hl-change ((t (:background "light blue" :foreground "blue"))))
   '(diff-hl-delete ((t (:background "light coral" :foreground "red"))))
   '(diff-hl-insert ((t (:background "light green" :foreground "green"))))))

;;----------------------------------------------------------------------------
;; Popwin - Popup Window Manager
;;----------------------------------------------------------------------------
;; Manages special buffers in popup windows
;; Prevents help buffers and similar from taking over your layout
(use-package popwin
  :ensure t
  :config
  (progn
    ;; Configure buffers to open in popup windows at bottom
    (push '("*Completions*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Messages*" :position bottom :height .3) popwin:special-display-config)
    (push '("COMMIT_EDITMSG" :position bottom :height .3) popwin:special-display-config)
    (push '("*shell*" :position bottom :height .3) popwin:special-display-config)
    (push '("*company-documentation*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Calendar*" :position bottom :height .3) popwin:special-display-config)
    (push '("*undo-tree Diff*" :position bottom :height .3) popwin:special-display-config)

    ;; Activate popwin mode
    (popwin-mode 1)))

;;----------------------------------------------------------------------------
;; Flycheck (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; On-the-fly syntax checking
;; Uncomment if you want real-time error checking
;;(use-package flycheck
;;  :ensure t
;;  :defer t
;;  :hook ((prog-mode . flycheck-mode)
;;         (ess-r-mode . flycheck-mode))
;;  :custom
;;  (flycheck-check-syntax-automatically '(save mode-enable))
;;  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; Flycheck Popup Tip - Show errors in popup
;;(use-package flycheck-popup-tip
;;  :ensure t
;;  :after flycheck
;;  :config
;;  (flycheck-popup-tip-mode))

;;----------------------------------------------------------------------------
;; Fix Word - Case Conversion
;;----------------------------------------------------------------------------
;; Better word case conversion that works with regions
(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)       ;; Uppercase word/region
         ("M-l" . fix-word-downcase)     ;; Lowercase word/region
         ("M-c" . fix-word-capitalize))) ;; Capitalize word/region

;;----------------------------------------------------------------------------
;; Dictionary/Spell Checking (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Hunspell spell checker configuration
;; Requires: brew install hunspell
;; Uncomment if you want spell checking
;;(setenv "LANG" "en_US")
;;(setq-default ispell-program-name "/usr/local/bin/hunspell")
;;
;;(with-eval-after-load "ispell"
;;  (setq ispell-really-hunspell t)
;;  (setq ispell-program-name "hunspell")
;;  (setq ispell-dictionary "en_US")
;;  (ispell-set-spellchecker-params)
;;  (ispell-hunspell-add-multi-dic "en_US"))
;;
;; Enable flyspell in text modes
;;(dolist (hook '(text-mode-hook
;;                org-mode-hook
;;                prog-mode-hook))
;;  (add-hook hook 'flyspell-mode))

;;----------------------------------------------------------------------------
;; Undo Tree
;;----------------------------------------------------------------------------
;; Visual undo history with branching
;; Shows undo history as a tree structure
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)  ;; Enable globally
  :custom
  (undo-tree-visualizer-diff t)        ;; Show diffs in visualizer
  (undo-tree-visualizer-timestamps t)  ;; Show timestamps
  (undo-tree-visualizer-visibility t)  ;; Additional visibility option
  :bind
  (("C-x u" . undo-tree-visualize)     ;; Open visual undo tree
   ("C-x r" . undo-tree-redo)))        ;; Redo operation

;;----------------------------------------------------------------------------
;; Which-Key
;;----------------------------------------------------------------------------
;; Display available keybindings in popup
;; Shows possible key combinations after you start a command
(use-package which-key
  :ensure t
  :custom
  (which-key-separator " ")                    ;; Separator between key and description
  (which-key-prefix-prefix "+")                ;; Prefix indicator
  (which-key-sort-order 'which-key-key-order)  ;; Sort by key
  (which-key-allow-imprecise-window-fit t)     ;; Allow window resizing
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  ;; Conditionally enable which-key
  (defun my/conditionally-enable-which-key ()
    "Enable which-key only if Ivy is not active."
    (unless (bound-and-true-p ivy-mode)
      (which-key-mode)))
  (add-hook 'emacs-startup-hook 'my/conditionally-enable-which-key))

;;----------------------------------------------------------------------------
;; Rainbow Delimiters
;;----------------------------------------------------------------------------
;; Color-code matching parentheses/brackets by depth
;; Makes it easy to see nested structures
(use-package rainbow-delimiters
  :ensure t
  :demand
  :init
  (progn
    ;; Enable in programming modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ;; Customize colors for different nesting depths
    (custom-set-faces
     '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff5555"))))  ;; Red
     '(rainbow-delimiters-depth-2-face ((t (:foreground "#50fa7b"))))  ;; Green
     '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffb86c"))))  ;; Orange
     '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff79c6"))))  ;; Pink
     ;; Add more depths as needed
    )))

;;----------------------------------------------------------------------------
;; Rainbow Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Display color codes with their actual colors
;; Useful for CSS/HTML editing
;; (use-package rainbow-mode
;;   :ensure t
;;   :commands rainbow-mode
;;   :hook ((prog-mode . rainbow-mode)
;;          (css-mode . rainbow-mode)
;;          (html-mode . rainbow-mode))
;;   :custom
;;   (rainbow-mode t))

;;----------------------------------------------------------------------------
;; Highlight Indent Guides
;;----------------------------------------------------------------------------
;; Show vertical lines for indentation levels
;; Helps visualize code structure
(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-auto-enabled t)     ;; Auto-detect indentation
  (highlight-indent-guides-responsive t)       ;; Respond to cursor position
  (highlight-indent-guides-method 'character)  ;; Use characters for guides
  :hook
  (prog-mode . highlight-indent-guides-mode)   ;; Enable in programming modes
  :custom
  ;; Customize colors
  (highlight-indent-guides-character-face ((t (:foreground "#bd93f9"))))
  (highlight-indent-guides-top-character-face ((t (:foreground "#ff5555" :weight bold)))))

;;----------------------------------------------------------------------------
;; Beacon (COMMENTED OUT - MAY NOT WORK WELL IN TERMINAL)
;;----------------------------------------------------------------------------
;; Highlight cursor when switching windows/buffers
;; May not work properly in terminal Emacs
;; (use-package beacon
;;   :ensure t
;;   :commands beacon-mode
;;   :init
;;   (beacon-mode 1)
;;   :config
;;   (setq beacon-blink-when-window-changes t)
;;   (setq beacon-blink-when-window-scrolls nil)
;;   (setq beacon-blink-when-point-moves t)
;;   (setq beacon-blink-duration 0.3)
;;   (setq beacon-blink-delay 0.2)
;;   (setq beacon-size 30)
;;   (custom-set-faces
;;    '(beacon ((t (:background "#ff5555" :foreground "#ffffff"))))))
;; (global-set-key (kbd "C-c b") #'beacon-mode)

;;----------------------------------------------------------------------------
;; Org Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Comprehensive configuration for Org mode
;; Uncomment if you use Org for note-taking, task management, etc.
;;(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-agenda-files (directory-files-recursively "~/GDrive_Personal/org/" "\\.org$"))
;;(setq org-log-done 'time)
;;(setq org-todo-keywords
;;      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))

;;----------------------------------------------------------------------------
;; Org Journal (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Journal/diary functionality for Org mode
;;(use-package org-journal
;;  :ensure t
;;  :defer t
;;  :init
;;  (setq org-journal-prefix-key "C-c j ")
;;  :config
;;  (setq org-journal-dir "~/GDrive_Personal/org/journal/"
;;        org-journal-date-format "%A, %d %B %Y"
;;        org-journal-file-format "%Y-%m-%d.org"
;;        org-journal-find-file 'find-file
;;        org-journal-file-type 'weekly
;;        org-journal-enable-agenda-integration 't)
;;  :bind
;;  (("C-c C-j" . org-journal-new-entry)
;;   ("C-c C-s" . org-journal-search)))

;;----------------------------------------------------------------------------
;; Org Bullets (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Pretty bullets for Org headings
;;(use-package org-bullets
;;  :ensure t
;;  :hook (org-mode . (lambda () (org-bullets-mode 1)))
;;  :custom
;;  (org-bullets-bullet-list '("◉" "○" "✸" "✿")))

;;----------------------------------------------------------------------------
;; ESS - Emacs Speaks Statistics (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; R language support
;; Uncomment if you use R for statistical computing
;; See original config for full ESS configuration

;;----------------------------------------------------------------------------
;; LSP Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Language Server Protocol support
;; Provides IDE-like features for many languages
;; Uncomment if you need advanced language support
;; See original config for full LSP configuration

;;----------------------------------------------------------------------------
;; Markdown Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Markdown editing support
;;(use-package markdown-mode
;;  :ensure t
;;  :defer t
;;  :mode (("\\.markdown\\'" . markdown-mode)
;;         ("\\.md\\'" . markdown-mode))
;;  :init
;;  (setq markdown-command "markdown"))

;;----------------------------------------------------------------------------
;; Polymode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Support for files with multiple major modes (e.g., R Markdown)
;; See original config for Poly-R configuration

;;----------------------------------------------------------------------------
;; Quarto Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Support for Quarto documents
;; See original config for Quarto configuration

;;----------------------------------------------------------------------------
;; Stan Mode (COMMENTED OUT)
;;----------------------------------------------------------------------------
;; Support for Stan probabilistic programming language
;; See original config for Stan configuration

;;----------------------------------------------------------------------------
;; End of Configuration
;;----------------------------------------------------------------------------

(provide 'init) ;; Provide the 'init' feature
;;; init.el ends here

;;----------------------------------------------------------------------------
;; Custom Variables (Auto-generated by Emacs)
;;----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

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
