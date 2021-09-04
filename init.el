;;; package --- Summary
;;; init.el ---

;;; Commentary:
;; Download emacs: brew install --cask emacs
;;
;; To use c-spc as set-mark-command in mac you need to modify at
;; the mac os level:: System Preferences > Keyboard > Shortcuts >
;; Input Sources > Select the previous input source and uncheck
;;
;; I also used code from: https://github.com/MatthewZMD/.emacs.d#org9bf5ed1
;;; Code:


;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;;----------------------------------------------------------------------------
;; theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  ;;   Global settings (defaults)
  (setq doom-themes-enable-bold t;;  if nil, bold is universally disabled
        doom-themes-enable-italic t;;  if nil, italics is universally disabled
	doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-one t);; Iosvkem
  ;;   Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;----------------------------------------------------------------------------
;; Kill general login buffers
;;----------------------------------------------------------------------------
;; Makes *scratch* empty.
(setq initial-scratch-message "Type C-j to run the code")

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;----------------------------------------------------------------------------
;; Interface and General Tweaks
;;----------------------------------------------------------------------------
;; Define the home directory
(cd (getenv "HOME"))
(message "Current dir: %s" (pwd))
(message "Current buffer: %s" (buffer-name))

;; Coding systems
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Window size and features
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 90))

;; We don't want to type yes and no all the time so do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Overwrite region selected
(delete-selection-mode t)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

;; Don't Lock Files
(setq create-lockfiles nil)

;; Remove noise emacs
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Suppress GUI features
(setq inhibit-startup-screen t)

;; Confirm quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Title bar
(setq-default frame-title-format '("" user-login-name "@" system-name " - %b"))

;; Make the fringe narrower
;; make the left fringe 4 pixels wide and the right disappear
(fringe-mode '1)

;; Trim spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Wrap lines automatically
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

;; line and column number
(setq column-number-mode t)
(setq line-number-mode t)

;;----------------------------------------------------------------------------
;; Personal information
;;----------------------------------------------------------------------------
(setq user-full-name "J.A. Medina-Vega")
(setq user-mail-address "jamedina09@gmail.com")

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "M-<up>") #'backward-paragraph)

;;----------------------------------------------------------------------------
;; Backpups
;;----------------------------------------------------------------------------
(defconst my-backup-dir
  (expand-file-name (concat user-emacs-directory "backups")))

(setq make-backup-files t ;;make backup first time a file is saved
      version-control t ;; number and keep versions of backups
      backup-by-copying t ;; and copy (don't clobber symlinks) them to...
      backup-directory-alist '(("." . "~/GDrive_Personal/EMACS_BACKUPS/")) ;; ... here
      kept-new-versions 6 ;; the number of newest version to keep
      kept-old-versions 2 ;; the number of old versions to keep
      delete-old-versions t ;; don't ask about deleting old versions
      vc-make-backup-files t ;; even backup files under version control (git,svn,etc.)
      ;;make-backup-files nil  ;; no annoying "~file.txt"
      auto-save-default nil) ;; no auto saves to #file#

;;----------------------------------------------------------------------------
;; Time-stamp
;;----------------------------------------------------------------------------
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
(setq time-stamp-active t  ;; do enable time-stamp
      time-stamp-line-limit 10 ;; check first 10 buffer lines for Time-stamp: <>
      time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %L") ; date format
(add-hook 'write-file-functions 'time-stamp) ; update when saving

;;----------------------------------------------------------------------------
;; Use ibuffer instead of normal buffer
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

;;----------------------------------------------------------------------------
;; Dashboard
;;----------------------------------------------------------------------------
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Set the title
  (setq dashboard-banner-logo-title "")
  ;; Set the banner
  (setq dashboard-startup-banner 'official)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer;
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t);
  ;; To disable shortcut "jump" indicators for each section, set
  ;;(setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
			  ))
  ;; To add icons to the widget headings and their items:
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; A randomly selected footnote will be displayed. To disable it:
  (setq dashboard-set-footer nil)
  ;; horizontal lines
  (setq dashboard-page-separator "\n\f\n")
  )

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :ensure t
  :config
  (setq global-page-break-lines-mode t)
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

;;----------------------------------------------------------------------------
;; Dired
;;----------------------------------------------------------------------------
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

;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; make small subtrees within the same buffer
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; copy paste easy in dired :)
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;;----------------------------------------------------------------------------
;; dired-sidebar
;;----------------------------------------------------------------------------
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands
  (dired-sidebar-toggle-sidebar)
  )

;;----------------------------------------------------------------------------
;; transpose-frame
;;----------------------------------------------------------------------------
(use-package transpose-frame
  :ensure t
  :bind (("C-c t" . transpose-frame)
	 ("C-c f" . rotate-frame)))

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
  :ensure t)
(global-set-key [remap goto-line] 'goto-line-preview)

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
(setq inhibit-compacting-font-caches t)

;;----------------------------------------------------------------------------
;; Projectile
;;----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))
(setq projectile-sort-order 'recently-active)

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
;; Ace window
;;----------------------------------------------------------------------------
(use-package ace-window
  :ensure t
  :bind ("M-o" . shackra/other-window)
  :init
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  (defun --count-frames ()
    "Return the number of visible frames"
    (let* ((frames (if (daemonp) (butlast (visible-frame-list) 1) (visible-frame-list))))
      (length frames)))
  (defun shackra/other-window ()
    "Change the cursor's focus to another window"
    (interactive)
    (if (or (> (count-windows) 2) (> (--count-frames) 1))
        (ace-window 1)
      (ace-window 0)))
  :config
  (setf aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;----------------------------------------------------------------------------
;; Ivy, ivy rich and dependatns
;;----------------------------------------------------------------------------
(use-package counsel
  :after ivy
  :init
  (setq counsel-yank-pop-separator
	(concat "\n\n"
		(concat (apply 'concat (make-list 50 "---")) "\n")))
  :bind (
	 ("M-y" . counsel-yank-pop)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable))
  :config
  (use-package smex :ensure t))

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

(use-package ivy-rich
  :ensure t
  :after ivy
  :config (setq ivy-rich-path-style 'abbrev)
  :init (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;----------------------------------------------------------------------------
;; Counsel projectile
;;----------------------------------------------------------------------------
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))
;;C-c p p which project

;;----------------------------------------------------------------------------
;; Company-mode
;;----------------------------------------------------------------------------
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf ; completion-at-point-functions
	   company-emoji
           company-abbrev
	   company-dabbrev))))

;; Company binds ‘RET’ key to ‘company-complete-selection’.
;; This is rather inconvenient in inferior R buffers.
;; One solution is to use ‘TAB’ to complete common
;; part and ‘M-TAB’ for full selection:
(define-key company-active-map [return] nil)
(define-key company-active-map [tab] 'company-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)
(define-key company-active-map (kbd "M-TAB") 'company-complete-selection)

(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-idle-delay 0.5
      company-minimum-prefix-length 2
      company-tooltip-limit 10)

(use-package company-emoji
  :ensure t)

;;----------------------------------------------------------------------------
;; yasnippet
;;----------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode 1))

;;----------------------------------------------------------------------------
;; Magit
;;----------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-c C-g l" . magit-file-log)))

;;----------------------------------------------------------------------------
;; diff-hl
;;----------------------------------------------------------------------------
(use-package diff-hl
  :ensure t
  :init
  ;; Highlight changes to the current file in the fringe
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; Fall back to the display margin, if the fringe is unavailable
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (diff-hl-margin-mode))

;;----------------------------------------------------------------------------
;; popwin
;;----------------------------------------------------------------------------
(use-package popwin
  :ensure t
  :config
  (progn
    (push '("*Completions*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Messages*" :position bottom :height .3) popwin:special-display-config)
    (push '("*magit-commit*" :position bottom :height .3) popwin:special-display-config)
    (push '("COMMIT_EDITMSG" :position bottom :height .3) popwin:special-display-config)
    (push '("*magit-diff*" :position bottom :height .3) popwin:special-display-config)
    (push '("*magit-edit-log*" :position bottom :height .3) popwin:special-display-config)
    (push '("*magit-process*" :position bottom :height .3) popwin:special-display-config)
    (push '("*shell*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Flycheck errors*" :position bottom :height .3) popwin:special-display-config)
    (push '("*company-documentation*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Org Select*" :position bottom :height .3) popwin:special-display-config)
    (push '("*compilation*" :position right :width 80 :noselect t) popwin:special-display-config)
    (push '("*Calendar*" :position bottom :height .3) popwin:special-display-config)
    (push '("*undo-tree Diff*" :position bottom :height .3) popwin:special-display-config)
    (popwin-mode 1)))

;;----------------------------------------------------------------------------
;; flycheck
;;----------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :defer t
  :hook ((ess-r-mode) . flycheck-mode)
  )

					;(use-package flycheck-tip
					;  :ensure t
					;  :defer t
					;  :commands 'flycheck-tip-cycle
					;  :after flycheck
					;  :bind (:map flycheck-mode-map
					;              ("C-c C-n" . flycheck-tip-cycle))
					;  :config
					;  (setq flycheck-display-errors-function 'ignore))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :if (display-graphic-p)
  :after flycheck
  :commands flycheck-pos-tip-mode
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;;----------------------------------------------------------------------------
;; Fix word - upcase - downcase region
;;----------------------------------------------------------------------------
(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)
	 ("M-l" . fix-word-downcase)
	 ("M-c" . fix-word-capitalize)))

;;----------------------------------------------------------------------------
;; Dictionary
;;----------------------------------------------------------------------------
;; Spell checking inside Emacs on macOS requires an external checker. I
;; recommend to install Hunspell (<https://hunspell.github.io>) using
;; Homebrew (<https://brew.sh>).
;; The Hunspell installation does not include any dictionaries.
;; Therefore, this distributions of Emacs ships with the following Libre
;; Office dictionaries suitable for use with Hunspell:
;; - English (version 2019.07.01);
;; - French (version 5.7);
;; - German (version 2017.01.12);
;; - Spanish (version 2.4).
;; Copy the files in the `Dictionaries` directory of the disk image to
;; `~/Library/Spelling`. If needed, create a symbolic link named after
;; your LANG environment variable to the corresponding dictionary and
;; affix files. For example, if LANG is set to fr_CA.UTF-8, do from the
;; command line
;;  cd ~/Library/Spelling
;;  ln -s fr-classique.dic fr_CA.dic
;;  ln -s fr-classique.aff fr_CA.aff
;; Finally, add the following lines to your ~/.emacs file:
(setenv "LANG" "en_US, es_ANY")
(setq-default  ispell-program-name "/usr/local/bin/hunspell")
(with-eval-after-load "ispell"
  (setq ispell-really-hunspell t)
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,es_ANY")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es_ANY"))
;; Spell checking should now work with M-x ispell

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;----------------------------------------------------------------------------
;; Undo-tree
;;----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))
;; Open with C-x u

;;----------------------------------------------------------------------------
;; Dired k
;;----------------------------------------------------------------------------
(use-package dired-k
  :ensure t
  :defer t
  :init
  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;;----------------------------------------------------------------------------
;; Which key  - Do not use with Ivi because it blocks its use
;;----------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

;;----------------------------------------------------------------------------
;; Rainbow delimiters
;;----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :demand
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;;----------------------------------------------------------------------------
;; Rainbow mode
;;----------------------------------------------------------------------------
;;Show Hex Color Codes
(use-package rainbow-mode
  :commands rainbow-mode
  :hook (
	 (prog-mode . rainbow-mode)))

;;----------------------------------------------------------------------------
;; Highlight-Identation
;;----------------------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;;----------------------------------------------------------------------------
;; Beacon
;;----------------------------------------------------------------------------
;;highlight cursor when jump window/buffer
(use-package beacon
  :ensure t
  :commands beacon-mode
  :init
  (beacon-mode 1)
  :config
  ;; only flash on window/buffer changes...
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-blink-when-point-moves t)
  (setq beacon-blink-duration .2)
  (setq beacon-blink-delay .2)
  (setq beacon-size 20));end beacon

;;----------------------------------------------------------------------------
;; all the icons
;;----------------------------------------------------------------------------
(use-package all-the-icons-ivy
  :ensure t
  :init
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

;;----------------------------------------------------------------------------
;; pdf-tools
;;----------------------------------------------------------------------------
;;; Instal poppler via homberew  (brew install poppler automake)
;;; You will also have to help pkg-config find some libraries by setting PKG_CONFIG_PATH, e.g.
;;; in the terminal writte: export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
;; This are other indications from https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

;;----------------------------------------------------------------------------
;; Org-mode
;;----------------------------------------------------------------------------
(define-key global-map "\C-ca" 'org-agenda)

;; Define my agenda files
(setq org-agenda-files (directory-files-recursively "~//GDrive_Personal/org/" "\\.org$"))

;; to automatically add time when a certain TODO is done
;;(setq org-log-done 'time)

;; Define my todo states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))

;; To filter eventual list
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; to filter habits
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; the final agenda
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks nil))))) ;; Change to t if you want to remove the equal divisions

;;----------------------------------------------------------------------------
;; Org-journal
;;----------------------------------------------------------------------------
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/GDrive_Personal/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-find-file 'find-file
	org-journal-file-type 'weekly
	org-journal-enable-agenda-integration 't)
  :bind
  (("C-c C-j" . org-journal-new-entry)
   ("C-c C-s" . org-journal-search)))

;;----------------------------------------------------------------------------
;; Org-roam
;;----------------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ; to remove warning about upgrade v2 - modify notes
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/GDrive_Personal/org/notes/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; From: https://github.com/org-roam/org-roam
;; Org-roam requires sqlite to function. Org-roam optionally uses Graphviz for
;; graph-related functionality. It is recommended to install
;; PCRE-enabled ripgrep for better performance and extended functionality.



;;----------------------------------------------------------------------------
;; Org-bullets
;;----------------------------------------------------------------------------
(use-package org-bullets
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;;----------------------------------------------------------------------------
;; Elpy
;;----------------------------------------------------------------------------
(use-package elpy
  :ensure t
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with company-yasnippet))))))
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 2))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;;----------------------------------------------------------------------------
;; ESS
;;----------------------------------------------------------------------------
(use-package ess
  :ensure t
  :defer t
  :init (require 'ess-site)
  (add-hook 'ess-r-mode-hook
            (lambda () (flycheck-mode t)))
  ;; Outline mode for R
  (add-hook 'ess-mode-hook
	    (lambda ()
	      (outline-minor-mode)
	      (setq outline-regexp "^#.*----")
	      (defun outline-level ()
		(cond (looking-at "^#.*----") 1)
		(t 1000))
	      (defun send-section-to-R ()
		(interactive ())
		(let ((beg))
		  (if (outline-on-heading-p)
		      (beginning-of-line)
		    (outline-previous-visible-heading 1))
		  (setq beg (point))
		  (set-mark (point))
		  (outline-next-visible-heading 1)
		  (previous-line 1)
		  (end-of-line 1)
		  (ess-eval-region-or-function-or-paragraph-and-step)))
	      (local-set-key (kbd "C-c h") 'outline-hide-body)
	      (local-set-key (kbd "C-c s") 'outline-show-all)
	      (local-set-key (kbd "C-c <left>") 'outline-hide-entry)
	      (local-set-key (kbd "C-c <right>") 'outline-show-entry)
	      (local-set-key (kbd "C-c <up>") 'outline-previous-heading)
	      (local-set-key (kbd "C-c <down>") 'outline-next-heading)
	      (local-set-key (kbd "C-c e") 'send-section-to-R)))
  :config
  (define-key ess-r-mode-map ";" #'ess-insert-assign)
  (define-key inferior-ess-r-mode-map ";" #'ess-insert-assign)
  (setq ess-use-eldoc 'script-only)
  ;; History directory
  (setq ess-history-directory "~/.cache")
  (setq inferior-ess-r-program "R")
  (setq ess-eval-visibly t)
  ;;; Flycheck ess
  (setq ess-use-flymake nil) ;; disable Flymake
  ;; use comany-mode
  (setq ess-use-auto-complete nil)
  (setq ess-use-company t)
  ;; Make ‘M-h’ to display quick help:
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  ;; To remove fancy comments defaults (issue with #)
  (setq ess-fancy-comments nil)
  ;; Syntax highlight
  (setq ess-R-font-lock-keywords
	(quote
	 ((ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:%op% . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . t)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t))))
  (setq inferior-ess-r-font-lock-keywords
	(quote
	 (
	  (ess-S-fl-keyword:prompt . t)
	  (ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:messages . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-fl-keyword:matrix-labels . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . t)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t))))
  (unless (getenv "LC_ALL") (setenv "LC_ALL" "en_US.UTF-8"))
					; code below to make it more like R-studio
  (eval-after-load "ess-r-mode"
    '(progn
       (define-key ess-r-mode-map [(control return)] nil)
       (define-key ess-r-mode-map [(shift return)]
	 'ess-eval-region-or-line-visibly-and-step))))
;; ESS
;;(kill-buffer "*ESS*")

;; https://github.com/davidshepherd7/electric-operator
(use-package electric-operator  ;; Automatically add spaces around operators
  :ensure t
  :after ess
  :hook ((ess-r-mode inferior-ess-r-mode python-mode) . electric-operator-mode)
  :custom
  (electric-operator-R-named-argument-style 'spaced))


(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(define-key ess-mode-map (kbd "C->") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C->") 'then_R_operator)

;;----------------------------------------------------------------------------
;; lsp-mode
;;----------------------------------------------------------------------------
(use-package lsp-mode
  :hook (
         ((ess-r-mode python-mode) . lsp)
         ;;if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-auto-guess-root nil
	lsp-eldoc-enable-hover t
        lsp-enable-symbol-highlighting t
	lsp-enable-snippet t
	lsp-file-watch-threshold 2000 ;;nil to disable warning
	lsp-idle-delay 0.5
	lsp-signature-render-documentation t;nil
	lsp-diagnostics-provider 'flycheck
	lsp-prefer-flymake nil
	lsp-signature-auto-activate t
	lsp-completion-show-detail t
	lsp-completion-show-kind nil
	lsp-modeline-code-actions-enable t
	lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-configuration-sources ["flake8"]
	lsp-lens-enable nil
	)
  ;;to remove error ls does not support dired
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (when (boundp 'read-process-output-max)
    ;;New in Emacs 27
    (setq read-process-output-max (* 1024 1024)))
  (setq lsp-log-io nil)) ; if set to true can cause a performance hit)


(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable nil
	lsp-ui-sideline-show-code-actions nil
	lsp-ui-sideline-enable nil))

;;----------------------------------------------------------------------------
;; Markdown-mode
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("//.markdown" . markdown-mode)
         ("//.md" . markdown-mode))
  :init
  (setq markdown-command "markdown"))


;;----------------------------------------------------------------------------
;; grip-mode
;;----------------------------------------------------------------------------
;; it uses python grip package
;; pip install grip
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;;----------------------------------------------------------------------------
;; Polymode - Poly R
;;----------------------------------------------------------------------------
(use-package poly-R
  :ensure t
  :defer t
  :mode (("//.Rnw" . poly-noweb+r-mode)
	 ("//.Rmd" . poly-markdown+r-mode)
	 ("//.Snw" . poly-noweb+r-mode)
         ("//.rmd" . poly-markdown+r-mode))
  :config
  ;; Wrap lines at column limit, but don't put hard returns in
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))

;;Insert new chunk for Rmarkdown
(defun tws-insert-r-chunk (header)
  "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet"
  (interactive "sHeader: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

(global-set-key (kbd "C-c c") 'tws-insert-r-chunk)

;; store the pdf with proper name
(defcustom polymode-exporter-output-file-format "%s"
  "Format of the exported files.
%s is substituted with the current file name sans extension."
  :group 'polymode-export
  :type 'string)

;;----------------------------------------------------------------------------
;; Stan
;;----------------------------------------------------------------------------
;;; stan-mode.el
(use-package stan-mode
  :ensure t
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :ensure t
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/company-stan/"
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; flycheck-stan.el
(use-package flycheck-stan
  :ensure t
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;----------------------------------------------------------------------------
;; AucTeX
;;----------------------------------------------------------------------------
;; for lsp support you need to install a server
;; https://github.com/latex-lsp/texlab#building-from-source
;; 1) (in terminal) curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; 2) $HOME/.cargo/env
;; 3) cargo install --git https://github.com/latex-lsp/texlab.git --locked
;; Installed in: /Users/josemedina/.cargo/bin/texlab
;;; LaTeX with AUCTeX
(use-package tex-site                   ;; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ;; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ;; Parse documents to provide completion
        ;; for packages, etc.
        TeX-auto-save t                 ;; Automatically save style information
        TeX-electric-sub-and-superscript t ;; Automatically insert braces after
        ;; sub- and superscripts in math mode
        TeX-electric-math '("\\(" "\\)")
        ;; Don't insert magic quotes right away.
        TeX-quote-after-quote t
        ;; Don't ask for confirmation when cleaning
        TeX-clean-confirm nil)
  (setq-default TeX-master nil          ;; Ask for the master file
                TeX-engine 'luatex      ;; Use a modern engine
                ;; Redundant in 11.88, but keep for older AUCTeX
                TeX-PDF-mode t)
  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")
  :custom
  (TeX-master nil)
  ;;to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; Provide forward and inverse search with SyncTeX
  (TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  (TeX-source-correlate-mode t)
  (add-hook 'LaTeX-mode-hook 'company-mode))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
          #'TeX-revert-document-buffer)


(use-package tex-buf                    ;; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ;; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                  ;; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ;; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ;; LaTeX editing
  :ensure auctex
  :defer t
  :config
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen nil)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode) ;; Easy math input
  (add-hook 'LaTeX-mode-hook 'company-mode))

(use-package auctex-latexmk             ;; latexmk command for AUCTeX
  :ensure t
  :defer t
  :after latex
  :config (auctex-latexmk-setup))

(use-package auctex-skim                ;; Skim as viewer for AUCTeX
  :load-path "lisp/"
  :commands (auctex-skim-select)
  :after tex
  :config (auctex-skim-select))

(use-package bibtex                     ;; BibTeX editing
  :defer t
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

(defun lunaryorn-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package reftex                     ;; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (setq reftex-plug-into-AUCTeX t
        ;; Automatically derive labels, and prompt for confirmation
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(
          ;; Additional label definitions for RefTeX.
          ("definition" ?d "def:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("definition" "def.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("theorem" "th.") -3)
          ("example" ?x "ex:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("example" "ex") -3)
          ;; Algorithms package
          ("algorithm" ?a "alg:" "~\\ref{%s}"
           "\\\\caption[[{]" ("algorithm" "alg") -3)))

  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}"))))
    (setq reftex-cite-format 'biblatex))
  :diminish reftex-mode)

;;; Markup languages
(use-package rst ;; ReStructuredText
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package latex-preview-pane
  :defer t
  :ensure t)

;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
