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

(setq gc-cons-threshold 100000000)

(require 'package)
(package-initialize)


(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
			 ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
			 ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
			 ))

;; ConfigureUsePackage
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; -ConfigureUsePackage

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
  (load-theme 'doom-dracula t)
  ;;   Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;----------------------------------------------------------------------------
;; Kill general login buffers
;;----------------------------------------------------------------------------
;; Makes *scratch* empty.
(setq initial-scratch-message "Type C-j to run the code")

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;;----------------------------------------------------------------------------
;; Interface and General Tweaks
;;----------------------------------------------------------------------------
;; Define the home directory
(cd (getenv "HOME"))
(message "Current dir: %s" (pwd))
(message "Current buffer: %s" (buffer-name))

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
;; Remove tool bar an scroll bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

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
(fringe-mode '1)

;; Trim spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Wrap lines automatically
(setq-default fill-column 80)

;; line and column number
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode t)

(setq linum-format "%d ")

;; highlight current line
(global-hl-line-mode)

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

;; make small subtrees within the same buffer
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package dired-k
  :ensure t
  :defer t
  :init
  ;; always execute dired-k when dired buffer is opened
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

;;----------------------------------------------------------------------------
;; Projectile
;;----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-sort-order 'recently-active))

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
;; Company-mode
;;----------------------------------------------------------------------------
(use-package company
  :ensure t
  :init
  (global-company-mode))

;;----------------------------------------------------------------------------
;; yasnippet
;;----------------------------------------------------------------------------
(use-package yasnippet                  ;; Snippets
  :ensure t
  :config
  (setq yas-verbosity 1                      ;; No need to be so verbose
	yas-wrap-around-region t)
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets         ;; Collection of snippets
  :ensure t)

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

(setq flycheck-check-syntax-automatically '(save mode-enable))
;; the default value was '(save idle-change new-line mode-enabled)
;; This way, syntax checking will occur only when you save your file or change
;; the major mode.

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
  :ensure t
  :demand
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;;----------------------------------------------------------------------------
;; Rainbow mode
;;----------------------------------------------------------------------------
;;Show Hex Color Codes
(use-package rainbow-mode
  :ensure t
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
;; Org-bullets
;;----------------------------------------------------------------------------
(use-package org-bullets
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

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
  ;; code below to make it more like R-studio
  (eval-after-load "ess-r-mode"
    '(progn
       (define-key ess-r-mode-map [(control return)] nil)
       (define-key ess-r-mode-map [(shift return)]
	 'ess-eval-region-or-line-visibly-and-step))))
;; ESS
;;(kill-buffer "*ESS*")

(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(define-key ess-mode-map (kbd "C->") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C->") 'then_R_operator)

;; https://github.com/davidshepherd7/electric-operator
;(use-package electric-operator  ;; Automatically add spaces around operators
;  :ensure t
;  :after ess
;  :hook ((ess-r-mode inferior-ess-r-mode) . electric-operator-mode)
;  :custom
;  (electric-operator-R-named-argument-style 'spaced)
;  (electric-operator-add-rules-for-mode 'ess-r-mode
;					(cons "*" nil)
;					(cons "in" nil)))

;;----------------------------------------------------------------------------
;; lsp-mode
;;----------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :hook (
         ((ess-r-mode LaTeX-mode) . lsp)
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
	lsp-file-watch-threshold nil
	lsp-idle-delay 0.5
	lsp-signature-render-documentation nil
	lsp-diagnostics-provider 'flycheck
	lsp-prefer-flymake nil
	lsp-signature-auto-activate t
	lsp-completion-show-detail t
	lsp-completion-show-kind nil
	lsp-modeline-code-actions-enable nil
	lsp-lens-enable nil
	lsp-response-timeout 20
	lsp-headerline-breadcrumb-enable nil
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
	lsp-ui-sideline-enable nil
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse nil))

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
;;----------------------------------------------------------------------------
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
