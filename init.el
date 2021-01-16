;;; package --- Summary
;;; init.el ---

;;; Commentary:
;; I used a lot of code from https://github.com/MatthewZMD/.emacs.d
;; I use emacs-head@28 from daviderestivo/homebrew-emacs-head,

;;; Code:
(require 'package)
(package-initialize)


;;; Package archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))


;;----------------------------------------------------------------------------
;; theme
;;----------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;;----------------------------------------------------------------------------
;; Kill general login buffers
;;----------------------------------------------------------------------------
;; Makes *scratch* empty.
(setq initial-scratch-message "type C-j to run the code ")

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")


;;----------------------------------------------------------------------------
;; Prevent may windows open when open one window
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)


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

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Line number configuration
(global-display-line-numbers-mode)

;; Window size and features
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 90))

;; Remove tool bar an scroll bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; We don't want to type yes and no all the time so do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Overwrite region selected
(delete-selection-mode t)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Remove noise emacs
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Suppress GUI features
(setq inhibit-startup-screen t)

;; Confirm quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Title bar
(setq-default frame-title-format '("" user-login-name "@" system-name " - %b"))

;; To remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
(display-time-mode 1)
(display-battery-mode 1)


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
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)


;;----------------------------------------------------------------------------
;; Smooth scrolling
;;----------------------------------------------------------------------------
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)


;;----------------------------------------------------------------------------
;; Backpups
;;----------------------------------------------------------------------------
(defconst my-backup-dir
  (expand-file-name (concat user-emacs-directory "backups")))

(setq make-backup-files t ;;make backup first time a file is saved
      version-control t ;; number and keep versions of backups
      backup-by-copying t ;; and copy (don't clobber symlinks) them to...
      backup-directory-alist '(("." . "~/Google Drive/EMACS_BACKUPS/")) ;; ... here
      kept-new-versions 6 ;; the number of newest (before current version) version to keep
      kept-old-versions 2 ;; the number of old versions to keep
      delete-old-versions t ;; don't ask about deleting old versions
      vc-make-backup-files t ;; even backup files under version control (git,svn,etc.)
      ;;make-backup-files nil  ;; no annoying "~file.txt"
      auto-save-default nil ;; no auto saves to #file#
      )


;;----------------------------------------------------------------------------
;; Time-stamp
;;----------------------------------------------------------------------------
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
1;; emacs will write time-stamp information there when saving the file.
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
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
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
;; exec-path-from-shell
;;----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
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
                  (face-attribute 'default :family))
  )


;;----------------------------------------------------------------------------
;; all the icons
;;----------------------------------------------------------------------------
;; For this package to work best, you need to install the resource fonts
;; included in the package. M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  )

; If you experience a slow down in performance when rendering multiple icons
; simultaneously, you can try setting the following variable
(setq inhibit-compacting-font-caches t)


;;----------------------------------------------------------------------------
;; Projectile
;;----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;;----------------------------------------------------------------------------
;; Dashboard
;;----------------------------------------------------------------------------
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Set the title
  (setq dashboard-banner-logo-title "")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer;

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t);

  ;; To disable shortcut "jump" indicators for each section, set
  ;(setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 10)
                          (projects . 7)
			  (agenda . 5)))
 ;; To add icons to the widget headings and their items:
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
 ;; A randomly selected footnote will be displayed. To disable it:
  (setq dashboard-set-footer nil)
  )


;;----------------------------------------------------------------------------
;; Dimmer
;;----------------------------------------------------------------------------
(use-package dimmer
  :ensure t
  :init
  (dimmer-mode)
  :custom
  (dimmer-fraction 0.225))


;;----------------------------------------------------------------------------
;; Diminish
;;----------------------------------------------------------------------------
(use-package diminish
  :ensure t)


;;----------------------------------------------------------------------------
;; smartparens
;;----------------------------------------------------------------------------
(use-package smartparens
  :ensure t
  :hook ((prog-mode ess-r-mode) . smartparens-mode)
  :diminish ;smartparens-mode
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
   (doom-modeline-height 15)
   (doom-modeline-bar-width 3)
   (doom-modeline-icon t)
   (doom-modeline-buffer-file-name-style  'truncate-with-project))
;;This package requires the fonts included with all-the-icons to be installed. Run M-x all-the-icons-install-fonts to do so.


;;----------------------------------------------------------------------------
;; Ivy, ivy rich and dependatns
;;----------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-rich
  :ensure t
  :after counsel
  :config (setq ivy-rich-path-style 'abbrev)
  :init (ivy-rich-mode 1)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :init
  (setq counsel-yank-pop-separator
    (concat "\n\n"
      (concat (apply 'concat (make-list 50 "---")) "\n")))
  :bind (
  ("M-y" . counsel-yank-pop)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))
  :config
  (use-package smex :ensure t)
  )


(use-package ivy-prescient
  :ensure t
  :after ivy
  :config
  (ivy-prescient-mode))


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
    :config
    ;; Highlight changes to the current file in the fringe
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    ;; Fall back to the display margin, if the fringe is unavailable
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (diff-hl-margin-mode)
    (setq diff-hl-margin-side 'right)
    )


;;----------------------------------------------------------------------------
;; treemacs
;;----------------------------------------------------------------------------
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-all-the-icons
  :ensure t)


;;----------------------------------------------------------------------------
;; lsp-mode
;;----------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :hook ((ess-r-mode . lsp)
		 ;(python-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; to remove error ls does not support dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c C-v s" . lsp-ui-sideline-toggle-symbols-info)
         ("C-c C-v d" . lsp-ui-doc-mode))
  :config
  (setq
        ;; lsp-ui-doc
        lsp-ui-doc-enable t
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature nil
	lsp-ui-doc-delay 2
        ;; top, bottom, or at-point
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit t
        ;; lsp-ui-sideline
        ;lsp-ui-sideline-enable nil
        ;lsp-ui-sideline-ignore-duplicate t
        ;lsp-ui-sideline-show-symbol t
        ;lsp-ui-sideline-show-hover t
        ;lsp-ui-sideline-show-diagnostics t
        ;lsp-ui-sideline-show-code-actions t
        ;lsp-ui-sideline-code-actions-prefix ""
         ;; lsp-ui-peek
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        ;; never, on-demand, or always
        lsp-ui-peek-fontify 'on-demand
        ))

;; Ivy integration
(use-package lsp-ivy
  :ensure t
  :after lsp-mode)


;;----------------------------------------------------------------------------
;; popwin
;;----------------------------------------------------------------------------
(use-package popwin
  :ensure t
  :diminish ;popwin
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
    (popwin-mode 1)))


;;----------------------------------------------------------------------------
;; flycheck
;;----------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :defer t
;  :diminish
  :hook ((markdown-mode ess-r-mode python-mode) . flycheck-mode))


;;----------------------------------------------------------------------------
;; flycheck-tip
;;----------------------------------------------------------------------------
(use-package flycheck-tip
  :ensure t
  :commands 'flycheck-tip-cycle
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c C-n" . flycheck-tip-cycle))
  :config
  (setq flycheck-display-errors-function 'ignore)
  )


;;----------------------------------------------------------------------------
;; Emoji
;;----------------------------------------------------------------------------
(use-package company-emoji
  :ensure t)


;;----------------------------------------------------------------------------
;; Company-mode
;;----------------------------------------------------------------------------
(use-package company
  :diminish
  :ensure t
  :hook
  (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-emoji)
  )


(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode))


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
; Spell checking inside Emacs on macOS requires an external checker. I
; recommend to install Hunspell (<https://hunspell.github.io>) using
; Homebrew (<https://brew.sh>).
; The Hunspell installation does not include any dictionaries.
; Therefore, this distributions of Emacs ships with the following Libre
; Office dictionaries suitable for use with Hunspell:
; - English (version 2019.07.01);
; - French (version 5.7);
; - German (version 2017.01.12);
; - Spanish (version 2.4).
; Copy the files in the `Dictionaries` directory of the disk image to
; `~/Library/Spelling`. If needed, create a symbolic link named after
; your LANG environment variable to the corresponding dictionary and
; affix files. For example, if LANG is set to fr_CA.UTF-8, do from the
; command line
;  cd ~/Library/Spelling
;  ln -s fr-classique.dic fr_CA.dic
;  ln -s fr-classique.aff fr_CA.aff
; Finally, add the following lines to your ~/.emacs file:
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


;;----------------------------------------------------------------------------
;; Undo-tree
;;----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :defer t
  :diminish ;undo-tree-mode
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
  :diminish
  :config
(which-key-mode)
(which-key-setup-side-window-bottom))


;;----------------------------------------------------------------------------
;; Rainbow delimiters
;;----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;;----------------------------------------------------------------------------
;; Rainbow mode
;;----------------------------------------------------------------------------
(use-package rainbow-mode
  :diminish
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode 'ess-r-mode 'markdown-mode))


;;----------------------------------------------------------------------------
;; Highlight-Identation
;;----------------------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :if (display-graphic-p)
  :diminish
  ;; Enable manually if needed, it a severe bug which potentially core-dumps Emacs
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/76
  :commands (highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))


;;----------------------------------------------------------------------------
;; all the icons ivy
;;----------------------------------------------------------------------------
(use-package all-the-icons-ivy
  :ensure t
  :init
  (all-the-icons-ivy-setup)
  )


;;----------------------------------------------------------------------------
;; all the icons ivy rich
;;----------------------------------------------------------------------------
(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1)
  )


;;----------------------------------------------------------------------------
;; all the icons dired
;;----------------------------------------------------------------------------
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
)


;;----------------------------------------------------------------------------
;; all the icons gnus
;;----------------------------------------------------------------------------
(use-package all-the-icons-gnus
  :ensure t
  :init (all-the-icons-gnus-setup)
)


;;----------------------------------------------------------------------------
;; all the icons ibuffer
;;----------------------------------------------------------------------------
(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1)
  )


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
 (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
 )


;;----------------------------------------------------------------------------
;; Web-mode
;;----------------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode
  ("\\.html?\\'" . web-mode)
  )


;;----------------------------------------------------------------------------
;; impatient-mode
;;----------------------------------------------------------------------------
(use-package impatient-mode
  :ensure t)
;; Package cl is deprecated -> this warning is ok dont worry


;;----------------------------------------------------------------------------
;; elpy
;;----------------------------------------------------------------------------
(use-package elpy
  :ensure t
  :defer t
  :init
 (advice-add 'python-mode :before 'elpy-enable)
 :config
;; Interpreter setup
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
; emacs dont warm me about identation
(setq python-indent-guess-indent-offset-verbose nil)
)


(add-hook 'python-mode-hook
(lambda ()
   (setq indent-tabs-mode nil)
   (setq python-indent-offset 4)))


;;----------------------------------------------------------------------------
;; lsp-python-ms
;;----------------------------------------------------------------------------
;; to use python with lsp install the following
;; pip3 install 'python-language-server[all]'
;(use-package lsp-python-ms
;  :ensure t
;  :init (setq lsp-python-ms-auto-install-server t)
;  :hook (python-mode . (lambda ()
;                          (require 'lsp-python-ms)
;                          (lsp))))  ; or lsp-deferred


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
    '(lambda ()
    (outline-minor-mode)
    (setq outline-regexp "^#.*----")
    (defun outline-level ()
    (cond (looking-at "^#.*----") 1)
    (t 1000)
    )

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
    (ess-eval-region-or-function-or-paragraph-and-step)
    )
    )

    (local-set-key (kbd "C-c h") 'outline-hide-body)
    (local-set-key (kbd "C-c s") 'outline-show-all)
    (local-set-key (kbd "C-c <left>") 'outline-hide-entry)
    (local-set-key (kbd "C-c <right>") 'outline-show-entry)
    (local-set-key (kbd "C-c <up>") 'outline-previous-heading)
    (local-set-key (kbd "C-c <down>") 'outline-next-heading)
    (local-set-key (kbd "C-c e") 'send-section-to-R)))
  :config
  (define-key ess-r-mode-map "_" #'ess-insert-assign)
  (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)
  (setq ess-use-eldoc nil)
  ;(setq ess-use-company nil)
  (setq inferior-ess-r-program "R")
  (setq ess-eval-visibly t)
  ;;; Flycheck ess
  (setq ess-use-flymake nil) ;; disable Flymake
  ;; Auto-complete only in the script
  ;(setq ess-use-auto-complete t) ;'script-only
  ; Syntax highlight
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
     (ess-R-fl-keyword:F&T . t)
     )))
(eval-after-load "ess-r-mode"
  '(progn
     (define-key ess-r-mode-map [(control return)] nil)
     (define-key ess-r-mode-map [(shift return)]
       'ess-eval-region-or-line-and-step)))
  (unless (getenv "LC_ALL") (setenv "LC_ALL" "en_US.UTF-8")))

;; ESS
(kill-buffer "*ESS*")







;;----------------------------------------------------------------------------
;; AucTeX
;;----------------------------------------------------------------------------
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
;  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'company-mode)

  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;  (add-hook 'LaTeX-mode-hook (lambda ()
;    (push
;    '("latexmk" "latexmk -pdf --synctex=1 %s" TeX-run-TeX nil t
;      :help "Run latexmk on file")
;    TeX-command-list)))
   ;; Update PDF buffers after successful LaTeX runs
;  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
;            #'TeX-revert-document-buffer)
;  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  (TeX-source-correlate-mode t))

 ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)


;;----------------------------------------------------------------------------
;; Latex preview pane
;;----------------------------------------------------------------------------
(use-package latex-preview-pane
  :defer t
  :ensure t
;; :hook (LaTeX-mode . latex-preview-pane-mode)
  )


;;----------------------------------------------------------------------------
;; Markdown-mode
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :defer t
   :mode (("//.markdown" . markdown-mode)
         ("//.md" . markdown-mode)
         ("//.ronn?" . markdown-mode)))


;;----------------------------------------------------------------------------
;; Polymode - Poly R
;;----------------------------------------------------------------------------
(use-package poly-R
  :ensure t
  :defer t
   :mode (("//.Rnw" . poly-noweb+r-mode)
	  ("//.Rmd" . poly-markdown+r-mode)
	  ("//.Snw" . poly-noweb+r-mode)
          ("//.rmd" . poly-markdown+r-mode)))

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
  (setq stan-indentation-offset 2)
  )


;;; company-stan.el
(use-package company-stan
  :ensure t
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/company-stan/"
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil)
  )


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
;; Org-mode
;;----------------------------------------------------------------------------
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

 ;; Define my agenda files
(setq org-agenda-files (directory-files-recursively "~//Google Drive/org/" "\\.org$"))

;; to automatically add time when a certain TODO is done
;(setq org-log-done 'time)

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
          ((org-agenda-compact-blocks nil))))) ; Change to t if you want to remove the equal divisions


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
  (setq org-journal-dir "~/Google Drive/org/journal/"
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
  :diminish
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Google Drive/org/notes/")
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




(provide 'init)
