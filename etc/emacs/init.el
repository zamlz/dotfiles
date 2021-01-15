;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;;    _______   ____  __   ______
;;   / ____/ | / / / / /  / ____/___ ___  ____ ___________
;;  / / __/  |/ / / / /  / __/ / __ `__ \/ __ `/ ___/ ___/
;; / /_/ / /|  / /_/ /  / /___/ / / / / / /_/ / /__(__  )
;; \____/_/ |_/\____/  /_____/_/ /_/ /_/\__,_/\___/____/
;; ----------------------------------------------------------------------------
;; My Personal GNU Emacs Configuration
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; PACKAGE MANAGEMENT
;; ----------------------------------------------------------------------------

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------------------------------------------------------------------
;; APPEARANCE SETTINGS
;; ----------------------------------------------------------------------------

;; no default startup screen!
;; (setq inhibit-startup-message t)
(use-package dashboard
  :ensure t
  :config
  (setq dhasboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq initial-buffer-choice (lambda() (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents   . 10)
                          (bookmarks . 5)
                          (projects  . 10)
                          (agenda    . 10)
                          (registers . 5)))
  (dashboard-modify-heading-icons '((bookmarks . "book")))
  (dashboard-setup-startup-hook))

;; Set up the visible bell
(setq visible-bell t)

;; Cleanup the sidebars
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Setup line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers in some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set custom font (type face)

;; Set default face
;;(set-face-attribute 'default nil :font "Fira Code" :height 100)
;;(set-face-attribute 'default nil :font "xos4 Terminus" :height 110)
(set-face-attribute 'default nil :font "Iosevka Term" :height 100)

;; Set the fixed pitch face
;;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)

;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)

;; to install the fonts, you must also run this command
;;    M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; Modeline needs the icons above to be installed
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Setup the gruvbox theme lol
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-hard t))

;; rainbow delimiters for programming parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; configure aesthetically pleasing tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker  "●")
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>"  . centaur-tabs-forward))

;; ----------------------------------------------------------------------------
;; CUSTOM KEYBINDINGS
;; ----------------------------------------------------------------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Useful package to show what keybindings are available
;; under each prefix
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

;; Setup evil mode
(use-package evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;; ----------------------------------------------------------------------------
;; AUTOCOMPLETION ENGINE AND DOCUMENTATION REDUX
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; GIT AND PROJECT CONFIGURATION
;; ----------------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind (:map projectile-mode-map ("M-p"   . projectile-command-map))
  :init (when (file-directory-p "~/src")
	  (setq projectile-project-search-path '("~/src")))
        (setq projectile-switch-project-action #'projectile-dired))

(use-package magit)

;; (use-package forge) Set this up!

;; ----------------------------------------------------------------------------
;; ORG MODE SETTINGS
;; ----------------------------------------------------------------------------

;; Create a function that sets up fonts for org-mode
(defun zamlz/org-font-setup ()
  ;; Converts bullet lists to not use the - character but the • character
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; for now, keep all at 1.0
  (dolist (face '((org-level-1 . 1.0)
    (org-level-2 . 1.0)
    (org-level-3 . 1.0)
    (org-level-4 . 1.0)
    (org-level-5 . 1.0)
    (org-level-6 . 1.0)
    (org-level-7 . 1.0)
    (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
      :font "Iosevka Term"
      :weight 'regular
      :height (cdr face))))

;; Setup org-mode
(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-directory "~/org")
  (setq org-ellipsis " ▾")
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-agenda-files
    (list org-directory
          (concat org-directory "/todo")
          (concat org-directory "/notes")))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)"
                "|" "DONE(d)" "CANCELLED(c)")))
  (zamlz/org-font-setup))

;; Change the bullet appearance of org-mode headings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))

(defun zamlz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . zamlz/org-mode-visual-fill))

;; Custom keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Setup org capture mode
(setq org-capture-templates
      '(
	("j" "Journal Entries")

        ("jj" "Journal" entry (file+datetree "journal.org")
         "\n* %U :JOURNAL:\n  %?")
        ("ji" "Journal with Context" entry (file+datetree "journal.org")
         "\n* %U :JOURNAL:\n  %?\n  %i\n  %a")

	("t" "Tasks / Projects")

        ("tt" "Todo" entry (file "todo/inbox.org")
         "* TODO  %?")
        ("ti" "Todo with Context" entry (file "todo/inbox.org")
         "* TODO  %?\n  %i\n  %a")

	("c" "Contacts" entry (file "contacts.org")
	 "* %^{NAME}
  :PROPERTIES:
  :CELLPHONE: %^{CELLPHONE}
  :HOMEPHONE: %^{HOMEPHONE}
  :WORKPHONE: %^{WORKPHONE}
  :EMAIL: %^{EMAIL}
  :EMAIL_ALT: %^{EMAIL_ALT}
  :WEBSITE: %^{WEBSITE}
  :COMPANY: %^{COMPANY}
  :ADDRESS: %^{ADDRESS}
  :BIRTHDAY: %^{BIRHDAY}t
  :TITLE: %^{TITLE}
  :END")
       ))

;; ----------------------------------------------------------------------------
;; MISC SETTINGS
;; ----------------------------------------------------------------------------

;; don't create backups in same directory as source file
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Set a seperate directory for emacs customize options
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
