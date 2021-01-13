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

;; no startup screen! (maybe I want a custom one though)
(setq inhibit-startup-message t)

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
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set custom font (type face)
;;(set-face-attribute 'default nil :font "xos4 Terminus" :height 110)
(set-face-attribute 'default nil :font "Iosevka Term" :height 110)

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
  :init (load-theme 'gruvbox t))

;; rainbow delimiters for programming parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :init (evil-mode 1))

;; ----------------------------------------------------------------------------
;; ORG MODE SETTINGS
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-log-done t)
(setq org-agenda-files (list "~/org"))
(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
  '(("^ *\\([-]\\) "
  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ----------------------------------------------------------------------------
;; MISC CONFIGS 
;; ----------------------------------------------------------------------------

(use-package magit)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
