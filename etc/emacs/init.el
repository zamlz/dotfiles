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
		shell-mode-hook
                eshell-mode-hook
		vterm-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set custom font (type face)

;; Set default face
;;(set-face-attribute 'default nil :font "Fira Code" :height 100)
(set-face-attribute 'default nil :font "xos4 Terminus" :height 110)
;;(set-face-attribute 'default nil :font "Iosevka Term" :height 100)

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
;;(use-package gruvbox-theme
;;  :init (load-theme 'gruvbox-dark-hard t))
;;(set-background-color "black")
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; rainbow delimiters for programming parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; configure aesthetically pleasing tabs
;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-headline-match)
;;   (setq centaur-tabs-style "bar")
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-gray-out-icons 'buffer)
;;   (setq centaur-tabs-height 24)
;;   (setq centaur-tabs-set-bar 'over)
;;   (setq centaur-tabs-set-modified-marker t)
;;   (setq centaur-tabs-modified-marker  "●")
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>"  . centaur-tabs-forward))

;; setup neotree (make sure all-the-icons is installed)
(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind ("<f8>" . neotree-toggle))

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
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; C-h is help in normal mode, but becomes BACKSPACE in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Really nice collection of evil bindings for other modes
;; very good defaults
(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;; Generalized keybinder
(use-package general
  :config
  (general-create-definer zamlz/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (zamlz/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

;; Conquer the hydra
(use-package hydra)

;; Add a custom hydra func to scale text size
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Add hydra func to our personal keybindings
(zamlz/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; ----------------------------------------------------------------------------
;; AUTOCOMPLETION ENGINE AND DOCUMENTATION REDUX
;; ----------------------------------------------------------------------------

;; Supposedly used by ivy
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode))

;; Add Ivy Completion
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-x B" . ivy-switch-buffer-other-window)
	 :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; Improve the Ivy completion UI
(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

;; TODO: Figure out what swiper is lol
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; Improve the help documentation commands by overriding them with helpful
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; ----------------------------------------------------------------------------
;; GIT AND PROJECT CONFIGURATION
;; ----------------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired))

;; use C-o in the "C-c p p" to see other extra commands provided by counsel
(use-package counsel-projectile
  :config (counsel-projectile-mode))

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
  :custom

  ;; Setup directories
  (org-directory "~/usr/org")
  (org-agenda-files (list org-directory
          (concat org-directory "/notes")))

  ;; Add some nice visuals changes
  (org-ellipsis " ▾")

  ;; Some todo/logging changes
  (org-log-done t)
  (org-log-into-drawer t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|"
	       "DONE(d)" "CANCELLED(c)")))

  ;; Setup org capture mode
  (org-capture-templates
   '(
	;; Capture todo type tasks
	;; -------------------
        ("t" "Todo" entry (file "inbox.org")
         "* TODO  %?")
        ;; Capture Journal entries
	;; -------------------
        ("j" "Journal" entry (file+datetree "journal.org")
         "\n* %U :JOURNAL:\n  %?")
	;; Capture with context
	;; -------------------
	("i" "Index Context")
        ("it" "Todo with Context" entry (file "inbox.org")
         "* TODO  %?\n  %i\n  %a")
        ("ij" "Journal with Context" entry (file+datetree "journal.org")
         "\n* %U :JOURNAL:\n  %?\n  %i\n  %a")
	;; Capture Contact Information of a person
	;; -------------------
	("c" "Contacts" entry (file "contacts.org")
	 (concat "* %^{NAME}\n"
		 "  :PROPERTIES:\n"
		 "  :CELLPHONE: %^{CELLPHONE}\n"
		 "  :HOMEPHONE: %^{HOMEPHONE}\n"
		 "  :WORKPHONE: %^{WORKPHONE}\n"
		 "  :EMAIL: %^{EMAIL}\n"
		 "  :EMAIL_ALT: %^{EMAIL_ALT}\n"
		 "  :WEBSITE: %^{WEBSITE}\n"
		 "  :COMPANY: %^{COMPANY}\n"
		 "  :ADDRESS: %^{ADDRESS}\n"
		 "  :BIRTHDAY: %^{BIRHDAY}t\n"
		 "  :TITLE: %^{TITLE}\n"
		 "  :END:"))))

  ;; Setup refiling
  (org-log-refile t)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets
   `((,(concat org-directory "/gtd.org") :maxlevel . 1)
     (,(concat org-directory "/routines.org") :maxlevel . 1)))

  ;; Setup archive location
  (org-archive-location (concat org-directory "/archive.org::"))

  ;; ensure that refiling saves buffers
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Finally a post setup func to setup fonts
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

;; Configure babel for org-mode
;; https://orgmode.org/worg/org-contrib/babel/languages/index.html
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (gnuplot . t)
   (latex . t)
   ))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

;; Provide latex previews in org-mode
(setq org-startup-with-latex-preview t)

;; Add habit tracking into org-mode
(add-to-list 'org-modules 'org-habit t)

;; ----------------------------------------------------------------------------
;; MISC SETTINGS
;; ----------------------------------------------------------------------------

;; Configure a better terminal
(use-package vterm
  :ensure t)

;; manuualy setup beancount mode
(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.lgr\\'" . beancount-mode))

;;(setq beancount-outline-regexp "\\(\\*+\\)")
(add-hook 'beancount-mode-hook #'outline-minor-mode)

;; don't create backups in same directory as source file
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Set a seperate directory for emacs customize options
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
