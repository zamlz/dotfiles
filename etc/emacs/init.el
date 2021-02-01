;; ------------------------------------------------------------------------------------------------
;; ------------------------------------------------------------------------------------------------
;;                       _______   ____  __   ______
;;                      / ____/ | / / / / /  / ____/___ ___  ____ ___________
;;                     / / __/  |/ / / / /  / __/ / __ `__ \/ __ `/ ___/ ___/
;;                    / /_/ / /|  / /_/ /  / /___/ / / / / / /_/ / /__(__  )
;;                    \____/_/ |_/\____/  /_____/_/ /_/ /_/\__,_/\___/____/
;; ------------------------------------------------------------------------------------------------
;;                              My Personal GNU Emacs Configuration
;; ------------------------------------------------------------------------------------------------
;; NOTE: This file contains the bulk of the config. If this is the init.el file then you are
;;       looking at the auto-generated file. Please edit config.org instead.
;; ------------------------------------------------------------------------------------------------

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar zamlz/gc-cons-threshold 100000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold zamlz/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq gc-cons-threshold (* zamlz/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold zamlz/gc-cons-threshold)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

(use-package emacs
  :preface
  (defvar zamlz/indent-width 4)   ; tab size
  (defvar zamlz/default-screen-width 100)
  :config
  (setq ring-bell-function 'ignore       ; minimise distraction
        frame-resize-pixelwise t
        default-directory "~/")

  (tool-bar-mode -1)          ; Disable the toolbar
  (menu-bar-mode -1)          ; disable the menubar
  ;; (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  ;; better scrolling experience
  (setq scroll-margin 0
        scroll-conservatively 101 ; > 100
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width zamlz/indent-width
                fill-column zamlz/default-screen-width))

(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package simple
  :ensure nil
  :config
  (column-number-mode +1)
  (global-display-line-numbers-mode t))

;; DONT display line numbers in certain modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package "window"
  :ensure nil
  :preface
  (defun zamlz/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun zamlz/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") #'zamlz/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'zamlz/split-and-follow-vertically))

(use-package files
  :ensure nil
  :config
  (setq create-lockfiles nil ; don't create .# files (crashes 'npm start')
        backup-directory-alist '(("" . "~/.emacs.d/backup"))))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.4))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset zamlz/indent-width)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

(use-package python
  :ensure nil
  :config (setq python-indent-offset zamlz/indent-width))

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))

;; no default startup screen!
;; (setq inhibit-startup-message t)

;; Enable custom dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner "~/etc/emacs/black-hole.png")
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

;; Set default face
(set-face-attribute 'default nil :font "xos4 Terminus" :height 110)
;; (set-face-attribute 'default nil :font "Fira Code" :height 100)
;; (set-face-attribute 'default nil :font "Iosevka Term" :height 100)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "xos4 Terminus" :height 100)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height 100)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; (use-package powerline
;;   :init (powerline-default-theme))

;; (use-package smart-mode-line
;;    :config
;;    (sml/setup))

(add-to-list 'custom-theme-load-path "~/etc/emacs/themes/")

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(use-package gruvbox-theme
;;  :init (load-theme 'gruvbox-dark-hard t))
;;(set-background-color "black")

;; (use-package spacemacs-theme
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))

;; (load-theme 'xresources t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

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

(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind ("<f8>" . neotree-toggle))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-create-definer zamlz/leader-keys
:keymaps '(normal insert visual emacs)
:prefix "SPC"
:global-prefix "C-SPC"))

(use-package hydra)

(zamlz/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme"))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Add hydra func to our personal keybindings
(zamlz/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

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

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
     ("C-x b" . counsel-ibuffer)
     ("C-x C-f" . counsel-find-file)
     :map minibuffer-local-map
     ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode))

;; TODO: Figure out what swiper is lol
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package ivy-rich
  :after ivy
  :config
  ;;(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

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

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-common)))

(use-package flycheck
  :config (global-flycheck-mode +1))

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

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

;; (use-package forge)

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(defun zamlz/org-font-setup ()
  ;; Converts bullet lists to not use the - character but the • character
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; for now, keep all at 1.0
  (dolist (face '((org-level-1 . 3.0)
    (org-level-2 . 2.5)
    (org-level-3 . 2.0)
    (org-level-4 . 1.5)
    (org-level-5 . 1.0)
    (org-level-6 . 1.0)
    (org-level-7 . 1.0)
    (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :font "Fira Code"
                        :weight 'regular
                        :height (cdr face)))

    ;; ensure that anything that should be fixed-width in org appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun zamlz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode +1)
  (setq evil-auto-indent nil)
  (setq fill-column 10000000))

(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . zamlz/org-mode-setup))
  :custom

  ;; Setup directories
  (org-directory "~/usr/org")
  (org-agenda-files (list org-directory
      (concat org-directory "/notes")))

  ;; Add some nice visuals changes
  (org-ellipsis " ▾")

  ;; Some todo/logging changes
  (org-enforce-todo-dependencies t)
  (org-log-done t)
  (org-log-into-drawer t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)

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
     "\n* %U :JOURNAL:\n%?")
    ;; Capture with context
    ;; -------------------
    ("i" "Index Context")
    ("it" "Todo with Context" entry (file "inbox.org")
     "* TODO  %?\n%i\n%a")
    ("ij" "Journal with Context" entry (file+datetree "journal.org")
     "\n* %U :JOURNAL:\n%?\n%i\n%a")
    ;; Capture Contact Information of a person
    ;; -------------------
    ("c" "Contacts" entry (file "contacts.org")
     (concat "* %^{NAME}\n"
         ":PROPERTIES:\n"
         ":CELLPHONE: %^{CELLPHONE}\n"
         ":HOMEPHONE: %^{HOMEPHONE}\n"
         ":WORKPHONE: %^{WORKPHONE}\n"
         ":EMAIL: %^{EMAIL}\n"
         ":EMAIL_ALT: %^{EMAIL_ALT}\n"
         ":WEBSITE: %^{WEBSITE}\n"
         ":COMPANY: %^{COMPANY}\n"
         ":ADDRESS: %^{ADDRESS}\n"
         ":BIRTHDAY: %^{BIRHDAY}t\n"
         ":TITLE: %^{TITLE}\n"
         ":END:"))))

  ;; Setup refiling
  (org-log-refile t)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets
   `((,(concat org-directory "/gtd.org") :maxlevel . 1)))

  ;; Setup archive location
  (org-archive-location (concat org-directory "/archive.org::"))

  ;; ensure that refiling saves buffers
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Make sure we display inline images by default
  (org-startup-with-inline-images t)

  ;; Finally a post setup func to setup fonts
  (zamlz/org-font-setup))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "ROUTINE(r)" "|" "DONE(d)")
              (sequence "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)")
              (sequence "WAITING(w)" "|")
              (sequence "|" "CANCELLED(c)")
              (sequence "SOMEDAY(s)" "|" "CANCELLED(c)")
              (sequence "MEETING" "|"))))

(setq org-todo-keyword-faces
  '(("DONE"      . (:background "#98be65" :foreground "#ffffff" :weight bold))
    ("TODO"      . (:background "#ff6c6b" :foreground "#ffffff" :weight bold))
    ("ROUTINE"   . (:background "#3f444a" :foreground "#51afef" :weight bold))
    ("PROJECT"   . (:background "#51afef" :foreground "#ffffff" :weight bold))
    ("WAITING"   . (:background "#3f444a" :foreground "#ffffff" :weight bold))
    ("CANCELLED" . (:background "#181818" :foreground "#ffffff" :weight bold))
    ("SOMEDAY"   . (:background "#3f444a" :foreground "#ffffff" :weight bold))
    ("MEETING"   . (:background "#a9a1e1" :foreground "#ffffff" :weight bold))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))

(defun zamlz/org-mode-visual-fill ()
  (setq visual-fill-column-width zamlz/default-screen-width
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . zamlz/org-mode-visual-fill))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

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

;; Setup structure templates for org-babel
(require 'org-tempo)
(add-to-list `org-structure-template-alist '("sh" . "src shell"))
(add-to-list `org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list `org-structure-template-alist '("py" . "src python"))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/etc/emacs/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(add-to-list 'org-modules 'org-habit t)
(setq org-habit-preceding-days 31)
(setq org-habit-following-days 3)
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-show-all-today t)

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/usr/notes/")
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))

(use-package vterm
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.lgr\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
