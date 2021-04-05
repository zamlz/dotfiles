;; Garbage Collector Hooks

;; Supposedly makes the startup a bit more effecient. We also revert the changes to the GC via a hook once the startup has completed.


;; [[file:emacs.org::*Garbage Collector Hooks][Garbage Collector Hooks:1]]
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
;; Garbage Collector Hooks:1 ends here

;; Update Load Path

;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.


;; [[file:emacs.org::*Update Load Path][Update Load Path:1]]
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
;; Update Load Path:1 ends here

;; Package Manager

;; Setup the package manager for use later on downstream.


;; [[file:emacs.org::*Package Manager][Package Manager:1]]
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
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))
;; Package Manager:1 ends here

;; Basic Emacs Setup

;; Lets get some basic settings out of the way here.


;; [[file:emacs.org::*Basic Emacs Setup][Basic Emacs Setup:1]]
(use-package emacs
  :preface
  ;; Setup personal preferances
  (defvar zamlz/indent-width 4)   ; tab size
  (defvar zamlz/default-screen-width 100)
  :custom
  ;; Configure personal information
  (user-full-name "Amlesh Sivanantham")
  (user-mail-address "zamlz@pm.me")
  (user-login-name "zamlz")
  ;; Other basic settings
  (ring-bell-function 'ignore) ; minimise distractio
  (frame-resize-pixelwise t)
  (default-directory "~/")
  :config
  ;; Set Environment Variables
  (setenv "PINENTRY_USER_DATA" "rofi")
  (setenv "VISUAL" "emacsclient --socket-name=xorg-emacs-daemon" )
  (setenv "EDITOR" (getenv "VISUAL"))
  ;; Configure Specific UI changes
  (tool-bar-mode -1)          ; Disable the toolbar
  (menu-bar-mode -1)          ; disable the menubar
  (set-fringe-mode 10)        ; Give some breathing room
  (blink-cursor-mode 1)       ; Let the cursor be blinking
  (semantic-mode 1)
  ;; (tooltip-mode -1)           ; Disable tooltips
  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width zamlz/indent-width
                fill-column zamlz/default-screen-width))
;; Basic Emacs Setup:1 ends here

;; Disable Default Startup

;; Original startup is hideous...


;; [[file:emacs.org::*Disable Default Startup][Disable Default Startup:1]]
(use-package "startup"
  :ensure nil
  :custom (inhibit-startup-screen t))
;; Disable Default Startup:1 ends here

;; Modernize Selection Behaviour

;; Replaces active region just by typing text (is this true in visual mode as well?)


;; [[file:emacs.org::*Modernize Selection Behaviour][Modernize Selection Behaviour:1]]
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))
;; Modernize Selection Behaviour:1 ends here

;; Disable Scroll-Bar


;; [[file:emacs.org::*Disable Scroll-Bar][Disable Scroll-Bar:1]]
(use-package scroll-bar
  :ensure nil
  :custom
  ;; better scrolling experience
  (scroll-margin 0)
  (scroll-conservatively 101) ; > 100
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  :config
  ;; Don't display the scroll bar in buffers
  (scroll-bar-mode -1))
;; Disable Scroll-Bar:1 ends here

;; Enable Column Numbers


;; [[file:emacs.org::*Enable Column Numbers][Enable Column Numbers:1]]
(use-package simple
  :ensure nil
  :config
  (column-number-mode +1)
  (global-display-line-numbers-mode t))

  ;; DONT display line numbers in certain modes
  (dolist (mode '(term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; Enable Column Numbers:1 ends here

;; Split and Follow Windows


;; [[file:emacs.org::*Split and Follow Windows][Split and Follow Windows:1]]
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
;; Split and Follow Windows:1 ends here

;; Backup and Autosave Files

;; Emacs decides to save backup files and lockfiles within the same directory as the files we are editing. Thats just ugly when looking at the filesystem. This will fix that.


;; [[file:emacs.org::*Backup and Autosave Files][Backup and Autosave Files:1]]
(use-package files
  :ensure nil
  :custom
  (create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (backup-directory-alist `(("." . "~/.emacs.d/backup"))))
;; Backup and Autosave Files:1 ends here

;; Auto-Refresh Changes from External Buffers

;; Auto refresh changes from outsides buffers. What more needs to be said.


;; [[file:emacs.org::*Auto-Refresh Changes from External Buffers][Auto-Refresh Changes from External Buffers:1]]
;; (use-package autorevert
;;   :ensure nil
;;   :config
;;   (auto-revert-interval 2)
;;   (auto-revert-check-vc-info t)
;;   (global-auto-revert-non-file-buffers t)
;;   (auto-revert-verbose nil)
;;   :config
;;   (global-auto-revert-mode +1))
;; Auto-Refresh Changes from External Buffers:1 ends here

;; Eldoc Documentation

;; Slightly shorten the Eldoc display delay


;; [[file:emacs.org::*Eldoc Documentation][Eldoc Documentation:1]]
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 0.4))
;; Eldoc Documentation:1 ends here

;; Mouse Wheel Scroll Speed


;; [[file:emacs.org::*Mouse Wheel Scroll Speed][Mouse Wheel Scroll Speed:1]]
(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))
;; Mouse Wheel Scroll Speed:1 ends here

;; Highlight Matching Parentheses


;; [[file:emacs.org::*Highlight Matching Parentheses][Highlight Matching Parentheses:1]]
(use-package paren
  :ensure nil
  :custom (show-paren-delay 0)
  :config (show-paren-mode +1))
;; Highlight Matching Parentheses:1 ends here

;; Auto-pairing Quotes and Parentheses

;; Super useful for auto-pairing certain characters. However we should make use of a hook to prevent it for left carrot bracket in org-mode buffers


;; [[file:emacs.org::*Auto-pairing Quotes and Parentheses][Auto-pairing Quotes and Parentheses:1]]
(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  ;; disable <> auto-pairing in org-mode buffers
  (org-mode  . (lambda ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
;; Auto-pairing Quotes and Parentheses:1 ends here

;; Clean Whitespace on Buffer Save


;; [[file:emacs.org::*Clean Whitespace on Buffer Save][Clean Whitespace on Buffer Save:1]]
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))
;; Clean Whitespace on Buffer Save:1 ends here

;; Dump Custom-Set-Variables


;; [[file:emacs.org::*Dump Custom-Set-Variables][Dump Custom-Set-Variables:1]]
(use-package cus-edit
  :ensure nil
  :custom (custom-file (concat user-emacs-directory "to-be-dumped.el")))
;; Dump Custom-Set-Variables:1 ends here

;; Dired Customization


;; [[file:emacs.org::*Dired Customization][Dired Customization:1]]
(use-package dired
  :ensure nil
  :after evil-collection
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-lahF --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
;; Dired Customization:1 ends here

;; Easy PGP Assistant (EPA)

;; EPA is a built-in emacs package for interfacing with GnuPG.


;; [[file:emacs.org::*Easy PGP Assistant (EPA)][Easy PGP Assistant (EPA):1]]
(use-package epa-file
  :ensure nil
  :custom
  ;; Don't ask by default which key to use
  (epa-file-select-keys nil)
  ;; default to user mail address
  (epa-file-encrypt-to user-mail-address)
  ;; Set the pinentry mode to be loopback to gpg gets the password
  ;; through emacs instead of using pinentry.
  (epa-pinentry-mode 'loopback))
;; Easy PGP Assistant (EPA):1 ends here

;; Auth Source Pass

;; The auth-source-pass package, formerly known as auth-password-store, integrates Emacs' auth-source library with password-store. The auth-source library is a way for Emacs to answer the old burning question “What are my user name and password?”. Password-store (or just pass) is a standard unix password manager following the Unix philosophy. More details can be found at [[https://github.com/DamienCassou/auth-source-pass][github:DamienCassou/auth-source-pass]].


;; [[file:emacs.org::*Auth Source Pass][Auth Source Pass:1]]
(use-package auth-source-pass
  :ensure nil
  :init (auth-source-pass-enable))
;; Auth Source Pass:1 ends here

;; Calc


;; [[file:emacs.org::*Calc][Calc:1]]
(use-package calc
  :ensure nil
  :custom
  (calc-angle-mode 'rad)
  (calc-symbolic-mode t))
;; Calc:1 ends here

;; Startup Dashboard

;; The default landing page isn't quite nice. I originally had it configured to display the scratch page, but then I really wanted like a menu to quickly access my stuff.


;; [[file:emacs.org::*Startup Dashboard][Startup Dashboard:1]]
;; Enable custom dashboard
(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner "~/org/config/lib/emacs-themes/navi.png")
  ;; (dashboard-startup-banner "~/org/config/lib/emacs-themes/black-hole.png")
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  (initial-buffer-choice (lambda() (get-buffer "*dashboard*")))
  (dashboard-items '())
  :config
  (dashboard-modify-heading-icons '((bookmarks . "book")))
  (dashboard-setup-startup-hook))
;; Startup Dashboard:1 ends here

;; Font Configuration

;; I have a lot of fonts commented out right now because I can't decide on which ones to keep lol.


;; [[file:emacs.org::*Font Configuration][Font Configuration:1]]
(defun zamlz/set-font-faces ()
  ;; Set default face
  ;; (set-face-attribute 'default nil :font "xos4 Terminus" :height 110)
  ;; (set-face-attribute 'default nil :font "Fira Code" :height 100)
  ;; (set-face-attribute 'default nil :font "Dina" :height 100)
  (set-face-attribute 'default nil :font "Iosevka Term" :height 110)
  ;; (set-face-attribute 'default nil :font "Source Code Pro" :height 100)

  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil :font "xos4 Terminus" :height 100)

  ;; Set the variable pitch face
  ;; (set-face-attribute 'variable-pitch nil :font "Fira Code" :height 100)
  )
;; Font Configuration:1 ends here

;; Emojify

;; Display emojis within emacs thanks to [[https://github.com/iqbalansari/emacs-emojify][github:iqbalansari/emacs-emojify]].


;; [[file:emacs.org::*Emojify][Emojify:1]]
(use-package emojify)
  ;; :hook (after-init . global-emojify-mode))
;; Emojify:1 ends here

;; Which Key

;; Spawns a simple UI panel that shows available keybindings based on what keys I've pressed so far.


;; [[file:emacs.org::*Which Key][Which Key:1]]
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 1.0))
;; Which Key:1 ends here

;; General Keybinder

;; This is an interesting package. It basically lets me define my own keybinding space and configure it to run various commands as I see fit.


;; [[file:emacs.org::*General Keybinder][General Keybinder:1]]
(use-package general
  :config
  (general-create-definer zamlz/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))
;; General Keybinder:1 ends here

;; Hydra

;; Hydra lets me also define functions that can very quickly let me do various tasks in quick succession. I will be using this a lot of =general= I imagine.


;; [[file:emacs.org::*Hydra][Hydra:1]]
(use-package hydra)
;; Hydra:1 ends here

;; Misc Shortcuts

;; These are just random shorts to emacs built-in commands that I'd like access to as a keybinding.


;; [[file:emacs.org::*Misc Shortcuts][Misc Shortcuts:1]]
(zamlz/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme"))
;; Misc Shortcuts:1 ends here

;; Text Size Scaling

;; Adds a =Hydra= function to =General= to control the size of the font face.


;; [[file:emacs.org::*Text Size Scaling][Text Size Scaling:1]]
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Add hydra func to our personal keybindings
(zamlz/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))
;; Text Size Scaling:1 ends here

;; Prescient

;; Simple but effective sorting and filtering for emacs completion buffers.


;; [[file:emacs.org::*Prescient][Prescient:1]]
(use-package prescient)
;; Prescient:1 ends here

;; Helpful Help Commands

;; [[https://github.com/Wilfred/helpful][Wilfred/helpful]] improves the documentation shown when running one of emacs's =describe-*= functions.


;; [[file:emacs.org::*Helpful Help Commands][Helpful Help Commands:1]]
(use-package helpful
  :after counsel
  :custom
  ; This is only needed if I'm still using counsel
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ("C-c C-d"                 . helpful-at-point)
  ("C-h F"                   . helpful-function)
  )
;; Helpful Help Commands:1 ends here

;; Modeline

;; To use =Doom-Modeline=, we need to have some custom icons installed. However, they must be manually installed via the following command (=M-x all-the-icons-install-fonts=)


;; [[file:emacs.org::*Modeline][Modeline:1]]
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
;; Modeline:1 ends here

;; Color Theme

;; I have a couple themes here. Eventually I want to setup my own custom theme but for now this will have to do.


;; [[file:emacs.org::*Color Theme][Color Theme:1]]
(use-package autothemer
  :ensure t)

(add-to-list 'custom-theme-load-path "~/org/config/lib/emacs-themes/")
;; (load-theme 'gruvbox-black t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-homage-black t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package spacemacs-theme
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))
;; Color Theme:1 ends here

;; Rainbow Delimiters

;; Normally I don't like rainbow delimiters but its actually pretty good on emacs. And you actually can't survive without it IMO.


;; [[file:emacs.org::*Rainbow Delimiters][Rainbow Delimiters:1]]
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Rainbow Delimiters:1 ends here

;; Syntax Highlighting


;; [[file:emacs.org::*Syntax Highlighting][Syntax Highlighting:1]]
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))
;; Syntax Highlighting:1 ends here

;; Rainbow Mode


;; [[file:emacs.org::*Rainbow Mode][Rainbow Mode:1]]
(use-package rainbow-mode
  :init (rainbow-mode))
;; Rainbow Mode:1 ends here

;; Transparency


;; [[file:emacs.org::*Transparency][Transparency:1]]
;; Set transparency of emacs
(defun zamlz/set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Add the transparency function to my leader keys
(zamlz/leader-keys
  "tx" '(zamlz/set-transparency :which-key "Set transparency"))

;; Set the default transparency
(zamlz/set-transparency 80)
;; Transparency:1 ends here

;; Single Dired Buffer

;; This doesn't prevent dired from having multiple buffers open at once, rather, it forces dired to not create a new buffer whenever a new directory is open.


;; [[file:emacs.org::*Single Dired Buffer][Single Dired Buffer:1]]
(use-package dired-single)
;; Single Dired Buffer:1 ends here

;; Icons for Dired


;; [[file:emacs.org::*Icons for Dired][Icons for Dired:1]]
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; Icons for Dired:1 ends here

;; Hide Dotfiles

;; Hide hidden files from dired buffers (toggleable)


;; [[file:emacs.org::*Hide Dotfiles][Hide Dotfiles:1]]
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))
;; Hide Dotfiles:1 ends here

;; Daemon and Client Hooks


;; [[file:emacs.org::*Daemon and Client Hooks][Daemon and Client Hooks:1]]
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (zamlz/set-font-faces)
                  (zamlz/set-transparency 80))))
  (zamlz/set-font-faces))
;; Daemon and Client Hooks:1 ends here

;; Vterm

;; Installs a better virtual terminal to use within emacs. I'm just playing around with this for now.


;; [[file:emacs.org::*Vterm][Vterm:1]]
(use-package vterm
  :custom
  ;; (vterm-shell "/bin/fish")
  (vterm-ignore-blink-cursor nil)
  (vterm-buffer-name-string "vterm [%s]")
  (vterm-always-compile-module t))

(zamlz/leader-keys
  "e" '(:ignore t :which-key "Exec Commands")
  "ee" '(vterm :which-key "Spawn vterm instance"))
;; Vterm:1 ends here

;; Language Server Protocol

;; Powerful languages server protocols that were designed originally for visual studio code in order to created a unified protocol for getting functionality of the langauge from within the editor.


;; [[file:emacs.org::*Language Server Protocol][Language Server Protocol:1]]
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))
;; Language Server Protocol:1 ends here

;; Python


;; [[file:emacs.org::*Python][Python:1]]
(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :config (setq python-indent-offset zamlz/indent-width))
;; Python:1 ends here

;; C/C++/Java

;; Sets the formatting style for C/C++/Java from GNU (default) to the popular standard K&R.


;; [[file:emacs.org::*C/C++/Java][C/C++/Java:1]]
(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset zamlz/indent-width)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))
;; C/C++/Java:1 ends here

;; Web Languages

;; Useful mode for editing web based files


;; [[file:emacs.org::*Web Languages][Web Languages:1]]
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :custom
  (web-mode-markup-indent-offset 2) ; HTML
  (web-mode-css-indent-offset 2)    ; CSS
  (web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))
;; Web Languages:1 ends here

;; Markdown


;; [[file:emacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))
;; Markdown:1 ends here

;; Ledger


;; [[file:emacs.org::*Ledger][Ledger:1]]
(use-package ledger-mode)
;; Ledger:1 ends here

;; Beancount

;; Beancount is a plain text accounting ledger-like program that I've been experimenting with. This simply just adds the mode via **on-disk** extension. If the extension is missing, emacs will complain. Make sure to pull it from the repo!


;; [[file:emacs.org::*Beancount][Beancount:1]]
(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.lgr\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
;; Beancount:1 ends here

;; Company Auto-Completion for Programming Languages

;; Use =C-n= and =C-p= to navigate tooltip


;; [[file:emacs.org::*Company Auto-Completion for Programming Languages][Company Auto-Completion for Programming Languages:1]]
(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        ; show tooltip even for single candidate
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-common)))
;; Company Auto-Completion for Programming Languages:1 ends here

;; Flycheck Syntax Linter


;; [[file:emacs.org::*Flycheck Syntax Linter][Flycheck Syntax Linter:1]]
(use-package flycheck
  :config (global-flycheck-mode +1))
;; Flycheck Syntax Linter:1 ends here

;; Projectile

;; Provides useful integration to a variety of project formats. Able to automatically identify project directories and can be configured to build, run unit-tests, etc. Need to explore this more.


;; [[file:emacs.org::*Projectile][Projectile:1]]
(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))
;; Projectile:1 ends here

;; Diff HL Mode

;; [[https://github.com/dgutov/diff-hl][github:dgutov/diff-hl]] highlights uncommited changes on the left side of text buffers.


;; [[file:emacs.org::*Diff HL Mode][Diff HL Mode:1]]
(use-package diff-hl
  :init (global-diff-hl-mode 1))
;; Diff HL Mode:1 ends here

;; Forge

;; Package provides integration to upstream GitHub, GitLab APIs and more. Setting this up would let me merge/review pull requests, create/address issues, etc. from within emacs.


;; [[file:emacs.org::*Forge][Forge:1]]
;; (use-package forge)
;; Forge:1 ends here

;; Interface


;; [[file:emacs.org::*Interface][Interface:1]]
(require 'init-ivy)
(require 'init-helm)
;; Interface:1 ends here

;; keybinding Improvements


;; [[file:emacs.org::*keybinding Improvements][keybinding Improvements:1]]
(require 'init-evil)
;; keybinding Improvements:1 ends here

;; Development and Project Management Packages:


;; [[file:emacs.org::*Development and Project Management Packages:][Development and Project Management Packages::1]]
(require 'init-magit)
;; Development and Project Management Packages::1 ends here

;; Org Mode Packages:


;; [[file:emacs.org::*Org Mode Packages:][Org Mode Packages::1]]
(require 'init-org)
(require 'init-org-babel)
(require 'init-org-roam)
;; Org Mode Packages::1 ends here

;; PDF Tools

;; A nice standalone replacement for DocView.


;; [[file:emacs.org::*PDF Tools][PDF Tools:1]]
(use-package pdf-tools
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-midnight-colors '("#ebdbb2" . "#000000"))
  :init (pdf-tools-install))
;; PDF Tools:1 ends here

;; xkcd

;; A simple plugin for getting the today's xkcd comic.


;; [[file:emacs.org::*xkcd][xkcd:1]]
(use-package xkcd)
;; xkcd:1 ends here

;; Wttr.in

;; A frontend for [[http://wttr.in/][wttr.in]].


;; [[file:emacs.org::*Wttr.in][Wttr.in:1]]
(use-package wttrin
  :custom
  (wttrin-default-cities '("Union City, CA")))
;; Wttr.in:1 ends here

;; Key Quiz

;; A fun litte package for memorizing and learning keybindings. To play, simply use =M-x key-quiz= which will open up the game. It will default to *Fundamental-Mode* but this can be changed by setting the variable =key-quiz-use-mode=.


;; [[file:emacs.org::*Key Quiz][Key Quiz:1]]
(use-package key-quiz)
;; Key Quiz:1 ends here

;; Mu4e

;; We make use of [[file:isync.org][Isync (mbsync)]] to clone a local copy of the IMAP to use with mu4e.


;; [[file:emacs.org::*Mu4e][Mu4e:1]]
(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup

  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail/samlesh@gmail.com/")

  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")

  (setq mu4e-maildir-shortcuts
    '((:maildir "/inbox"    :key ?i)
      (:maildir "/[Gmail]/Sent Mail" :key ?s)
      (:maildir "/[Gmail]/Trash"     :key ?t)
      (:maildir "/[Gmail]/Drafts"    :key ?d)
      (:maildir "/[Gmail]/All Mail"  :key ?a))))
;; Mu4e:1 ends here

;; Paradox


;; [[file:emacs.org::*Paradox][Paradox:1]]
(use-package paradox
  :config (paradox-enable))
;; Paradox:1 ends here
