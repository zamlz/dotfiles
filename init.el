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

;; ESC should save the day

;; This really doesn't do what I think it does...


;; [[file:emacs.org::*ESC should save the day][ESC should save the day:1]]
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; ESC should save the day:1 ends here

;; Evil Mode

;; This is the core of =Evil-Mode=. This basicallly adds the modal functionality to emacs that we see in vim. But we also add a little bit of our own custom bindings not set by default that were present in vim for the sake of my sanity.


;; [[file:emacs.org::*Evil Mode][Evil Mode:1]]
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; The defaults for Shift-j/k are not inuitive. I even remapped them
  ;; back when I was using vim as well..
  (define-key evil-normal-state-map (kbd "J") nil)
  (define-key evil-normal-state-map (kbd "K") nil)

  ;; We want to override the RET key for other useful things but
  ;; Evil takes control of it because its evil. The same is true
  ;; for SPC and TAB but I'm not sure if I want those yet.
  (define-key evil-motion-state-map (kbd "RET") nil)
  ;(define-key evil-motion-state-map (kbd "SPC") nil)
  ;(define-key evil-motion-state-map (kbd "TAB") nil)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; configure initial states in specific modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;; Evil Mode:1 ends here

;; Evil Collection

;; Unfortunately, =Evil-Mode= is only enabled in text buffers. This means that in special buffers spawned by packages like =Magit=, =Org=, etc., we would not be able to use vim-like bindings. The package below aims to provide even more functionality to these other non-trivial modes.


;; [[file:emacs.org::*Evil Collection][Evil Collection:1]]
(use-package evil-collection
  :after evil
  :config (evil-collection-init))
;; Evil Collection:1 ends here

;; Evil Commentary

;; Adds keybindings for quick commenting. Use =gcc= to comment out a line, use =gcap= to comment out a paragraph, use =gc= in visual mode to comment out a selection.


;; [[file:emacs.org::*Evil Commentary][Evil Commentary:1]]
(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))
;; Evil Commentary:1 ends here

;; Evil Org


;; [[file:emacs.org::*Evil Org][Evil Org:1]]
(use-package evil-org
  :after (evil org)
  :hook (org-mode . (lambda() evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;; Evil Org:1 ends here

;; Evil Visual Marks


;; [[file:emacs.org::*Evil Visual Marks][Evil Visual Marks:1]]
(use-package evil-visual-mark-mode
  :after evil)
  ;; :init (evil-visual-mark-mode))
;; Evil Visual Marks:1 ends here

;; Evil Fringe Marks


;; [[file:emacs.org::*Evil Fringe Marks][Evil Fringe Marks:1]]
(use-package evil-fringe-mark
  :after evil
  :custom
  ;; (right-fringe-width 16)
  (evil-fringe-mark-side 'right-fringe)
  (evil-fringe-mark-show-special t)
  :config
  (global-evil-fringe-mark-mode))
;; Evil Fringe Marks:1 ends here

;; Evil Goggles

;; All evil operations now have visual hints.


;; [[file:emacs.org::*Evil Goggles][Evil Goggles:1]]
;; (use-package evil-goggles
;;   :after evil
;;   :init (evil-goggles-mode)
;;   :config (evil-goggles-use-diff-faces))
;; Evil Goggles:1 ends here

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

;; Ivy

;; =Ivy= is basically a completion framework. Its minimal but provides a simple but powerful menu that appears when switching files, opening buffers, etc.


;; [[file:emacs.org::*Ivy][Ivy:1]]
(use-package ivy
  :defer 0.1
  :init
  ;; Change completion method (not working as expected)
  ;; This needs to be in the init it seems othrewise, it doesn't get loaded...
  (setq ivy-re-builders-alist `((t . ivy--regex-ignore-order)))
  (ivy-mode)
  :bind (:map ivy-minibuffer-map
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
  ;; Default count format
  (ivy-count-format "[%d/%d] ")
  ;; Don't start searches with ^
  (ivy-initial-inputs-alist nil)
  ;; Show recentf files in buffer switch
  (ivy-use-virtual-buffers nil)
  ;; Show the full virtual file paths
  (ivy-virtual-abbreviate 'full)
  ;; Avoid displaying things like "./" and "../" in the list
  (ivy-extra-directories nil)
  ;; Set the height of the ivy minibuffer
  (ivy-height 20))
;; Ivy:1 ends here

;; Counsel

;; =Counsel= on the other hand, provides replaces commands that replace the typical emacs commands.


;; [[file:emacs.org::*Counsel][Counsel:1]]
(use-package counsel
  :after ivy
  :bind (
         ("M-x"       . counsel-M-x)
         ("C-x TAB"   . counsel-semantic-or-imenu)
         ("C-x b"     . counsel-switch-buffer)
         ("M-y"       . counsel-yank-pop)
         ("M-o"       . counsel-recentf)
         ("M-m"       . counsel-evil-marks)
         ("C-x B"     . counsel-switch-buffer-other-window)
         ("C-x C-f"   . counsel-find-file)
         ("C-x C-M-f" . counsel-find-file-extern)
         ("C-x C-l"   . counsel-locate)
         ("C-x C-M-l" . counsel-locate-action-extern)
         ("C-x C-v"   . counsel-set-variable)
         ("C-c u"     . counsel-unicode-char)
         :map minibuffer-local-map
         ("C-r"       . 'counsel-minibuffer-history))
  :config (counsel-mode)
  )
;; Counsel:1 ends here

;; Swiper


;; [[file:emacs.org::*Swiper][Swiper:1]]
(use-package swiper
  :after counsel
  :bind (("C-s"   . swiper)
         ("C-M-s" . swiper-all)))
;; Swiper:1 ends here

;; Ivy Rich

;; =Ivy-Rich= provides extra columns in the counsel commands to get more information about each item during autocompletion.


;; [[file:emacs.org::*Ivy Rich][Ivy Rich:1]]
;; Adds nice icons to the ivy rich buffer
(use-package all-the-icons-ivy-rich
  :after (counsel counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1))

;; Actually install ivy rich
(use-package ivy-rich
  :after (counsel all-the-icons-ivy-rich)
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  :init (ivy-rich-mode 1))
;; Ivy Rich:1 ends here

;; Ivy Posframe

;; [[https://github.com/tumashu/ivy-posframe][tumashu/ivy-postframe]] is ivy extension, which let ivy use the postframe to show its candidate menu.


;; [[file:emacs.org::*Ivy Posframe][Ivy Posframe:1]]
;; (use-package ivy-posframe
;;   :after counsel
;;   :custom
;;   ;; Specify the the display posframe
;;   (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-window-center)))
;;   ;; Customize height and width of the posframe
;;   (ivy-posframe-height 20)
;;   (ivy-posframe-min-height 5)
;;   (ivy-posframe-width 120)
;;   (ivy-posframe-min-width 120)
;;   (ivy-posframe-border-width 20)
;;   :init
;;   (ivy-posframe-mode 1)
;; Ivy Posframe:1 ends here

;; Ivy Hydra

;; Ivy Hydra is part of the original repo, but its bundled seperately as a package. These lets us drop into a hydra session while we are in a ivy minibuffer.


;; [[file:emacs.org::*Ivy Hydra][Ivy Hydra:1]]
(use-package ivy-hydra
  :after (ivy hydra))
;; Ivy Hydra:1 ends here

;; Ivy Bibtex


;; [[file:emacs.org::*Ivy Bibtex][Ivy Bibtex:1]]
(use-package ivy-bibtex
  :after ivy
  :bind (("C-c n p"   . ivy-bibtex)
         ("C-c n C-p" . ivy-bibtex-with-notes))
  :custom
  (bibtex-completion-bibliography `((,(directory-files-recursively "~/org/papers/bib/" ""))))
  (bibtex-completion-library-path '("~/org/papers/doc/"))
  (bibtex-completion-notes-path "~/org/papers/notes/")
  ;; Style the output indicators
  (bibtex-completion-pdf-symbol "⌘")
  (bibtex-completion-notes-symbol "✎")
  ;; TODO Use bibtex-completion-additional-search-fields
  (bibtex-completion-notes-template-multiple-files
   (concat "#+TITLE: Notes on \"${title}\" by ${author-or-editor} (${year})\n"
           "#+AUTHOR: %n (%(user-login-name))\n"
           "#+ROAM_ALIAS:\n"
           "#+ROAM_TAGS:\n"
           "#+ROAM_KEY: cite:${=key=}\n"
           "#+CREATED: %U\n"
           "#+LAST_MODIFIED: %U\n"))
  )
;; Ivy Bibtex:1 ends here

;; Ivy Pass


;; [[file:emacs.org::*Ivy Pass][Ivy Pass:1]]
(use-package ivy-pass
  :after ivy
  :bind ("C-x C-p" . ivy-pass))
;; Ivy Pass:1 ends here

;; Ivy Prescient


;; [[file:emacs.org::*Ivy Prescient][Ivy Prescient:1]]
(use-package ivy-prescient
  :after (ivy prescient))
;; Ivy Prescient:1 ends here

;; Counsel Projectile

;; Provides counsel interface for projectile.


;; [[file:emacs.org::*Counsel Projectile][Counsel Projectile:1]]
(use-package counsel-projectile
  :after counsel
  :init (counsel-projectile-mode))
;; Counsel Projectile:1 ends here

;; Helm

;; Trying out helm. Got most of the stuff on how this works from [[https://tuhdo.github.io/helm-intro.html][this guide]].


;; [[file:emacs.org::*Helm][Helm:1]]
(use-package helm
  :bind (
  ;;        ("M-x"     . helm-M-x)
  ;;        ("M-y"     . helm-show-kill-ring)
  ;;        ("C-x b"   . helm-mini)
  ;;        ("C-x C-f" . helm-find-files)
  ;;        ("C-x C-l" . helm-locate)
  ;;        ("C-x r b" . helm-bookmarks)
  ;;        ;; ("C-c h"   . helm-command-prefix)
  ;;        ("C-x TAB" . helm-semantic-or-imenu)
  ;;        ("C-s"     . helm-occur)
  ;;        :map helm-map
  ;;        ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
  ;;        ("C-i"   . helm-execute-persistent-action) ; make TAB work in terminal
  ;;        ("C-z"   . helm-select-action) ; list actions using C-z
         )
  :custom
  ; max height for the helm buffer
  (helm-autoresize-max-height 30)
  ; min height for the helm buffer
  (helm-autoresize-min-height 0)
  ; open helm buffer inside current window, not occupy whole other window
  (helm-split-window-in-side-p t)
  ; move to end or beginning of source when reaching top or bottom of source.
  ;; (helm-move-to-line-cycle-in-source t)
  ; search for library in `require' and `declare-function' sexp.
  (helm-ff-search-library-in-sexp t)
  ; scroll 8 lines other window using M-<next>/M-<prior>
  (helm-scroll-amount 8)
  ;; use recentf-list for recent files
  (helm-ff-file-name-history-use-recentf t)
  ;; show current input in header line
  (helm-echo-input-in-header-line t)
  ;; enable fuzzy searching in semantic-or-imenu
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  ;; enable fuzzy matching in buffer list
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  :config
  (require 'helm-config)
  ;; Use curl when found
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (helm-autoresize-mode 1)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; (helm-mode 1)
  )
;; Helm:1 ends here

;; Helm Describe Bindings

;; A useful package for figuring out what bindings there is in a particular major/minor mode.


;; [[file:emacs.org::*Helm Describe Bindings][Helm Describe Bindings:1]]
;; (use-package helm-descbinds
;;   :bind ([remap describe-bindings] . helm-descbinds))
;; Helm Describe Bindings:1 ends here

;; Helm Describe Mode

;; [[https://github.com/emacs-helm/helm-describe-modes/tree/11fb36af119b784539d31c6160002de1957408aa][github:emacs-helm/helm-describe-modes]]


;; [[file:emacs.org::*Helm Describe Mode][Helm Describe Mode:1]]
;; (use-package helm-describe-modes
;;   ;; This is just bound to "C-h m"
;;   :bind ([remap describe-mode] . helm-describe-modes))
;; Helm Describe Mode:1 ends here

;; Helm Bibtex

;; [[https://github.com/tmalsburg/helm-bibtex][github:tmalsburg/helm-bibtex]]


;; [[file:emacs.org::*Helm Bibtex][Helm Bibtex:1]]
;; (use-package helm-bibtex
;;   :after helm
;;   :custom
;;   ;; Helm specific UI changes
;;   (helm-bibtex-full-frame nil)
;;   ;; Currently this points to my old pubs paper archive
;;   (bibtex-completion-bibliography '("~/org/papers/bibliography.bib"))
;;   (bibtex-completion-library-path '("~/org/papers/pdfs/"))
;;   ;; Store my paper notes alongside my roam notes stuff
;;   (bibtex-completion-notes-path "~/org/papers/")
;;   ;; Style the output indicators
;;   (bibtex-completion-pdf-symbol "⌘")
;;   (bibtex-completion-notes-symbol "✎")
;;   ;; TODO Use bibtex-completion-additional-search-fields
;;   )
;; Helm Bibtex:1 ends here

;; Helm Dictioary

;; [[https://github.com/emacs-helm/helm-dictionary][github:emacs-helm/helm-dictionary]]


;; [[file:emacs.org::*Helm Dictioary][Helm Dictioary:1]]
(use-package helm-dictionary
  :after helm
  :bind ("C-c h d" . helm-dictionary))
;; Helm Dictioary:1 ends here

;; Helm Org Rifle

;; [[https://github.com/alphapapa/org-rifle][github:alphapapa/org-rifle]]. We shall see if I truly find this useful or not.


;; [[file:emacs.org::*Helm Org Rifle][Helm Org Rifle:1]]
(use-package helm-org-rifle
  :after helm)
;; Helm Org Rifle:1 ends here

;; Helm Themes

;; [[https://github.com/emacsorphanage/helm-themes][github:emacsorphanage/helm-themes]]


;; [[file:emacs.org::*Helm Themes][Helm Themes:1]]
(use-package helm-themes
  :after helm
  :bind (("C-c h C-t" . helm-themes)))
;; Helm Themes:1 ends here

;; Helm Spotify Plus

;; [[https://github.com/wandersoncferreira/helm-spotify-plus][github:wandersoncferreira/helm-spotify-plus]]


;; [[file:emacs.org::*Helm Spotify Plus][Helm Spotify Plus:1]]
;; (use-package helm-spotify-plus
;;   :after helm
;;   :bind ("C-c h C-s" . helm-spotify-plus))
;; Helm Spotify Plus:1 ends here

;; Helm Pass

;; [[https://github.com/emacs-helm/helm-pass/][github:emacs-helm/helm-pass]]


;; [[file:emacs.org::*Helm Pass][Helm Pass:1]]
(use-package helm-pass
  :after helm)
  ;; :bind ("C-x C-p" . helm-pass))
;; Helm Pass:1 ends here

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

;; Magit Git Interface

;; Git interface for emacs. Very quickly run git commands and evaluate diffs, etc.


;; [[file:emacs.org::*Magit Git Interface][Magit Git Interface:1]]
(use-package magit
  :hook (with-editor-mode . evil-insert-state)
  :bind ("C-x g" . magit-status))
;; Magit Git Interface:1 ends here

;; Magit TODOs


;; [[file:emacs.org::*Magit TODOs][Magit TODOs:1]]
(use-package magit-todos
  :after magit
  :init (magit-todos-mode))
;; Magit TODOs:1 ends here

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

;; Better Font Faces Hook

;; We can configure =org-mode= font faces via a seperate function that we define. We need to add it as a hook afterwards however which is why this section appears before the basic configuration.


;; [[file:emacs.org::*Better Font Faces Hook][Better Font Faces Hook:1]]
(defun zamlz/org-font-setup ()
  ;; Converts bullet lists to not use the - character but the • character
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; ;; Set faces for heading levels
  ;; ;; for now, keep all at 1.0
  ;; (dolist (face '((org-level-1 . 3.0)
  ;;   (org-level-2 . 2.5)
  ;;   (org-level-3 . 2.0)
  ;;   (org-level-4 . 1.5)
  ;;   (org-level-5 . 1.0)
  ;;   (org-level-6 . 1.0)
  ;;   (org-level-7 . 1.0)
  ;;   (org-level-8 . 1.0)))
  ;;   (set-face-attribute (car face) nil
  ;;                       :font "Fira Code"
  ;;                       :weight 'regular
  ;;                       :height (cdr face)))

  ;;   ;; ensure that anything that should be fixed-width in org appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch)
  ;; (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
  )
;; Better Font Faces Hook:1 ends here

;; Mode Startup Hooks

;; These are some hooks that we would like to run when =org-mode= is started.


;; [[file:emacs.org::*Mode Startup Hooks][Mode Startup Hooks:1]]
(defun zamlz/org-mode-setup ()
  (org-indent-mode)
  (org-make-toc-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode +1)
  (setq evil-auto-indent nil)
  (setq fill-column 10000000))
;; Mode Startup Hooks:1 ends here

;; Basic Configuration

;; I should probably split this bloody mess up.


;; [[file:emacs.org::*Basic Configuration][Basic Configuration:1]]
(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . zamlz/org-mode-setup))
  :bind (:map org-mode-map
              ("C-M-h" . org-previous-link)
              ("C-M-l" . org-next-link))
  :custom

  ;; Setup directories
  (org-directory "~/org/gtd/")
  (org-agenda-files (list org-directory))

  ;; Add some nice visuals changes
  (org-ellipsis " ▾")

  ;; These will be unhidden by the org-appear package
  (org-hide-emphasis-markers t)

  ;; This is so that the imenu displays all levels in ivy
  (org-imenu-depth 10)

  ;; dont use C-c C-o for opening links REEEEEEEEEEEE
  (org-return-follows-link t)

  ;; Some todo/logging changes
  (org-enforce-todo-dependencies t)
  (org-log-done t)
  (org-log-into-drawer t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Setup org capture mode
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

  ;; Inline Image improvements
  (org-startup-with-inline-images t)
  (org-image-actual-width 500)

  ;; Finally a post setup func to setup fonts
  (zamlz/org-font-setup))
;; Basic Configuration:1 ends here

;; Task Categories


;; [[file:emacs.org::*Task Categories][Task Categories:1]]
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "ROUTINE(r)" "|" "DONE(d)")
              (sequence "PROJECT(p)" "|" "COMPLETED(d)" "CANCELLED(c)")
              (sequence "WAITING(w)" "|")
              (sequence "|" "CANCELLED(c)")
              (sequence "SOMEDAY(s)" "|" "CANCELLED(c)")
              (sequence "MEETING(m)" "|"))))
;; Task Categories:1 ends here

;; Task Colors

;; The first version here works nice with the =doom-nord= theme. The second version here works better with my custom gruvbox theme.


;; [[file:emacs.org::*Task Colors][Task Colors:1]]
;; (setq org-todo-keyword-faces
;;   '(("DONE"      . (:background "#98be65" :foreground "#ffffff" :weight bold))
;;     ("TODO"      . (:background "#ff6c6b" :foreground "#ffffff" :weight bold))
;;     ("ROUTINE"   . (:background "#3f444a" :foreground "#51afef" :weight bold))
;;     ("PROJECT"   . (:background "#51afef" :foreground "#ffffff" :weight bold))
;;     ("COMPLETED" . (:background "#98be65" :foreground "#ffffff" :weight bold))
;;     ("WAITING"   . (:background "#3f444a" :foreground "#ffffff" :weight bold))
;;     ("CANCELLED" . (:background "#181818" :foreground "#ffffff" :weight bold))
;;     ("SOMEDAY"   . (:background "#3f444a" :foreground "#ffffff" :weight bold))
;;     ("MEETING"   . (:background "#a9a1e1" :foreground "#ffffff" :weight bold))))
;; Task Colors:1 ends here

;; [[file:emacs.org::*Task Colors][Task Colors:2]]
(setq org-todo-keyword-faces
  '(("DONE"      . (:background "#b8bb26" :foreground "#000000" :weight bold))
    ("TODO"      . (:background "#fb4934" :foreground "#000000" :weight bold))
    ("ROUTINE"   . (:background "#689d6a" :foreground "#000000" :weight bold))
    ("PROJECT"   . (:background "#458588" :foreground "#000000" :weight bold))
    ("COMPLETED" . (:background "#b8bb26" :foreground "#000000" :weight bold))
    ("WAITING"   . (:background "#d79921" :foreground "#000000" :weight bold))
    ("CANCELLED" . (:background "#181818" :foreground "#fb4934" :weight bold))
    ("SOMEDAY"   . (:background "#3f444a" :foreground "#000000" :weight bold))
    ("MEETING"   . (:background "#b16286" :foreground "#000000" :weight bold))))
;; Task Colors:2 ends here

;; Better Heading Bullets

;; The default bullets are asteriks and thats just plain ugly. We change it to some nice unicode characters. I should also look into =org-superstar-mode= as well at some point.


;; [[file:emacs.org::*Better Heading Bullets][Better Heading Bullets:1]]
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))
  ;; (org-bullets-bullet-list '("◇")))
;; Better Heading Bullets:1 ends here

;; Center Org buffers

;; Center the org buffers and remove line numbers to reduce visual clutter.


;; [[file:emacs.org::*Center Org buffers][Center Org buffers:1]]
(defun zamlz/org-mode-visual-fill ()
  (setq visual-fill-column-width zamlz/default-screen-width
        ;; visual-fill-column-extra-text-width (0 . 1000)
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

 (use-package visual-fill-column
   :hook (org-mode . zamlz/org-mode-visual-fill))
;; Center Org buffers:1 ends here

;; Default Keybindings

;; Just some keybindings to access various =org-mode= commands.


;; [[file:emacs.org::*Default Keybindings][Default Keybindings:1]]
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; Default Keybindings:1 ends here

;; Org Mode Quick Access

;; Just quick access to my most used org-mode functions via the leader key.


;; [[file:emacs.org::*Org Mode Quick Access][Org Mode Quick Access:1]]
(zamlz/leader-keys
  "o"  '(:ignore t :which-key "Org Mode")
  "oa" '(org-agenda-list :which-key "Org Agenda Weekly View")
  "oo" '(org-capture :which-key "Org Capture Templates")
  "oi" '((lambda () (interactive)
           (find-file (concat org-directory "/inbox.org"))
           (message "Opened:  %s" (buffer-name)))
         :which-key "Inbox")
  "og" '((lambda () (interactive)
           (find-file (concat org-directory "/gtd.org"))
           (message "Opened:  %s" (buffer-name)))
         :which-key "GTD")
  "oj" '((lambda () (interactive)
           (find-file (concat org-directory "/journal.org"))
           (message "Opened:  %s" (buffer-name)))
         :which-key "Journal"))
;; Org Mode Quick Access:1 ends here

;; DOCT: Declarative Org Capture Templates

;; DOCT is a cool package that lets me build =org-capture= templates with ease. It's really way more readable than the original syntax.


;; [[file:emacs.org::*DOCT: Declarative Org Capture Templates][DOCT: Declarative Org Capture Templates:1]]
(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))
;; DOCT: Declarative Org Capture Templates:1 ends here

;; Template Definitions

;; Org mode template definitions in the doct format


;; [[file:emacs.org::*Template Definitions][Template Definitions:1]]
(setq org-capture-templates
      (doct '(("Todo, Meetings, Projects and more!" :keys "t"
               :file "inbox.org"
               :type entry
               :prepend t
               :template ("* %{todo-state} %^{Description}"
                          ":PROPERTIES:"
                          ":Created: %U"
                          ":END:"
                          "%?")
               :children (("Todo Task" :keys "t"
                           :todo-state "TODO")
                          ("Routine/Habit" :keys "r"
                           :todo-state "ROUTINE")
                          ("Project Group" :keys "p"
                           :todo-state "PROJECT")
                          ("Someday/Maybe" :keys "s"
                           :todo-state "SOMEDAY")
                          ("Meeting/Appointment" :keys "m"
                           :todo-state "MEETING")
                          ("Todo Task (context)" :keys "i"
                           :todo-state "TODO"
                           :template ("* %{todo-state} %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "%?"
                                      "%i"
                                      "%a"))))
              ("Journal Entries and Data Capture" :keys "j"
               :file "journal.org"
               :type entry
               :datetree t
               :template ("* %U :JOURNAL:%{extra-tags}"
                          "%?")
               :children (("Journal (now)" :keys "j"
                           :extra-tags "REALITY:")
                          ("Journal (dream)" :keys "d"
                           :extra-tags "DREAM:")
                          ("Journal (context)" :keys "i"
                           :template ("* %U :JOURNAL:CONTEXT:"
                                      "%?"
                                      "%i"
                                      "%a")))))))
;; Template Definitions:1 ends here

;; Org Make TOC

;; Dynamically make table of contents in org files


;; [[file:emacs.org::*Org Make TOC][Org Make TOC:1]]
(use-package org-make-toc)
;; Org Make TOC:1 ends here

;; Literate Programming with Babel

;; Babel is basically like jupyter notebooks for =org-mode= but its also way more powerful! Below we also configure quick access structure templates to write src blocks with ease. Babel is already a part of emacs so we must just configure it. To get a list of what languages you can configure for babel, take a look [[https://orgmode.org/worg/org-contrib/babel/languages/index.html][here]]!


;; [[file:emacs.org::*Literate Programming with Babel][Literate Programming with Babel:1]]
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
(add-to-list `org-structure-template-alist '("cf" . "src conf"))
;; Literate Programming with Babel:1 ends here

;; Auto-Tangle Configuration File

;; We make use of =org-babel= here to create our config file. This is currently also checking to my dotfiles in case of system rescue reasons.


;; [[file:emacs.org::*Auto-Tangle Configuration File][Auto-Tangle Configuration File:1]]
;; (defun efs/org-babel-tangle-config ()
;;   (when (string-equal (buffer-file-name)
;;                       (expand-file-name "~/etc/emacs/config.org"))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
;; Auto-Tangle Configuration File:1 ends here

;; LaTeX Inline Preview

;; Pretty straightforward except that you need to have =latex= installed and also =dvi2png= as well. Need to figure out how to configure the size of the generated latex image.


;; [[file:emacs.org::*LaTeX Inline Preview][LaTeX Inline Preview:1]]
(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
(setq org-preview-latex-image-directory  "/tmp/ltximg/")
;; LaTeX Inline Preview:1 ends here

;; Habit Tracking

;; Enables habit tracking in =org-mode= via =org-habit=.


;; [[file:emacs.org::*Habit Tracking][Habit Tracking:1]]
(add-to-list 'org-modules 'org-habit t)
(setq org-habit-preceding-days 31)
(setq org-habit-following-days 3)
(setq org-habit-show-habits-only-for-today t)
;;(setq org-habit-show-all-today t)
;; Habit Tracking:1 ends here

;; Org Download

;; This package should let me quickly download images from web browsers and have images in my clipboard and paste them into my org files.


;; [[file:emacs.org::*Org Download][Org Download:1]]
(use-package org-download
  :custom
  (org-download-image-dir "./data")
  (org-download-heading-lvl nil)
  (org-download-method 'directory))
;; Org Download:1 ends here

;; Org Appear

;; Added [[https://github.com/awth13/org-appear][github:awth13/org-appear]]


;; [[file:emacs.org::*Org Appear][Org Appear:1]]
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoentities nil)
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers nil))
;; Org Appear:1 ends here

;; Org FragTog

;; [[https://github.com/io12/org-fragtog][github:io12/org-fragtog]] basically toggles  latex fragements when you hover over them.


;; [[file:emacs.org::*Org FragTog][Org FragTog:1]]
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))
;; Org FragTog:1 ends here

;; Org Last Modified Timestamp

;; The following allows any =#+LAST_MODIFIED= headers to be updated on file-save.


;; [[file:emacs.org::*Org Last Modified Timestamp][Org Last Modified Timestamp:1]]
(defun zamlz/update-org-modified-property ()
  "If a file contains a '#+LAST_MODIFIED' property update it to contain
  the current date/time"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LAST_MODIFIED:" (point-max) t)
      (progn
        (kill-line)
        (insert (format-time-string " [%Y-%m-%d %a %H:%M:%S]") )))))
;; Org Last Modified Timestamp:1 ends here



;; It is made available like so.


;; [[file:emacs.org::*Org Last Modified Timestamp][Org Last Modified Timestamp:2]]
(defun zamlz/org-mode-before-save-hook ()
  (when (eq major-mode 'org-mode)
    (zamlz/update-org-modified-property)))

(add-hook 'before-save-hook #'zamlz/org-mode-before-save-hook)
;; Org Last Modified Timestamp:2 ends here

;; Roam Notes

;; =org-roam= is a useful package for taking notes.


;; [[file:emacs.org::*Roam Notes][Roam Notes:1]]
(use-package org-roam
  :ensure t
  :after org
  :hook (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
         ;; Standard Roam Stuff
         ("C-c n l" . org-roam)
         ("C-c n /" . org-roam-find-file)
         ;; Graph Functions
         ("C-c n g" . org-roam-graph)
         ("C-c n b" . org-roam-db-build-cache)
         ;; Quick access to Dired buffers
         ("C-c n f" . org-roam-find-directory)
         ("C-c n F" . org-roam-dailies-find-directory)
         ;; Daily Journal
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n y" . org-roam-dailies-capture-yesterday)
         ("C-c n d" . org-roam-dailies-capture-date)
         ;; Org Roam File Mods
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         :map org-mode-map
         ;; Insert notes...
         ("M-i" . org-roam-insert)
         ("M-I" . org-roam-insert-immediate))
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "private/journal/")
  (org-roam-file-exclude-regexp "README.org")
  (org-roam-db-udpate-method 'immediate))
;; Roam Notes:1 ends here

;; Roam Capture Templates


;; [[file:emacs.org::*Roam Capture Templates][Roam Capture Templates:1]]
(setq org-roam-capture-templates
      `(("d" "default" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "notes/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("p" "private" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "private/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("c" "config" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "config/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS: CONFIG SOFTWARE\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("t" "talks/lectures" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "talks/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS: \n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("w" "webpages/bookmarks" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "bookmarks/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS: BOOKMARK\n"
                        "#+ROAM_KEY: %x\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("b" "blogpost" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "blog/${slug}"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS:\n"
                        "#+ROAM_TAGS: BLOGPOST\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ))
;; Roam Capture Templates:1 ends here

;; Roam Daily Notes


;; [[file:emacs.org::*Roam Daily Notes][Roam Daily Notes:1]]
(setq org-roam-dailies-capture-templates
      `(("d" "default" entry
         #'org-roam-capture--get-point
         "* %U\n%?"
         :file-name "private/journal/%<%Y-%m-%d>"
         :head ,(concat "#+TITLE: %<[%Y-%m-%d] %B %e, %Y (%A)>\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_ALIAS: %<%Y-%m-%d>\n"
                        "#+ROAM_TAGS: JOURNAL\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n"
                        "\n"))
        ))
;; Roam Daily Notes:1 ends here

;; Roam Quick Access


;; [[file:emacs.org::*Roam Quick Access][Roam Quick Access:1]]
;; (zamlz/leader-keys
;;   "n"  '(:ignore t :which-key "Org Roam Notes")
;;   "nj" '(org-roam-dailies-capture-today :which-key "Roam Daily Capture Today")
;;   "ny" '(org-roam-dailies-capture-yesterday :which-key "Roam Daily Capture Yesterday"))
;; Roam Quick Access:1 ends here

;; Roam Server

;; =org-roam-server= provides a useful way to view my notes in a graph like view. I can also open them from this view as well.


;; [[file:emacs.org::*Roam Server][Roam Server:1]]
(use-package org-roam-server
  :ensure t
  :custom
  (org-roam-server-host "127.0.0.1")
  (org-roam-server-port 8080
  (org-roam-server-authenticate nil)
  (org-roam-server-export-inline-images t)
  (org-roam-server-serve-files t)
  (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "png" "svg"))
  (org-roam-server-network-poll t)
  (org-roam-server-network-arrows t)
  (org-roam-server-network-label-truncate t)
  (org-roam-server-network-label-truncate-length 60)
  (org-roam-server-network-label-wrap-length 20))
  :config (org-roam-server-mode))
;; Roam Server:1 ends here

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
