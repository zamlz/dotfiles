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
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))

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

(use-package "startup"
  :ensure nil
  :custom (inhibit-startup-screen t))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

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
  :custom
  (create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (backup-directory-alist `(("." . "~/.emacs.d/backup"))))

(use-package autorevert
  :ensure nil
  :config
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 0.4))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :custom (show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  ;; disable <> auto-pairing in org-mode buffers
  (org-mode  . (lambda ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package cus-edit
  :ensure nil
  :custom (custom-file (concat user-emacs-directory "to-be-dumped.el")))

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

(use-package epa-file
  :ensure nil
  :custom
  (epa-file-select-keys nil)
  (epa-file-encrypt-to user-mail-address))

(use-package auth-source-pass
  :ensure nil
  :init (auth-source-pass-enable))

;; Enable custom dashboard
(use-package dashboard
  :ensure t
  :custom
  ;; (dashboard-startup-banner "~/lib/emacs-themes/navi.png")
  (dashboard-startup-banner "~/lib/emacs-themes/black-hole.png")
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

(use-package emojify)
  ;; :hook (after-init . global-emojify-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . (lambda() evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-visual-mark-mode
  :after evil)
  ;; :init (evil-visual-mark-mode))

(use-package evil-fringe-mark
  :after evil
  :custom
  ;; (right-fringe-width 16)
  (evil-fringe-mark-side 'right-fringe)
  (evil-fringe-mark-show-special t)
  :config
  (global-evil-fringe-mark-mode))

(use-package evil-goggles
  :after evil
  :init (evil-goggles-mode)
  :config (evil-goggles-use-diff-faces))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 1.0))

(use-package general
  :config
  (general-create-definer zamlz/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package hydra)

(zamlz/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tt" '(helm-themes :which-key "choose theme"))

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

(use-package swiper
  :after counsel
  :bind (("C-s"   . swiper)
         ("C-M-s" . swiper-all)))

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

;; (use-package ivy-posframe
;;   :after counsel
;;   :custom
;;   ;; Specify the the display posframe
;;   (ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   ;; (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   ;; (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;;   ;; (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;;   ;; (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;;   ;; (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;;   ;; Customize size and width of the posframe
;;   (ivy-posframe-height 30)
;;   (ivy-posframe-min-width 120)
;;   (ivy-posframe-min-height 2)
;;   :init
;;   (ivy-posframe-mode 1))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package ivy-bibtex
  :after ivy
  :bind (("C-c n p"   . ivy-bibtex)
         ("C-c n C-p" . ivy-bibtex-with-notes))
  :custom
  (bibtex-completion-bibliography '("~/org/papers/bibliography.bib"))
  (bibtex-completion-library-path '("~/org/papers/pdfs/"))
  (bibtex-completion-notes-path "~/org/papers/")
  ;; Style the output indicators
  (bibtex-completion-pdf-symbol "⌘")
  (bibtex-completion-notes-symbol "✎")
  ;; TODO Use bibtex-completion-additional-search-fields
  )

(use-package ivy-pass
  :after ivy
  :bind ("C-x C-p" . ivy-pass))

(use-package ivy-prescient
  :after (ivy prescient))

(use-package counsel-projectile
  :after counsel
  :bind ("C-x p" . counsel-projectile-switch-project)
  :init (counsel-projectile-mode))

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

;; (use-package helm-descbinds
;;   :bind ([remap describe-bindings] . helm-descbinds))

;; (use-package helm-describe-modes
;;   ;; This is just bound to "C-h m"
;;   :bind ([remap describe-mode] . helm-describe-modes))

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

(use-package helm-dictionary
  :after helm
  :bind ("C-c h d" . helm-dictionary))

(use-package helm-org-rifle
  :after helm)

(use-package helm-themes
  :after helm
  :bind (("C-c h C-t" . helm-themes)))

;; (use-package helm-spotify-plus
;;   :after helm
;;   :bind ("C-c h C-s" . helm-spotify-plus))

(use-package helm-pass
  :after helm)
  ;; :bind ("C-x C-p" . helm-pass))

(use-package prescient)

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

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package autothemer
  :ensure t)

(add-to-list 'custom-theme-load-path "~/lib/emacs-themes/")
(load-theme 'gruvbox-black t)

;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-homage-black t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package spacemacs-theme
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package rainbow-mode
  :init (rainbow-mode))

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

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (zamlz/set-font-faces)
                  (zamlz/set-transparency 80))))
  (zamlz/set-font-faces))

(use-package vterm
  :custom
  ;; (vterm-shell "/bin/fish")
  (vterm-ignore-blink-cursor nil)
  (vterm-buffer-name-string "vterm [%s]")
  (vterm-always-compile-module t))

(zamlz/leader-keys
  "e" '(:ignore t :which-key "Exec Commands")
  "ee" '(vterm :which-key "Spawn vterm instance"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :config (setq python-indent-offset zamlz/indent-width))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset zamlz/indent-width)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

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

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package ledger-mode)

(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.lgr\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)

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

(use-package flycheck
  :config (global-flycheck-mode +1))

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

(use-package magit
  :hook (with-editor-mode . evil-insert-state)
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :after magit
  :init (magit-todos-mode))

(use-package diff-hl
  :init (global-diff-hl-mode 1))

;; (use-package forge)

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

(defun zamlz/org-mode-setup ()
  (org-indent-mode)
  (org-make-toc-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode +1)
  (setq evil-auto-indent nil)
  (setq fill-column 10000000))

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

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "ROUTINE(r)" "|" "DONE(d)")
              (sequence "PROJECT(p)" "|" "COMPLETED(d)" "CANCELLED(c)")
              (sequence "WAITING(w)" "|")
              (sequence "|" "CANCELLED(c)")
              (sequence "SOMEDAY(s)" "|" "CANCELLED(c)")
              (sequence "MEETING(m)" "|"))))

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))
  ;; (org-bullets-bullet-list '("◇")))

(defun zamlz/org-mode-visual-fill ()
  (setq visual-fill-column-width zamlz/default-screen-width
        ;; visual-fill-column-extra-text-width (0 . 1000)
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

 (use-package visual-fill-column
   :hook (org-mode . zamlz/org-mode-visual-fill))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

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

(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

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

(use-package org-make-toc)

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

;; (defun efs/org-babel-tangle-config ()
;;   (when (string-equal (buffer-file-name)
;;                       (expand-file-name "~/etc/emacs/config.org"))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
(setq org-preview-latex-image-directory  ".ltximg/")

(add-to-list 'org-modules 'org-habit t)
(setq org-habit-preceding-days 31)
(setq org-habit-following-days 3)
(setq org-habit-show-habits-only-for-today t)
;;(setq org-habit-show-all-today t)

(use-package org-download
  :custom
  (org-download-image-dir "./data")
  (org-download-heading-lvl nil)
  (org-download-method 'directory))

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

(defun zamlz/org-mode-before-save-hook ()
  (when (eq major-mode 'org-mode)
    (zamlz/update-org-modified-property)))

(add-hook 'before-save-hook #'zamlz/org-mode-before-save-hook)

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
         :map org-mode-map
         ;; Insert notes...
         ("C-c n i" . org-roam-insert)
         ("C-c n I" . org-roam-insert-immediate))
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "journal/")
  (org-roam-db-udpate-method 'immediate))

(setq org-roam-capture-templates
      `(("d" "default" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "${slug}-%<%Y%m%d%H%M%S>"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_TAGS:\n"
                        "#+ROAM_ALIAS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("i" "infrastructure" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "infra/${slug}-%<%Y%m%d%H%M%S>"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_TAGS: INFRASTRUCTURE\n"
                        "#+ROAM_ALIAS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("c" "contacts" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "contacts/${slug}-%<%Y%m%d%H%M%S>"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_TAGS: CONTACTS\n"
                        "#+ROAM_ALIAS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n")
         :unnarrowed t)
        ("t" "talks/lectures" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "talks/${slug}-%<%Y%m%d%H%M%S>"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_TAGS: TALK\n"
                        "#+ROAM_ALIAS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n"
                        "#+ROAM_KEY: %x\n")
         :unnarrowed t)
        ("w" "webpage" plain (function org-roam--capture-get-point)
         "\n%?"
         :file-name "webpages/${slug}-%<%Y%m%d%H%M%S>"
         :head ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: %n (%(user-login-name))\n"
                        "#+ROAM_TAGS: WEBPAGE\n"
                        "#+ROAM_ALIAS:\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n"
                        "#+ROAM_KEY: %x\n")
         :unnarrowed t)
        ))

(setq org-roam-dailies-capture-templates
      `(("d" "default" entry
         #'org-roam-capture--get-point
         "* %U\n%?"
         :file-name "journal/%<%Y-%m-%d>"
         :head ,(concat "#+TITLE: %<[%Y-%m-%d] %B %e, %Y (%A)>\n"
                        "#+ROAM_TAGS: JOURNAL\n"
                        "#+ROAM_ALIAS: %<%Y-%m-%d>\n"
                        "#+CREATED: %U\n"
                        "#+LAST_MODIFIED: %U\n"
                        "\n"))
        ))

;; (zamlz/leader-keys
;;   "n"  '(:ignore t :which-key "Org Roam Notes")
;;   "nj" '(org-roam-dailies-capture-today :which-key "Roam Daily Capture Today")
;;   "ny" '(org-roam-dailies-capture-yesterday :which-key "Roam Daily Capture Yesterday"))

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

(use-package pdf-tools
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-midnight-colors '("#ebdbb2" . "#000000"))
  :init (pdf-tools-install))

(use-package xkcd)

(use-package wttrin
  :custom
  (wttrin-default-cities '("Union City, CA")))

(use-package key-quiz)

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
