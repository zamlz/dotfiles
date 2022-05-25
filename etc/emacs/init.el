
;; Configure Package Manager :: straight.el

;; Unfortunately, this package is not built into the package manager so must
;; boostrap =straight.el= package manager manually

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; =use-package= is a very nice wrapper over =package.el=, unfortunately, it
;; does not support =straight.el= out of the box. The following will install
;; and enable that functionality.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-compute-statistics t)

;; This is a package that is used by use-package to diminish major modes
(use-package diminish)

;; We need to also install general so it's used by use-package
(use-package general
  :config
  (general-evil-setup))

;; Update the load path to load other configuration files.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("modules"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

;; First load up our configuration

;; Basic user settings
(setq user-full-name "Amlesh Sivanantham")
(setq user-mail-address "zamlz@pm.me")
(setq user-login-name "zamlz")

;; Shared variables for org modules
(setq +org-directory (file-truename "~/org"))


;; Load up our modules

;; regardless of modeline we use, we want to see line and column numbers in it
(column-number-mode +1)

;; We shoudl add a little bit of a fringe so things can be drawn there if needed
(set-fringe-mode 8)

;; Let's also make sure line numbers appear in programming modes 
(dolist (mode '(prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Let's also make the UI transparent
(defun +set-transparency (value)
  "Sets the transparency of the frame window. 0=max-transparency/100-opaque"
  (interactive "nTransparency Value 0 - 100: ")
  (set-frame-parameter (selected-frame) 'alpha value))
(+set-transparency 100) ;; set the default transparency

;; Configure our emacs font (fira code)
(setq +default-fixed-font "Fira Code")
(setq +default-fixed-font-size 110)
(set-face-attribute 'default nil :family +default-fixed-font :height +default-fixed-font-size)
(set-face-attribute 'fixed-pitch nil :family +default-fixed-font :height +default-fixed-font-size)
(set-face-attribute 'variable-pitch nil :family +default-fixed-font :height +default-fixed-font-size)

;; enable fira code ligitures
;; TODO: Figure out how to run the install command intelligently (fira-code-mode-install-fonts)
(use-package fira-code-mode
  :init
  (setq fira-code-mode-disabled-ligatures '("[]" "x"))
  :hook
  (prog-mode text-mode))

;; Let's replace the prexisting dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/etc/emacs/navi.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '())
  :config
  (dashboard-modify-heading-icons '((bookmarks . "book")))
  (dashboard-setup-startup-hook))

;; Let's configure Protesilaos Stavrou's Modus theme
(use-package modus-themes
  :init
  (setq modus-themes-inhibit-reload t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-syntax nil)
  (setq modus-themes-mixed-fonts nil)
  (setq modus-themes-links '(no-underline background))
  (setq modus-themes-prompts '(background italic))
  (setq modus-themes-mode-line '(accented))
  (setq modus-themes-tabs-accented nil)
  (setq modus-themes-completions nil)
  (setq modus-themes-mail-citations nil)
  (setq modus-themes-fringes nil)
  (setq modus-themes-lang-checkers nil)
  (setq modus-themes-hl-line nil)
  (setq modus-themes-subtle-line-numbers t)
  (setq modus-themes-paren-match '(bold intense))
  (setq modus-themes-region '(bg-only accented))
  (setq modus-themes-diffs nil)
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-org-agenda nil)
  (setq modus-themes-headings nil)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind
  ("<f5>" . modus-themes-toggle))


;; You need to manually install all-the-icons-install-fonts
(use-package all-the-icons)

;; Dired is lacking some icons so let's get it some icons
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; With our themes enabled, let's configure the modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode +1))

;; Let's us make sure numbers are highlighted
(use-package highlight-numbers
  :hook
  ((prog-mode conf-mode) . highlight-numbers-mode))

;; And also make sure that escape sequences are also highlighted
(use-package highlight-escape-sequences
  :hook
  ((prog-mode conf-mode) . hes-mode))

;; Let's enable indentation hints
(use-package highlight-indent-guides
  :hook
  ((prog-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-suppress-auto-error t)
  :config
  (highlight-indent-guides-auto-set-faces))

;; which-key is a nice tool to see available keybindings on the fly
;; in case we forget about it
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 1.0)
  (setq which-key-secondary-delay 0.0)
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode +1))

;; A nice cosmetic for parens that make all them colored differently.
;; VERY useful for lisp
(use-package rainbow-delimiters
  :hook
  ((prog-mode conf-mode) . rainbow-delimiters-mode))

;; Let's add some visual git integration to the editor
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (global-diff-hl-show-hunk-mouse-mode)
  (diff-hl-flydiff-mode))


;; Here are some saner editor defaults
(setq show-paren-delay 0)
(show-paren-mode +1)

;; Don't create backup files and lockdirs
(setq create-lockfiles nil)
(setq buckup-directory-alist `(("." . "~/.config/emacs/backup")))

;; Let's also add some saner dired defaults for ordering
(setq dired-listing-switches "-lahF --group-directories-first")

;; Reuse dired buffers instead of creating news whenever we traverse directories
;; FIXME: Doesn't appear to be working at all
(use-package dired-single)

;; Dired should not be showing hidden files by default lol
(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

;; Let's improve the undo system
(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.cache/undo-tree/")))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-relative-timestamps nil)
  (setq undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode +1))

;; Let's make GNU/Emacs more EVIL!!
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode +1))

;; Let's improve the keybindings of evil
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "J") 'next-buffer)
(define-key evil-normal-state-map (kbd "K") 'previous-buffer)

;; Now that evil and undo-tree are  both loaded, let's link them together
(evil-set-undo-system 'undo-tree)

;; This adds a bunch of extra useful evil functionality to other emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Now that evil-collection and dired is setup, we need to integrate the two
(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory ;; dired-single version of 'dired-up-directory
  "l" 'dired-single-buffer ;; dired-single version of 'dired-find-file
  "H" 'dired-hide-dotfiles-mode)

;; Really nice vi commenting keybindings
(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode +1))

;; while this is a UI change, this makes emacs "visually" more evil
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode +1)
  (evil-goggles-use-diff-faces))


;; A very lightweight minibuffer completion system
(use-package vertico
  :config
  (vertico-mode +1))

;; helps make vertico look nice by annotating completions
(use-package marginalia
  :config
  (marginalia-mode +1))

;; Cycle between marignalia annotations in vertico
(define-key vertico-map (kbd "M-m") #'marginalia-cycle)

;; Configure orderless completion
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))


;; Let's us add icons to the completion annotations
(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode +1))

;; We need to add the icons setup to marginalia's annotations with this hook
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package consult)
(use-package embark)
(use-package embark-consult)


;; gitattributes, gitconfig, gitignore, etc.
(use-package git-modes)

;; Create org directory if it doesn't exist
(unless (file-directory-p +org-directory)
  (make-directory +org-directory))

;; setup org
(use-package org
  :init
  (setq org-directory +org-directory))

;; Let's configure org-roam as well
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory +org-directory)
  :config
  (org-roam-db-autosync-mode +1))

(use-package magit)

;; Improve the magit experience with the following plugin
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode +1))

;; It's also useful to have projectile, a useful project management tool
(use-package projectile
  :diminish
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '())
  (dolist (project-dir '("~/src" "~/usr"))
    (when (file-directory-p project-dir)
      (add-to-list 'projectile-project-search-path project-dir)))
  :config
  (projectile-mode +1))

;; Helpful is a replacement for the emacs help pages with far more info
;; and context
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)
   ("C-h C-d" . helpful-at-point)))

;; Finally load any cross module integrations that were not able to be
;; put in the module files themselves (ex: magit and evil integration)

;; unfortunately, magit's editor doesn't start in "insert" mode which
;; is really inconvenient. Add this hook to enable that.
(add-hook 'with-editor-mode-hook #'evil-insert-state)
