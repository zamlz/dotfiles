
;; -------------------------------------------------------------
;; The UI in Emacs needs a lot of changes to make it more modern
;; -------------------------------------------------------------

;; Alright we have to start with sane defaults here, first let's disable the
;; ugly GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)

;; regardless of modeline we use, we want to see line and column numbers in it
(column-number-mode 1)

;; We shoudl add a little bit of a fringe so things can be drawn there if needed
(set-fringe-mode 8)

;; Let's also make sure line numbers appear in programming modes 
(dolist (mode '(prog-mode-hook))
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
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
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
  (doom-modeline-mode 1))

;; Let's us make sure numbers are highlighted
(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

;; And also make sure that escape sequences are also highlighted
(use-package highlight-escape-sequences
  :hook
  (prog-mode . hes-mode))

;; which-key is a nice tool to see available keybindings on the fly
;; in case we forget about it
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 1.0)
  (setq which-key-secondary-delay 0.0)
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode 1))

;; A nice cosmetic for parens that make all them colored differently.
;; VERY useful for lisp
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'ui)
