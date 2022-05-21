
;; The UI in Emacs needs a lot of changes to make it more modern

(use-package fira-code-mode)
(use-package dashboard)
(use-package modus-themes)
(use-package doom-modeline)
(use-package all-the-icons)
(use-package highlight-numbers)
(use-package highlight-escape-sequences)

;; Alright we have to start with sane defaults here, first let's disable the
;; ugly GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)

;; Configure our emacs font (fira code)
(setq +default-fixed-font "Fira Code")
(setq +default-fixed-font-size 110)
(set-face-attribute 'default nil :family +default-fixed-font :height +default-fixed-font-size)
(set-face-attribute 'fixed-pitch nil :family +default-fixed-font :height +default-fixed-font-size)
(set-face-attribute 'variable-pitch nil :family +default-fixed-font :height +default-fixed-font-size)

;; enable ligitures : (fira-code-mode-install-fonts)
(setq fira-code-mode-disabled-ligatures '("[]" "x"))
(add-hook 'prog-mode-hook 'fira-code-mode)

;; Let's also make sure line numbers appear (almost) everywhere
(global-display-line-numbers-mode 1)
(dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Let's replace the prexisting dashboard
(setq dashboard-startup-banner "~/etc/emacs/navi.png")
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
(setq dashboard-items '())
(dashboard-modify-heading-icons '((bookmarks . "book")))
(dashboard-setup-startup-hook)

;; Let's configure Protesilaos Stavrou's Modus theme
(modus-themes-load-themes)
(modus-themes-load-vivendi)

;; With our themes enabled, let's configure the modeline
(doom-modeline-mode 1)
(column-number-mode 1)

;; Some other minor imprvements
(add-hook 'prog-mode-hook #'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'hes-mode) ;; highlight-escape-sequences-mode

;; Let's also make the UI transparent
(defun +set-transparency (value)
  "Sets the transparency of the frame window. 0=max-transparency/100-opaque"
  (interactive "nTransparency Value 0 - 100: ")
  (set-frame-parameter (selected-frame) 'alpha value))
;; set the default transparency here
(+set-transparency 100)

(provide 'ui)
