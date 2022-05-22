
;; Evil collection expects evil-want-keybinding and evil-want-integration to
;; be configured as (nil, t) respectively before both evil and evil-collection
;; is initialized.

;; Here are some saner editor defaults
(setq show-paren-delay 0)
(show-paren-mode 1)

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
  (global-undo-tree-mode 1))

;; Let's make GNU/Emacs more EVIL!!
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "J") nil)
  (define-key evil-normal-state-map (kbd "K") nil))

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
  (evil-commentary-mode 1))

;; while this is a UI change, this makes emacs "visually" more evil
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode 1)
  (evil-goggles-use-diff-faces))

(provide 'editor)
