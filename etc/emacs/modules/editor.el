
(use-package evil
  :init (setq evil-want-keybinding nil))
(use-package evil-collection)
(use-package evil-commentary)
(use-package evil-goggles)

;; Here are some saner editor defaults
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Don't create backup files and lockdirs
(setq create-lockfiles nil)
(setq buckup-directory-alist `(("." . "~/.config/emacs/backup")))

;; Let's make GNU/Emacs more EVIL!!
(evil-mode 1)
(evil-goggles-mode 1)
(evil-goggles-use-diff-faces)

;; Evil needs to follow some more saner keybindings
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "J") nil)
(define-key evil-normal-state-map (kbd "K") nil)

(provide 'editor)
