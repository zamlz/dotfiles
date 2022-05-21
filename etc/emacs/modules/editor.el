
;; Evil collection expects evil-want-keybinding and evil-want-integration to
;; be configured as (nil, t) respectively before both evil and evil-collection
;; is initialized.
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t))
(use-package evil-collection :after evil)
(use-package evil-commentary :after evil)
(use-package evil-goggles :after evil)

;; Here are some saner editor defaults
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Don't create backup files and lockdirs
(setq create-lockfiles nil)
(setq buckup-directory-alist `(("." . "~/.config/emacs/backup")))

;; Let's make GNU/Emacs more EVIL!!
(evil-mode 1)
(evil-commentary-mode 1)
(evil-collection-init)

;; while this is a UI change, this makes emacs "visually" more evil
(evil-goggles-mode 1)
(evil-goggles-use-diff-faces)

;; Evil needs to follow some more saner keybindings
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "J") nil)
(define-key evil-normal-state-map (kbd "K") nil)

(provide 'editor)
