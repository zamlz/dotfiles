;; Here we configure standalone apps for emacs.
;; some of these are built-in but it's not necessarily the case for all.

;; The single most useful emacs app is =magit=
;; It provides a very nice interface to work with git repos.
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

(provide 'apps)
