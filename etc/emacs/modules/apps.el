;; Here we configure standalone apps for emacs.
;; some of these are built-in but it's not necessarily the case for all.

;; The single most useful emacs app is =magit=
;; It provides a very nice interface to work with git repos.
(use-package magit)

;; Improve the magit experience with the following plugin
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(provide 'apps)
