
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

;; This is a package that is used by use-package to diminish major modes
(straight-use-package 'diminish)

;; Update the load path to load other configuration files.

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("modules"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

;; First load up our configuration
(require 'config)

;; Load up our modules
(require 'ui)
(require 'editor)
(require 'completion)
(require 'lang)
(require 'checkers)
(require 'zorg)
(require 'apps)

;; Finally load any cross module integrations that were not able to be
;; put in the module files themselves (ex: magit and evil integration)
(require 'integrations)
