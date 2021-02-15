;;; init.el --- Emacs init file
;;  Author: Amlesh Sivanantham
;;; Commentary:
;;;
;;; INIT.EL file taken from Ian Y.E. Pan as I didn't want to have a copy of
;;; config that was tangled checked into git
;;;
;;; Code:
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

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold (* zamlz/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold zamlz/gc-cons-threshold)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;; Load main config file "./emacs.org"
(require 'org)
(org-babel-load-file (expand-file-name "~/etc/emacs.org"))

(provide 'init)
;;; init.el ends here
