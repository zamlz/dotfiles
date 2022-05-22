;; After all the packages are installed and configured at a basic level,
;; all high level configurations should be defined here.
;; Specifically, any custom keybindings and personal settings.

;; Basic user settings
(setq user-full-name "Amlesh Sivanantham")
(setq user-mail-address "zamlz@pm.me")
(setq user-login-name "zamlz")

;; --------------------------------------------------------------
;; Custom integrations
;; --------------------------------------------------------------

;; unfortunately, magit's editor doesn't start in "insert" mode which
;; is really inconvenient. Add this hook to enable that.
(add-hook 'with-editor-mode-hook #'evil-insert-state)

(provide 'config)
