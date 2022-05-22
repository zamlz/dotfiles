
;; --------------------------------------------------------------
;; Custom integrations
;; --------------------------------------------------------------

;; unfortunately, magit's editor doesn't start in "insert" mode which
;; is really inconvenient. Add this hook to enable that.
(add-hook 'with-editor-mode-hook #'evil-insert-state)

(provide 'integrations)
