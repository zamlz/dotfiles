
;; Let's use something lightweight like vertico
(use-package vertico)
(use-package orderless)
(use-package consult)
(use-package embark)
(use-package embark-consult)
(use-package marginalia)
(use-package all-the-icons-completion)

(vertico-mode)
(marginalia-mode)

;; Configure orderless completion
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))

;; Cycle between marignalia annotations in vertico
(define-key vertico-map (kbd "M-A") #'marginalia-cycle)

;; all-the-icons-completion and marginalia need to be combined
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(provide 'completion)
