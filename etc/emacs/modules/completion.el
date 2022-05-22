
;; A very lightweight minibuffer completion system
(use-package vertico
  :config
  (vertico-mode +1))

;; helps make vertico look nice by annotating completions
(use-package marginalia
  :config
  (marginalia-mode +1))

;; Cycle between marignalia annotations in vertico
(define-key vertico-map (kbd "M-m") #'marginalia-cycle)

;; Configure orderless completion
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))


;; Let's us add icons to the completion annotations
(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode +1))

;; We need to add the icons setup to marginalia's annotations with this hook
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package consult)
(use-package embark)
(use-package embark-consult)

(provide 'completion)
