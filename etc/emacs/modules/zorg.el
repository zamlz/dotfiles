;; Yeah I don't have a good name for this module, but this should
;; house everything related to org-mode. (zorg -> zamlz's org-mode)


;; Create org directory if it doesn't exist
(unless (file-directory-p +org-directory)
  (make-directory +org-directory))

;; setup org
(use-package org
  :init
  (setq org-directory +org-directory))

;; Let's configure org-roam as well
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory +org-directory)
  :config
  (org-roam-db-autosync-mode +1))

(provide 'zorg)
