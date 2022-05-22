;; Yeah I don't have a good name for this module, but this should
;; house everything related to org-mode. (zorg -> zamlz's org-mode)

;; Shared variables for org modules
(setq +org-directory (file-truename "~/org"))

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
  (org-roam-db-autosync-mode))

(provide 'zorg)
