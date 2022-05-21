;; Yeah I don't have a good name for this module, but this should
;; house everything related to org-mode. (zorg -> zamlz's org-mode)

(use-package org)
(use-package org-roam :after org)

;; Shared variables for org modules
(setq +org-directory (file-truename "~/org"))

;; setup org
(setq org-directory +org-directory)

;; We need to acknowledge org-roam version 2 and enable it
(setq org-roam-v2-ack t)
(setq org-roam-directory +org-directory)
(org-roam-db-autosync-mode)

(provide 'zorg)
