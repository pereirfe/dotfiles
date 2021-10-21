;;; dired-rcp.el --- This configures the org-journal system

;;; Code:
(use-package dired
  :config
  (setq dired-listing-switches "-alh")
  :bind ("C-x C-d" . dired)
  )

(provide 'dired-rcp)
;;; Commentary:
;;
;;; dired-rcp.el ends here
