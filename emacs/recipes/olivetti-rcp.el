;;; olivetti-rcp.el --- This configures the olivetti mode

;;; Code:
(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80)
  )

(provide 'olivetti-rcp)
;;; Commentary:
;;
;;; olivetti-rcp.el ends here
