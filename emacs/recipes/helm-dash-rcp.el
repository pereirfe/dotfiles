;;; helm-dash-rcp.el --- This configures the helm-dash

;;; Code:
(use-package helm-dash
  :config
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-common-docsets '("NodeJS"))
  )

(provide 'helm-dash-rcp)
;;; Commentary:
;;
;;; dired-rcp.el ends here
