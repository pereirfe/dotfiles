;;; magit-todos-rcp.el --- This configures the magit-todos package

;;; Code:
(use-package magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (custom-set-variables
   '(magit-todos-keywords (list "FPTODO" "FP-TODO" "FPFIX" "FPDEL"))
   )
  )

(provide 'magit-todos-rcp)
;;; Commentary:
;;

;;; magit-todos-rcp.el ends here
