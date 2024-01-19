;;; flycheck-rcp.el --- This configure the flycheck

;;; Code:
(use-package dap-mode
  :ensure t
  :init
  (dap-mode 1)
  :config
  (require 'dap-python)

  ;; (defun dap-python--pyenv-executable-find (command)
  ;;   (concat (getenv "VIRTUAL_ENV") "/bin/python"))

  (define-key dap-mode-map (kbd "C-c d d") #'dap-hydra)
)

(provide 'dap-mode-rcp)
;;; Commentary:
;;
;;; dap-mode-rcp.el ends here
