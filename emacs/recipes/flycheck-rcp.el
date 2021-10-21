;;; flycheck-rcp.el --- This configure the flycheck

;;; Code:
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)

  :config
  (setq-default flycheck-global-modes '(not org-mode))
  (setq-default flycheck-disabled-checkers             ;; disable jshint since we prefer eslint checking (and json checking)
                (append flycheck-disabled-checkers '(json-jsonlist javascript-standard javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)     ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq-default flycheck-temp-prefix ".flycheck")      ;; customize flycheck temp file prefix
  )

(use-package flycheck-jest
  :after flycheck
  :config
  (flycheck-jest-setup)
  )


(provide 'flycheck-rcp)
;;; Commentary:
;;
;;; flycheck-rcp.el ends here
