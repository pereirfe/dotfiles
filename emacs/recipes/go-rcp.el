(use-package go-mode
  :ensure t
  :config

  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook (lambda()
                            (setq tab-width 4)
                            ))
  (add-hook 'go-mode-hook
            (lambda ()
              (company-mode) ; enable company upon activating go

              ;; Code layout.
                                        ;(add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save

              ;; Shortcuts for common go-test invocations.
              (let ((map go-mode-map))
                (define-key map (kbd "C-c g t p") 'go-test-current-project) ;; current package, really
                (define-key map (kbd "C-c g t f") 'go-test-current-file)
                (define-key map (kbd "C-c g t t") 'go-test-current-test)
                )

              (define-key lsp-mode-map (kbd "C-c g i") 'lsp-goto-implementation)

              ;; Fix parsing of error and warning lines in compiler output.
                                        ;(setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
              ;; (remove 'go-panic
              ;;         (remove 'go-test compilation-error-regexp-alist-alist)))
              ;; Make another one that works better and strips more space at the beginning.
              ;; (add-to-list 'compilation-error-regexp-alist-alist
              ;;              '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):.*$" 1 2)))
              ;; (add-to-list 'compilation-error-regexp-alist-alist
              ;;              '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\)[[:space:]].*$" 1 2)))
              ;; override.
              ;; (add-to-list 'compilation-error-regexp-alist 'go-test t)
              ;; (add-to-list 'compilation-error-regexp-alist 'go-panic t)

              )
            (setq lsp-go-env '((GOFLAGS . "-tags=unit")))
            )
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )


(provide 'go-rcp)
