(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  ;; (setq pyvenv-post-activate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  ;; (setq pyvenv-post-deactivate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter "python3"))))
  )

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (setenv "WORKON_HOME" "~/.local/share/virtualenvs/")
  (define-key local-unset-key (kbd "C-c C-l") nil)
  (require 'dap-python))

(provide 'python-rcp)
