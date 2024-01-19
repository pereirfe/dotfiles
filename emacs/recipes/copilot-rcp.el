(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "M-\\") #'copilot-complete)
  (define-key copilot-completion-map (kbd "TAB") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
)

(provide 'copilot-rcp)
