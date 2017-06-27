;; Install Packages
;; --------------------
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(material-theme
    flycheck
    ido
    smex
    powerline
    iedit
    auto-complete
    impatient-mode
    visible-mark
    elpy))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)


;; Basic Customization
;; --------------------

;; Disable C-z suspend
;;(global-unset-key [(control z)])
;;(global-unset-key [(control x)(control z)])

;; Disable C-t toggle character
(global-unset-key [(control t)])

;; Move backup files from working directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Enable Material Theme
(load-theme 'material t)

;; Enable linenum-mode globally
;;(global-linum-mode t)

;; Enable Python development mode (ELPY)
(elpy-enable)

;; Enable Syntax correction on the fly for python
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable IDO-mode
(ido-mode t)
;;; Stops IDO from searching for similar-named files if I use Cx Cs to create
;;; new file or buffer
(setq ido-auto-merge-work-directories-length -1)

;; Powerline config
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;; Enable Autocomplete
(ac-config-default)

;; Enable Iedit mode
(define-key global-map (kbd "C-`") 'iedit-mode)

;; Enable SMEX and bind it to M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Auto-revert Mode Global
(global-auto-revert-mode 1)

;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;(setq elpy-rpc-backend "jedi")

;; Setting a vim like %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	            (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-S-5") 'goto-match-paren)

;; Start Server for emacs
(server-start)

;; Delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn on semantics
(semantic-mode 1)
;; define function to add semantics to autocomplete @c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; Turn automatic reparsing when idle
(global-semantic-idle-scheduler-mode 1)


;; Babel Load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; Turn semantic mode on
(semantic-mode 1)
;; Define a function to add semantic suggestion for c-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-sources-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(global-semantic-idle-scheduler-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode nil)
 '(custom-safe-themes (quote ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Dropbox/working_NOW/Pers/threads/persthread.org" "~/Dropbox/working_NOW/Master/threads/threads.org")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
