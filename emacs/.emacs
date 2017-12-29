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
    anaconda-mode
    ggtags
    smartparens
    avy
    expand-region
    w3m
    ag
    magit
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



(require 'server)
(unless (server-running-p)
  (server-start))

(setq org-agenda-custom-commands
      '(("L" "@LRC"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags "+@LRC+TODO=\"NEXT\"")
	  (tags "+Battlestation+TODO=\"NEXT\"")
	  (tags "+MOBILE+TODO=\"NEXT\"")
	 ))
	("H" "@HOME"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags "+@HOME+TODO=\"NEXT\"")
	  (tags "+Battlestation+TODO=\"NEXT\"")
	  (tags "+MOBILE+TODO=\"NEXT\"")
	  ))
	("N" "Nelson"
	 ((tags "+Nelson+TODO=\"NEXT\"|+Nelson+TODO=\"WAITING\"")
	  ))
	("E" "MOBILE+ERRANDS"
	 ((tags "+MOBILE+TODO=\"NEXT\"")
	  (tags "+ERRANDS+TODO=\"NEXT\"")
	  ))
	("W" "Waiting"
	 ((todo "WAITING")
	  ))

	("w" "Week-long"
	 ((agenda "" ((org-agenda-span 15)))
	  ))

	("P" "Printed agenda"
         ((tags "+ERRANDS+TODO=\"NEXT\"|+MOBILE+TODO=\"NEXT\""

                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nErrands and Mobile\n------------------\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
           (ps-landscape-mode t))
         ("~/gtd/Offline/ERRANDS.txt"))
	)
      )

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 0)

;; Store analogic agendas when closing emacs
(add-hook 'kill-emacs-hook 'org-store-agenda-views)


(require 'json)


(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)

;;(define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
;;(define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-c f") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c b") 'sp-forward-barf-sexp)

; I prefer return to activate a link
(setq org-return-follows-link t)

;; Disable C-z suspend
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Magit Configuration
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c a") 'org-agenda)

;; Disable C-t toggle character
(global-unset-key [(control t)])

;; Move backup files from working directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Enable linenum-mode globally
;;(global-linum-mode t)

;; Enable Python development mode (ELPY)
;;(elpy-enable)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Enable Syntax correction on the fly for python
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable IDO-mode
(ido-mode t)
;;; Stops IDO from searching for similar-named files if I use Cx Cs to create
;;; new file or buffer
(setq ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t)

;; Powerline config
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;; Enable Autocomplete
(ac-config-default)

;;(elpy-use-ipython)
(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")


;; Enable Iedit mode
(define-key global-map (kbd "C-ç") 'iedit-mode)

;; Enable SMEX and bind it to M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Auto-revert Mode Global
(global-auto-revert-mode 1)

;; GTD implementation
(setq org-tag-alist '(("@LRC" . ?l)
		      ("@HOME" . ?h)
		      ("MOBILE" . ?m)
		      ("Nelson" . ?n)
		      ("Carlos" . ?c)
		      ("Battlestation" . ?b)
		      ("TEL" . ?t)
		      ("ERRANDS" . ?e)))

(setq org-todo-keywords
      '((sequence "SCHED(s)" "TODO(t)" "NEXT(n)" "WAITING(w)" "TICKLED(T)" "POSTPONED(p)" "|" "DONE(d)" "DELEGATED(o)" "Cancelled(c)")))

(setq org-default-notes-file "~/gtd/in.org")
(global-set-key (kbd "C-c c") 'org-capture)


;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;(setq elpy-rpc-backend "jedi")

;; Avy - Jump like wind
;;(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-:") 'avy-goto-word-1)

;; Enable Material Theme
(load-theme 'material t)

;; Setting a vim like %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	            (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)


(defun open-pp-form()
  (interactive)
  (w3m-goto-url "https://docs.google.com/forms/d/e/1FAIpQLSdjtiVqL7d14_nXDF91x88MJm3eM9bhj07yg-_xyaP65Xb-tg/viewform"))
(global-set-key (kbd "C-c i") 'open-pp-form)


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c s") 'imenu)


(setq w3m-form-use-textarea-backup nil)
;; Start Server for emacs
;;(server-start)

;; Delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; CEDET
;; Turn on semantics
;(semantic-mode 1)
;; define function to add semantics to autocomplete @c-mode-common-hook
;(defun my:add-semantic-to-autocomplete()
;  (add-to-list 'ac-sources 'ac-source-semantic)
;  )
;(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; Turn automatic reparsing when idle
;(global-semantic-idle-scheduler-mode 1)

;; Trying to add the Project
;;(global-ede-mode t)
;;(ede-cpp-root-project "LTE-Sim" :file "~/lte-sim-fp/src/LTE-Sim.cpp")

;; Set default browser
;;(setq browse-url-browser-function 'browse-url-chrome)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Babel Load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   ))

;; Activate reformat-xml
(require 'sgml-mode)

(defun reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
        (indent-region (point-min) (point-max))))

;; Ag
;; Require AG installation!


(setq org-capture-templates
      '(("c" "Todo" entry (file+headline  org-default-notes-file "Tasks")
	 "* TODO %?\n  %U\n %i\n  %a")))

;; Set ls -alh as default for dired
(setq dired-listing-switches "-alh")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(column-number-mode nil)
 '(custom-safe-themes
   (quote
    ("43813ed7f4ada2420b4c68d26d88b75ef92f640bc93438812059f0275c34254b" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/gtd/tickler.org" "~/gtd/events.org" "~/gtd/birthdays.org" "~/gtd/projects.org" "~/gtd/tasks.org")))
 '(org-capture-templates
   (quote
    (("c" "Todo" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %?
  %U
 %i
  %a"))) t)
 '(package-selected-packages
   (quote
    (anaconda-mode zenburn-theme w3m visible-mark smex smartparens python-environment py-autopep8 powerline org noctilux-theme material-theme magit impatient-mode iedit ggtags flycheck find-file-in-repository expand-region elpy ctags-update ctable avy auto-complete ag)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
	   (quote after-save-hook)
	   (quote org-html-export-to-html)
	   t t))))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
