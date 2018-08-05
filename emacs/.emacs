;; Install Packages
;; --------------------
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(material-theme
    color-theme-sanityinc-tomorrow
	org-gcal
	anaconda-mode
    ggtags
	ace-window
    smartparens
    avy
    expand-region
    w3m
    helm
    helm-projectile
    projectile
    ag
    helm-ag
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

(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq org-M-RET-may-split-line nil)
(setq eval-expression-print-length nil)

(setq custom-safe-themes t)
(require 'iso-transl)
(require 'server)

(unless (server-running-p)
  (server-start))


(global-set-key (kbd "C-c C-b") 'mode-line-other-buffer)

(setq org-agenda-span 'day)

(setq org-fast-tag-selection-single-key t)
(setq org-agenda-custom-commands

      ;; S -> Samsung/LEDL
      ;; M -> Meeting

	  '(("L" "@LRC"
		 ((agenda "" ())
		  (tags "+@LRC+TODO=\"NEXT\"")
		  (tags "+Battlestation+TODO=\"NEXT\"")
		  (tags "+MOBILE+TODO=\"NEXT\"")
		  ))
		("NL" "@LRC + Notebook"
		 ((agenda "" ())
		  (tags "+@LRC+TODO=\"NEXT\"")
		  (tags "+NB+TODO=\"NEXT\"")
		  (tags "+Battlestation+TODO=\"NEXT\"")
		  (tags "+MOBILE+TODO=\"NEXT\"")
		  ))
		("DL" "Deep Work LRC"
		 (
		  (tags "DW+@LRC+TODO=\"NEXT\"")
		  (tags "DW+Battlestation+TODO=\"NEXT\"")
		  (tags "DW+MOBILE+TODO=\"NEXT\"")
		  ))
		("H" "@HOME"
		 (
		  (tags "+@HOME+TODO=\"NEXT\"")
		  (tags "+NB+TODO=\"NEXT\"")
		  (tags "+Battlestation+TODO=\"NEXT\"")
		  (tags "+MOBILE+TODO=\"NEXT\"")
		  ))
		("DH" "Deep Work HOME"
		 ((agenda "" ())
		  (tags "DW+@HOME+TODO=\"NEXT\"")
		  (tags "DW+NB+TODO=\"NEXT\"")
		  (tags "DW+Battlestation+TODO=\"NEXT\"")
		  (tags "DW+MOBILE+TODO=\"NEXT\"")
		  ))
		("S" "Working Samsung/SAM_LEDL"
		 ((agenda "" ())
		  (tags "+@HOME+SAM_LEDL+TODO=\"NEXT\"")
		  (tags "+@LRC+SAM_LEDL+TODO=\"NEXT\"")
		  (tags "+NB+SAM_LEDL+TODO=\"NEXT\"")
		  (tags "+Battlestation+SAM_LEDL+TODO=\"NEXT\"")
		  (tags "+MOBILE+SAM_LEDL+TODO=\"NEXT\"")
		  (tags "+SAM_LEDL+TODO=\"NEXT\"")
		  ))

		("DS" "DONE Samsung/SAM_LEDL"
		 ((tags "+SAM_LEDL+TODO=\"DONE\""
				((org-agenda-sorting-strategy '(tsia-down))
				 )))
         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t)))

		("MN" "Nelson"
		 ((tags "+Nelson+TODO=\"NEXT\"|+Nelson+TODO=\"WAITING\""
				((org-agenda-prefix-format "[ ] %-20b:")
				 (org-agenda-sorting-strategy '(tag-up priority-down))
				 (org-agenda-overriding-header "\nReunião Nelson\n------------------\n"))))
         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t)))
		("MS" "Meeting SAM_LEDL"
		 ((tags "+Meet_SAM_LEDL+TODO=\"NEXT\"|+Meet_SAM_LEDL+TODO=\"WAITING\""
				((org-agenda-prefix-format "[ ] %-20b:")
				 (org-agenda-sorting-strategy '(tag-up priority-down))
				 (org-agenda-overriding-header "\nReunião SAM_LEDL\n------------------\n"))))
         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t)))
		("MC" "Carlos"
		 ((tags "+Carlos+TODO=\"NEXT\"|+Carlos+TODO=\"WAITING\"")
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
		 ((tags "+ERRANDS+PLACE=\"\"+TODO=\"NEXT\""
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nErrands (General)\n------------------\n")))
		  (tags "+ERRANDS+PLACE={ATK}+TODO=\"NEXT\""
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nAtacadao\n------------------\n")))
		  (tags "+ERRANDS+PLACE={MKT}+TODO=\"NEXT\""
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nMarket\n------------------\n")))
		  (tags "+ERRANDS+PLACE={HORT}+TODO=\"NEXT\""
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nHortifruti\n------------------\n")))
		  (tags "+MOBILE+TODO=\"NEXT\""
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nMobile\n------------------\n"))))
		 ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t))
         ("~/gtd/Offline/ERRANDS.txt"))
		)
      )

;; Effort and global properties
(setq org-global-properties '(("Effort_ALL". "0 0:05 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00")))

;; Set global Column View format
(setq org-columns-default-format '"%38ITEM(Details) %1PRIORITY(P)  %7TODO(To Do) %5Effort(Effort){:} %6CLOCKSUM(Clock) %TAGS(Context)")

(setq org-use-property-inheritance t)

;; https://orgmode.org/worg/doc.html
(setq org-agenda-sorting-strategy '((agenda) priority-down effort-up))


(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 1)

;; Store analogic agendas when closing emacs
(add-hook 'kill-emacs-hook 'org-store-agenda-views)

(require 'json)

(set-default 'truncate-lines t)

;; set a default font
(when (member "IBM Plex Mono" (font-family-list))
  (set-face-attribute 'default nil :font "IBM Plex Mono"))

;; (setq org-file-apps
;;       '(("\\.pdf\\'" . "evince %s")
;; 	("\\.png\\'" . "eog %s")
;; 	("\\.org\\'" . default)
;; 	("\\.jpeg\\'". "eog %s")))

;; (setq org-file-apps
;;       (append '(
;; 		("\\.pdf\\'" . "evince %s")
;; 		) org-file-apps ))


(require 'org-gcal)

(defconst my-secrets-file "~/gtd/Files_Projects/.emacs_conf.d/secrets.el")

(if (file-exists-p my-secrets-file)
	(load-file my-secrets-file)
  )

(if (boundp 'my-gcal-definition)
    (setq org-gcal-client-id my-gcal-client-id
	  org-gcal-client-secret my-gcal-client-secret
	  org-gcal-file-alist '(("fernandhenriqp@gmail.com" .  "~/Dropbox/gtd/calendar.org")
				)
	  )

  )

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))


(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'awk-mode-hook #'smartparens-mode)

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
;(setq python-shell-interpreter "ipython"
;       python-shell-interpreter-args "-i")


;; Enable Iedit mode
(define-key global-map (kbd "C-ç") 'iedit-mode)

;; Enable SMEX and bind it to M-x
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)

;; Auto-revert Mode Global
(global-auto-revert-mode 1)

;; GTD implementation
(setq org-tag-alist '(
					  ("@LRC" . ?l)
					  ("@HOME" . ?h)
					  ("DW" . ?d)
					  ("MOBILE" . ?m)
					  ("NB" . ?n)
					  ("Nelson" . ?N)
					  ("SAM_LEDL" . ?S)
					  ("Meet_SAM_LEDL" . ?M)
					  ("Carlos" . ?c)
					  ("Battlestation" . ?b)
					  ("TEL" . ?t)
					  ("ERRANDS" . ?e)))


;;https://orgmode.org/manual/Tracking-TODO-state-changes.html#Tracking-TODO-state-changes
(setq org-todo-keywords
      '((sequence "TICKLED(T!)" "SCHED(s!)" "TODO(t!)" "NEXT(n!)" "WAITING(w!)" "POSTPONED(p!)" "|" "DONE(d)" "DELEGATED(o)" "Cancelled(c!)")))

(setq org-log-done 'time)
(setq org-use-tag-inheritance t)

;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;(setq elpy-rpc-backend "jedi")

;; Avy - Jump like wind
;;(avy-setup-default)
(global-set-key (kbd "C-c o") 'avy-goto-word-1)

;; Ace window
;; https://github.com/abo-abo/ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Enable Material Theme
;;(load-theme 'material t)
(load-theme 'sanityinc-tomorrow-night)


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
;; (semantic-mode 1)
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


(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
;; Visualize Kill ring
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Resize to fit list
(helm-autoresize-mode t)
;; Show buffers and recent files
(global-set-key (kbd "C-x b") 'helm-mini)
;; Find Files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Imenu
;(setq helm-semantic-fuzzy-match t
;      helm-imenu-fuzzy-match    t)

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal1
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

;;(global-set-key (kbd "M-s o") 'helm-occur)
;;(global-set-key (kbd "C-x f") 'helm-find)


(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )


(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)



;; Activate reformat-xml
(require 'sgml-mode)

(defun reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
        (indent-region (point-min) (point-max))))

(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Ag
;; Require AG installation!


;; (setq org-capture-templates
;;       '(("c" "Todo" entry (file+headline  org-default-notes-file "Tasks")
;; 	 "** NEXT %?\n  %U\n %i\n  %a")))


;(setq org-default-notes-file "~/gtd/in.org")
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Dropbox/gtd/calendar.org" )
		 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

		("c" "IN" entry (file+headline "~/Dropbox/gtd/in.org" "IN")
		 "* NEXT %?\n%u" :prepend t)))




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
	("b550fc3d6f0407185ace746913449f6ed5ddc4a9f0cf3be218af4fb3127c7877" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "43813ed7f4ada2420b4c68d26d88b75ef92f640bc93438812059f0275c34254b" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
	("~/gtd/calendar.org" "~/gtd/projects.org" "~/gtd/Reference/reference.org" "~/gtd/tickler.org" "~/gtd/birthdays.org" "~/gtd/tasks.org")))
 '(org-stuck-projects
   (quote
	("+LEVEL=1/-DONE"
	 ("TODO" "NEXT" "NEXTACTION")
	 nil "")))
 '(package-selected-packages
   (quote
	(academic-phrases borg deferred org-gcal helm-ag helm anaconda-mode zenburn-theme w3m visible-mark smex smartparens python-environment py-autopep8 powerline org noctilux-theme material-theme magit impatient-mode iedit ggtags flycheck find-file-in-repository expand-region elpy ctags-update ctable avy auto-complete ag)))
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
