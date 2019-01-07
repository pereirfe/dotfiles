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
	irony
	company-irony
    ggtags
    diminish
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
    exec-path-from-shell
    visible-mark
    yasnippet
    elpy))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;;;;;;;;;;;;;;; DIRED
;; Set ls -alh as default for dired
(setq dired-listing-switches "-alh")
(global-set-key (kbd "C-x C-d") 'dired)

;;;;;;;;;;;;;;;; LATEX

(setq-default TeX-master nil) ; Query for master file.
(add-hook 'LaTeX-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'LaTeX-mode-hook (lambda () (setq word-wrap t)))
(add-hook 'LaTeX-mode-hook #'smartparens-mode)
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'LaTeX-mode-hook #'company-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'LaTeX-mode-hook (lambda () (company-mode -1)))

(setq reftex-default-bibliography '("~/projects/project_hydra/src/references.bib"))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;;;;;;;;;;;;;;; MOVEMENT
(global-set-key (kbd "C-c C-b") 'mode-line-other-buffer)

;(global-set-key "\M-z" 'zap-up-to-char)

;; Default is Regex iSearching
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Disable C-t toggle character
(global-unset-key [(control t)])

(global-set-key (kbd "C-t o") (lambda () (interactive)
				(message "This is not Tmux :)")))
(global-set-key (kbd "C-t C-o") (lambda () (interactive)
				  (message "This is not Tmux :)")))

;; Avy - Jump like wind
;;(avy-setup-default)
(global-set-key (kbd "C-c o") 'avy-goto-word-1)

;; more familiar forward and backward word
;;(global-set-key (kbd "M-f") 'forward-same-syntax)
;; (global-set-key (kbd "M-b") (lambda () (interactive) ;
;;                               (forward-same-syntax -1)))

;; dwim C-a: move to indentation or beginning of line if already there
(defun beginning-of-indentation-or-line ()
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (if (= (point) (save-excursion (back-to-indentation) (point)))
        (beginning-of-line)
      (back-to-indentation))
    )
  )

(global-set-key (kbd "C-a") 'beginning-of-indentation-or-line)


;; saner forward and backward kill-word using thingatpt
(defun kill-syntax (&optional arg)
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))))
(defun backward-kill-syntax (&optional arg)
  (interactive)
  (kill-syntax -1))
(global-set-key (kbd "M-d") 'kill-syntax)
(global-set-key (kbd "C-<backspace>") 'backward-kill-syntax)

;; kill line, same as shell
(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line 0))
(global-set-key (kbd "M-k") 'backward-kill-line)

;; Ace window
;; https://github.com/abo-abo/ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;;;;;;;;;;;;;;; GENERAL CONFIG
; I prefer return to activate a link
(setq org-return-follows-link t)

(defun my-org-refile-goto ()
  (interactive)
  (org-refile '(4))
  )

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-g") 'my-org-refile-goto)))

(exec-path-from-shell-initialize)

(setq x-select-enable-clipboard t
	  x-select-enable-primary t)

(require 're-builder)
(setq reb-re-syntax 'string)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Tmux Copypast
(setq x-select-enable-clipboard t
	  x-select-enable-primary t)


;; Move backup files from working directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Auto-revert Mode Global
(global-auto-revert-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Disable C-z suspend
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;;;;;;;;;;;;;;;; APPEARANCE && STYLE
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq eval-expression-print-length nil)

(setq custom-safe-themes t)
(require 'iso-transl)

(load-theme 'sanityinc-tomorrow-night)

(when (member "IBM Plex Mono" (font-family-list))
  (set-face-attribute 'default nil :font "IBM Plex Mono"))

;; Powerline config
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;;;;;;;;;;;;;;;; CODING

;;;;;;;;;;;;;;;; Integration
;; Allow Integration with googlechrome
(require 'atomic-chrome)
(atomic-chrome-start-server)
(setq atomic-chrome-buffer-open-style 'frame)
(setq atomic-chrome-default-major-mode 'LaTeX-mode)
;; (setq atomic-chrome-url-major-mode-alist
;;       '(("overleaf\\.com" . 'LaTeX-mode)
;;         ("github\\.com" . 'python-mode)))


;;;;;;;;;;;;;;;; CODING / Code
(require 'json)


(define-key global-map (kbd "RET") 'newline-and-indent)

;; Use .agignore as ignore list for ag in this project
;(helm-ag--root-agignore)
(setq helm-ag-use-agignore t)
(setq helm-ag--ignore-case t)

(require 'yasnippet)
(yas-global-mode 1)
(global-set-key [backtab] 'yas-expand)

;;keys for navigation
(define-key yas-keymap [(tab)]       nil)
(define-key yas-keymap (kbd "TAB")   nil)
(define-key yas-keymap [(shift tab)] nil)
(define-key yas-keymap [backtab]     nil)
(define-key yas-keymap [backtab] 'yas-next-field-or-maybe-expand)
(define-key yas-keymap [(control backtab)] 'yas-prev)



(setq-default indent-tabs-mode nil)
(set-default 'truncate-lines t)
(add-hook 'c++-mode-hook '(lambda ()
                            (setq c-basic-offset 4)))
(add-hook 'c++-mode-hook '(lambda ()
                            (setq tab-width 4)))

;; Magit Configuration
(global-set-key (kbd "C-x g") 'magit-status)

(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'awk-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)

(define-key smartparens-mode-map (kbd "<f8>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "<f7>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

;; Enable Autocomplete
;(ac-config-default)

;; Enable Iedit mode
(define-key global-map (kbd "C-ç") 'iedit-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c s") 'imenu)

;; Delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Irony mode configuration
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony 'company-anaconda))


(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-c C-.") 'company-complete)


;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;; (PYTHON)
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")



(require 'sgml-mode)

(defun reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
        (indent-region (point-min) (point-max))))

(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)


;;;;;;;;;;;;;;; HELM && PROJECTILE

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
;; Resize to fit list
(helm-autoresize-mode t)
;; Show buffers and recent files
(global-set-key (kbd "C-x b") 'helm-mini)
;; Find Files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal1
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
;; (global-set-key (kbd "M-s o") 'helm-occur)

(global-set-key (kbd "C-x f") 'helm-find)


(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-remember-window-configs t )

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)


(diminish 'projectile-mode "P")
(diminish 'helm-mode)
(diminish 'Abbrev)

;;;;;;;;;;;;;;; ORG && AGENDA
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-M-RET-may-split-line nil)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-agenda-span 'day)
(add-hook 'org-mode-hook (lambda () (company-mode -1)))

(setq org-agenda-files
      (quote
       ("~/gtd/calendar.org"
        "~/gtd/projects.org"
        "~/gtd/Reference/reference.org"
        "~/gtd/tickler.org"
        "~/gtd/events.org"
        "~/gtd/birthdays.org"
        "~/gtd/tasks.org")))

;; (setq org-refile-targets '(
;;                            (nil :maxlevel . 1)
;;                            (org-agenda-files :maxlevel . 1)
;;                            ))

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)

(defun fp-org-open-4kft ()
  "Open the 4kft org file"
  (interactive)
  (org-open-file "~/gtd/4kft.org")
  )

;;(global-set-key (kbd "<f6>") 'org-latex-export-to-pdf)
(global-set-key (kbd "<f11>") (lambda ()
                                (interactive)
                                (find-name-dired
                                 "~/REFERENCE/INFO/.org_sources/" "*.org")))
(global-set-key (kbd "<f12>") 'fp-org-open-4kft)

(progn
  (require 'org)
  (define-key org-mode-map (kbd "<f5>") 'org-set-effort)
  (define-key org-mode-map (kbd "<f6>") 'org-latex-export-to-pdf)
  )


(global-unset-key  (kbd "<f2> <f2>"))



;; https://orgmode.org/worg/doc.html
(setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
									(todo priority-down effort-up)
									(tags priority-down effort-up)
									(search category-keep)))

(setq org-fast-tag-selection-single-key t)
(setq org-agenda-custom-commands

      ;; S -> Samsung/LEDL
      ;; M -> Meeting

	  '(("L" "@LRC"
		 ((agenda "" ())
          (tags "+PRIORITY=\"A\"-ERRANDS-@HOME/NEXT")
		  (tags "+@LRC/NEXT")
		  (tags "+Battlestation/NEXT")
		  (tags "+MOBILE/NEXT")
		  ))
		("H" "@HOME"
		 ((agenda "" ())
          (tags "+PRIORITY=\"A\"-ERRANDS-@LRC/NEXT")
		  (tags "+@HOME/NEXT")
		  (tags "+NB/NEXT")
		  (tags "+Battlestation/NEXT")
		  (tags "+MOBILE/NEXT")
		  ))
		("S" "Working Samsung/SAM_LEDL"
		 ((agenda "" ())
		  (tags "+@HOME+SAM_LEDL/NEXT")
		  (tags "+@LRC+SAM_LEDL/NEXT")
		  (tags "+NB+SAM_LEDL/NEXT")
		  (tags "+Battlestation+SAM_LEDL/NEXT")
		  (tags "+MOBILE+SAM_LEDL/NEXT")
		  (tags "+SAM_LEDL/NEXT")
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
		 ((tags "+MOBILE/NEXT")
		  (tags "+ERRANDS/NEXT")
		  ))
		("W" "Waiting"
		 ((todo "WAITING")
		  ))

		("w" "Week-long"
		 ((agenda "" ((org-agenda-span 15)))
		  ))

		("P" "Printed agenda"
		 ((tags "+ERRANDS+PLACE=\"\"/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nErrands (General)\n------------------\n")))
		  (tags "+ERRANDS+PLACE={ATK}/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nAtacadao\n------------------\n")))
		  (tags "+ERRANDS+PLACE={MKT}/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nMarket\n------------------\n")))
		  (tags "+ERRANDS+PLACE={HORT}/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nHortifruti\n------------------\n")))
		  (tags "+MOBILE/NEXT"
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
(setq org-global-properties '(("Effort_ALL". "0 0:05 0:15 0:25 0:50 2:00 3:00 4:00 6:00")))

;; Set global Column View format
(setq org-columns-default-format '"%38ITEM(Details) %1PRIORITY(P)  %7TODO(To Do) %5Effort(Effort){:} %6CLOCKSUM(Clock) %TAGS(Context)")

(setq org-use-property-inheritance t)

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 5)

;; Store analogic agendas when closing emacs
(add-hook 'kill-emacs-hook 'org-store-agenda-views)


(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("c" "IN" entry (file+headline "~/gtd/in.org" "IN")
		 "* NEXT %?\n%u" :prepend nil)))


(require 'org-gcal)
(defconst my-secrets-file "~/gtd/Files_Projects/.emacs_conf.d/secrets.el")

(if (file-exists-p my-secrets-file)
	(load-file my-secrets-file)
  )

(if (boundp 'my-gcal-definition)
    (setq org-gcal-client-id my-gcal-client-id
          org-gcal-client-secret my-gcal-client-secret
          org-gcal-file-alist '(("fernandhenriqp@gmail.com" .  "~/gtd/calendar.org")
                                )
          )
  )

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync nil t) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))

;; Babel Load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   ))

;; GTD implementation
(setq org-tag-alist '(
					  ("@LRC" . ?l)
					  ("@HOME" . ?h)
					  ("DW" . ?d)
					  ("MOBILE" . ?m)
					  ("NB" . ?n)
					  ("Nelson" . ?N)
					  ("SAM_LEDL" . ?S)
					  ("Carlos" . ?C)
					  ("Battlestation" . ?b)
					  ("ERRANDS" . ?e)
					  )
	  )


;;https://orgmode.org/manual/Tracking-TODO-state-changes.html#Tracking-TODO-state-changes
(setq org-todo-keywords
      '((sequence "TICKLED(T!)" "SCHED(s!)" "TODO(t!)" "NEXT(n!)" "WAITING(w!)" "POSTPONED(p!)" "|" "DONE(d)" "DELEGATED(o)" "Cancelled(c!)")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-use-tag-inheritance t)


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
 '(org-stuck-projects
   (quote
	("+LEVEL=1/-DONE"
	 ("TODO" "NEXT" "NEXTACTION")
	 nil "")))
 '(package-selected-packages
   (quote
    (atomic-chrome org-ref yasnippet-snippets company-auctex auctex yasnippet-classic-snippets sx exec-path-from-shell company-jedi highlight-indent-guides company-anaconda rtags diminish company-irony irony markdown-mode+ markdown-mode academic-phrases borg deferred org-gcal helm-ag helm anaconda-mode zenburn-theme w3m visible-mark smex smartparens python-environment py-autopep8 powerline org noctilux-theme material-theme magit impatient-mode iedit ggtags flycheck find-file-in-repository expand-region elpy ctags-update ctable avy auto-complete ag)))
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
