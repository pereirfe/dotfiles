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


;; Basic Customization
;; --------------------

(setq org-agenda-custom-commands
      '(("O" "Office block agenda"
         ((agenda "" ((org-agenda-ndays 7)))
                      ;; limits the agenda display to a single day
          (tags-todo "+PRIORITY=\"A\"")
          (tags-todo "MSC|ORG")
          (todo "WAITING"))
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
        ;; ...other commands here
        ))

; I prefer return to activate a link
(setq org-return-follows-link t)

;; Disable C-z suspend
;;(global-unset-key [(control z)])
;;(global-unset-key [(control x)(control z)])

;; Magit Configuration
(global-set-key (kbd "C-x g") 'magit-status)

;; Disable C-t toggle character
(global-unset-key [(control t)])

;; Move backup files from working directory
(setq backup-directory-alist `(("." . "~/.saves")))

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
(define-key global-map (kbd "C-ç") 'iedit-mode)

;; Enable SMEX and bind it to M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Auto-revert Mode Global
;;(global-auto-revert-mode 1)

;; GTD implementation
(setq org-tag-alist '(("@LRC" . ?l)
		      ("@HOME" . ?h)
		      ("MOBILE" . ?m)
		      ("Nelson" . ?n)
		      ("Carlos" . ?c)
		      ("TEL" . ?t)
		      ("ERRANDS" . ?e)))

 (setq org-todo-keywords
       '((sequence "TODO" "WAITING" "NEXT" "SCHED" "|" "DONE" "DELEGATED(@d)")))


;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;(setq elpy-rpc-backend "jedi")

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

;; Start Server for emacs
;;(server-start)

;; Delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; CEDET
;; Turn on semantics
(semantic-mode 1)
;; define function to add semantics to autocomplete @c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; Turn automatic reparsing when idle
(global-semantic-idle-scheduler-mode 1)

;; Trying to add the Project
(global-ede-mode t)
(ede-cpp-root-project "LTE-Sim" :file "~/lte-sim-fp/src/LTE-Sim.cpp")


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
    ("~/gtd/Holidays.org" "~/gtd/birthdays.org" "~/gtd/projects.org" "~/gtd/someday.org" "~/gtd/tasks.org")))
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