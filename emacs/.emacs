;; This .emacs file has been blessed by saint iGNUcius himself
;; on 31/may/2017 @CB03, Unicamp.
;; Any descendent of this file is, therefore, saint.

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


;; If some package is missing from melpa, try M-x package-refresh-contents
(defvar myPackages
  '(material-theme
    use-package
    nodejs-repl
    org-journal
    js2-mode  ;; Javascript with better syntax higlight
    js2-refactor ;; Js refactoring tools
    json-mode
    rjsx-mode
    xref-js2   ;; Js cross-references (AST-based)
    ;;company-tern ;; Js Autocomplete. Require npm tern
    lsp-mode
    company-lsp
    lsp-ui
    color-theme-sanityinc-tomorrow
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
    powerline
    auto-complete
    impatient-mode
    exec-path-from-shell
    visible-mark
    restclient
    ob-restclient
    yasnippet
    org-roam
    ob-async
    org-download
    dumb-jump
    tide
    elpy))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; Use Package
;; -----------
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;;;;;;;;;;;;;;; Org Journal
(require 'org-journal)
(setq org-journal-dir "~/Dropbox/gtd/note-references/journal")

;;;;;;;;;;;;;;; DIRED
;; Set ls -alh as default for dired
(setq dired-listing-switches "-alh")
(global-set-key (kbd "C-x C-d") 'dired)

(require 'tramp)
(setq tramp-default-method "ssh")

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;;;;;;;;;;;;;; ORG ROAM
(setq org-roam-directory "~/gtd/note-references")
(add-hook 'after-init-hook 'org-roam-mode)
(require 'org-tempo)

(setq org-roam-link-file-path-type 'absolute)

;;; Define key bindings for Org-roam
;; Define it just for org-roam?
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display) ;; Use f1?
(global-set-key (kbd "C-c n i") #'org-roam-insert)
(global-set-key (kbd "C-c n f") #'org-roam-find-file)
(global-set-key (kbd "C-c n b") #'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n d") #'org-roam-find-directory)
(global-set-key (kbd "C-c n n") #'org-roam-capture)
(global-set-key (kbd "C-c n m") #'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n s") #'helm-rg)

(setq org-roam-completion-system 'helm)

(setq org-roam-capture-templates
      '(("s" "Subject" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
         :head "#+title: ${title}\n#+roam_tags: subject \n\n -- tags :: \n -- keywords :: \n\n"
         :unnarrowed t)
        ("p" "Project Details" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%(format-time-string \"project-details/%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
         :head "#+title: ${title}\n#+roam_tags: project \n\n -- Project Name ::"
         :unnarrowed t)

        ("n" "Quick notes" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%(format-time-string \"./notes/%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
         :head "#+title: ${title}\n#+roam_tags: note \n\n -- tags :: \n -- keywords :: \n\n"
         :unnarrowed t)
        ("i" "Incident" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%(format-time-string \"./incidents/%Y-%m-%d--%H-%M-%SZ--incident-${slug}\" (current-time) t)"
         :head "#+title: Incident: ${title}\n#+roam_tags: incident \n\n -- tags :: \n -- keywords :: \n\n"
         :unnarrowed t)
        )
      )


(setq org-roam-dailies-directory "meetings/")

(setq org-roam-dailies-capture-templates
      '(("a" "Ad-hoc meeting"  plain (function org-roam--capture-get-point)
         "* %?"
         :file-name "meetings/ad-hoc-%(format-time-string \"%Y\" (current-time) t)"
         :head "#+title: ${title}\n#+roam_tags: meeting \n"
         :unnarrowed t
         )
        ("e" "Escale Journal"  entry (function org-roam--capture-get-point)
         "* %u"
         :file-name "meetings/journal-%<%Y>"
         :head "#+title: Journal\n#+roam_tags: journal \n"
         :unnarrowed t
         :olp ("Escale")
         )
        ("j" "Journal"  entry (function org-roam--capture-get-point)
         "* %u"
         :file-name "meetings/journal-%<%Y>"
         :head "#+title: Journal\n#+roam_tags: journal \n"
         :unnarrowed t
         :olp ("Personal Journal")
         )
        ("p" "Planning" plain (function org-roam--capture-get-point)
"* %u SPRINT #%^{PROMPT}

  Pré planning checklist (Use C-c C-x C-b):
    - [ ] Feriados Verificados
    - [ ] Férias Verificadas
    - [ ] Grandes eventos verificados

  Planning Checklist:
    - [ ] Objetivo da sprint Definido:
    - [ ] Pontuação Sprint Anterior Verificada:
    - [ ] Ballpark pontuação para esta sprint:
    - [ ] Link para o [[https://www.scrumpoker-online.org/en/][Scrum Poker Online]] Compartilhado
    - [ ] Escolher Tarefas Do backlog tais que encaixam no Objetivo
      - [ ] Um plano claro de como fazer está definido
      - [ ] Todos os recursos necessários existem

** Notas
%?"
        :prepend t
        :jump-to-captured t
        :empty-lines 1
        :file-name "meetings/ccc-planning"
        :head "#+title: Planning\n#+roam_tags: ccc planning meeting\n"
        :unnarrowed t
        )

        ("t" "Tim Weekly" plain (function org-roam--capture-get-point)
         "* %u Tim Weekly\n%?"
         :prepend t
         :jump-to-captured t
         :empty-lines 1
         :file-name "meetings/ccc-tim-weekly"
         :head "#+title: Tim Weekly\n#+roam_tags: escale ccc tim\n"
         :unnarrowed t
         )
        ("1" "1o1 Ft Jullyana " plain (function org-roam--capture-get-point)
         "* %u 1o1 Ft Jullyana\n%?"
         :prepend t
         :jump-to-captured t
         :empty-lines 1
         :file-name "meetings/1o1-jullyana"
         :head "#+title: 1o1's Ft Jullyana\n#+roam_tags: ccc escale 1o1\n"
         :unnarrowed t
         )
        ("r" "CCC Review" plain (function org-roam--capture-get-point)
         "* %u CCC Review Sprint #%^{Sprint #}\n  - [[https://escale.atlassian.net/secure/RapidBoard.jspa?projectKey=CCC&rapidView=284][BOARD]]\n%?"
         :prepend t
         :jump-to-captured t
         :empty-lines 1
         :file-name "meetings/ccc-review"
         :head "#+title: CCC Review \n#+roam_tags: ccc escale review\n"
         :unnarrowed t
         )
        )
      )

;;;;;;;;;;;;;;;; Org Download
(require 'org-download)

(global-set-key (kbd "C-c d y") 'org-download-yank)
(global-set-key (kbd "C-c d Y") 'org-download-screenshot)
(setq-default org-download-image-dir "~/gtd/note-references/.imgsrc")


;;;;;;;;;;;;;;;; FUNDAMENTAL TEXT EDITION
(setq-default word-wrap t)
(setq-default truncate-lines nil)

;; Remove non-ascii characters
(defun xah-asciify-text (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

;; Configure column max width for orgmode
(setq org-ellipsis "▼")

;; Remove Auto-generated buffers from cycling
;; (But keep buffers of interest)
(defun my-buffer-predicate (buffer)
  (not
   (and (string-match-p "^\\*.*" (buffer-name buffer))
        (not
         (or
          (string-match-p "\\*Org Agenda\\*" (buffer-name buffer))
          (string-match-p "\\*scratch\\*" (buffer-name buffer)))
         )
        )
   )
  )

(set-frame-parameter nil 'buffer-predicate #'my-buffer-predicate)
(push (cons 'buffer-predicate #'my-buffer-predicate) default-frame-alist)

;;;;;;;;;;;;;;;; LATEX

(setq-default TeX-master nil) ; Query for master file.
(add-hook 'latex-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'latex-mode-hook (lambda () (setq word-wrap t)))
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'visual-line-mode)
;;(add-hook 'latex-mode-hook #'company-mode)
(add-hook 'latex-mode-hook #'reftex-mode)
(add-hook 'latex-mode-hook (lambda () (company-mode -1)))

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
;; Disable toggle input methon
(global-unset-key (kbd "C-\\"))

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



;; Activate highlight indentation
(add-hook 'prog-mode-hook #'highlight-indentation-mode)

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
(setq org-confirm-babel-evaluate nil)

(setq org-special-ctrl-a/e t) ;; Set C-a/e to go behave in relation to the headline
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(require 'ob-js)
(require 'ob-async)

;; (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
;; (add-to-list 'org-babel-load-languages '(js . t))
;; (add-to-list 'org-babel-load-languages '(restclient . t))

;; Load languages to run on ORG
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)
   (emacs-lisp . t)
   (shell . t)
   (js . t)
   (http . t)
   ))



(setq company-dabbrev-downcase nil)

(defun my-org-refile-goto ()
  (interactive)
  (org-refile '(4))
  )

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-g") 'my-org-refile-goto)))

(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((nil :maxlevel . 1)
                           (("~/gtd/someday.org"
                             "~/gtd/projects.org"
                             "~/gtd/soon.org"
                             "~/gtd/escale.org"
                             "~/gtd/tickler.org"
                             "~/gtd/tasks.org") :maxlevel . 1)))
(setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling
(setq org-outline-path-complete-in-steps nil)

(exec-path-from-shell-initialize)
(setq x-select-enable-clipboard t
	  x-select-enable-primary t)

(setq org-stuck-projects '("+LEVEL=1" ("NEXT") nil "org-gcal:"))

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
(setq confirm-kill-emacs 'y-or-n-p)
(set-face-attribute 'default nil :height 140)

(setq eval-expression-print-length nil)

;;(setq custom-safe-themes t)
(require 'iso-transl)

(load-theme 'sanityinc-tomorrow-night t)

(when (member "IBM Plex Mono" (font-family-list))
  (set-face-attribute 'default nil :font "IBM Plex Mono"))

;; Powerline config
(powerline-center-theme)
(setq powerline-default-separator 'wave)


;;;;;;;;;;;;;;;; Integration
;; Allow Integration with googlechrome
;; (require 'atomic-chrome)
;; (atomic-chrome-start-server)
;; (setq atomic-chrome-buffer-open-style 'frame)
;; (setq atomic-chrome-default-major-mode 'LaTeX-mode)
;; ;; (setq atomic-chrome-url-major-mode-alist
;; ;;       '(("overleaf\\.com" . 'LaTeX-mode)
;; ;;         ("github\\.com" . 'python-mode)))

;;;;;;;;;;;;;;;; CODING / Code

;;;;;; Yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;;;;; Format on save when on terraform mode
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;;;;; Dockerfile
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . conf-mode))

;;;; SHELL
(add-hook 'eshell-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'eshell-mode-hook (lambda () (setq word-wrap t)))
(add-hook 'eshell-mode-hook #'smartparens-mode)
(add-hook 'eshell-mode-hook #'visual-line-mode)

;;;; Javascript
;;(require 'json)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-hook 'javascript-mode-hook (lambda () (setq font-lock-mode nil)))

;; use eslint_d insetad of eslint for faster linting
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)  ;; Are you sure?
(setq-default flycheck-global-modes '(not org-mode)) ;; Disable flycheck on orgmode

;; Setup TIDE
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;(add-hook 'before-save-hook 'tide-format-before-save)
;(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(add-hook 'js2-mode-hook #'setup-tide-mode)


;; Use eslint defaut swich-case indentation
(setq js-switch-indent-offset 2)

;; Add jest external function
;; (js2-mode
;;   (js2-additional-externs
;;    . (list "jest" "describe" "expect" "require" "beforeEach" "require" "it" "spyOn")))

;; Require Repl
(require 'nodejs-repl)

;; (defun fp-nodejs-repl-send-dwim ()
;;   (interactive)
;;   (if (bound-and-true-p visual-line-mode)
;;       (nodejs-repl-send-region ??)
;;     (nodejs-repl-send-line)
;;     )
;;  )

(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            ;; (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            ;; (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            ;; (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            ;; (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
            ))


;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

(setq flycheck-javascript-eslint-executable "eslint_d")

(setq js2-strict-missing-semi-warning nil)


;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

;; Disable code folding
(define-key js2-mode-map (kbd "C-c C-o") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;; Try to highlight most ECMA built-ins
(setq js2-highlight-level 3)
;; have a shorter idle time delay
(setq js2-idle-timer-delay 0.1)

;; turn off all warnings in js2-mode
(setq js2-mode-show-parse-errors t
      js2-mode-show-strict-warnings nil
      js2-strict-missing-semi-warning nil
      js2-strict-trailing-comma-warning nil)

(require 'company)
;; (require 'company-tern)

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


;;(add-to-list 'company-backends 'company-tern)
;; (add-hook 'js2-mode-hook (lambda ()
;;                            (tern-mode)
;;                            (company-mode)))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Disable completion keybindings, as we use xref-js2 instead
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)
;; (define-key tern-mode-keymap (kbd "C-c C-r") nil)
(define-key js-mode-map (kbd "M-.") nil)

(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
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
(setq-default bidi-display-reordering nil)
(set-default 'truncate-lines t)
(add-hook 'c++-mode-hook '(lambda ()
                            (setq c-basic-offset 4)))
(add-hook 'c++-mode-hook '(lambda ()
                            (setq tab-width 4)))

;; Magit Configuration
(global-set-key (kbd "C-x g") 'magit-status)

(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'css-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'awk-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
(add-hook 'restclient-mode-hook #'smartparens-mode)

(define-key smartparens-mode-map (kbd "M-]") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-[") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-c u") 'sp-unwrap-sexp)

(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(define-key smartparens-mode-map (kbd "C-c w (") 'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c w [") 'wrap-with-brackets)
(define-key smartparens-mode-map (kbd "C-c w {") 'wrap-with-braces)
(define-key smartparens-mode-map (kbd "C-c w '") 'wrap-with-single-quotes)
(define-key smartparens-mode-map (kbd "C-c w \"") 'wrap-with-double-quotes)
(define-key smartparens-mode-map (kbd "C-c w `") 'wrap-with-back-quotes)

;; Enable Autocomplete
;(ac-config-default)

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


;; LSP
;;https://github.com/emacs-lsp/lsp-mode
(require 'lsp-mode)
(add-hook 'js2-mode-hook #'lsp)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-show-numbers t)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(setq company-transformers nil
      company-lsp-async t
      company-lsp-cache-candidates nil)

(add-hook 'lsp-mode-hook (lambda() (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)))
(add-hook 'lsp-mode-hook (lambda() (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

;;;; (PYTHON)
(elpy-enable)
(pyvenv-workon "python3emacs")

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
(setq projectile-remember-window-configs t)

(projectile-mode +1)
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
;;(add-hook 'org-mode-hook (lambda () (company-mode -1)))
(add-to-list 'company-backends 'company-capf)

(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(setq org-agenda-files
      (quote
       ("~/gtd/projects.org"
        "~/gtd/escale.org"
        "~/gtd/tickler.org"
        "~/gtd/events.org"
        "~/gtd/birthdays.org"
        "~/gtd/tasks.org")))


(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)

(defun fp-org-open-4kft ()
  "Open the 4kft org file."
  (interactive)
  (org-open-file "~/gtd/4kft.org")
  )

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
      ;; M -> Meeting
      '(("E" "@ESCALE"
	 ((agenda ""
                  (
                   (org-agenda-time-grid nil)
                   (org-agenda-skip-function '(org-agenda-skip-subtree-if 'regexp ":@HOME"))
                   )
                  )
	  (tags "-Jullyana-Tamyres-toSTUDY+@ESCALE/NEXT")
          (tags "+Jullyana/!-DONE-Cancelled")
          (tags "+Tamyres/!-DONE-Cancelled")
          ))
	("H" "@HOME"
	 ((agenda "" ())
	  (tags "+@HOME/NEXT")
	  (tags "+Battlestation/NEXT")
	  (tags "+MOBILE/NEXT")
	  ))
        ("S" "toSTUDY"
	 ((tags "+toSTUDY/NEXT")
	  ))
        ("T" "Thesis"
	 ((tags "+Thesis/NEXT")
	  ))
	("MN" "Nelson"
	 ((tags "+Nelson+TODO=\"NEXT\"|+Nelson+TODO=\"WAITING\""
		((org-agenda-prefix-format "[ ] %-20b:")
		 (org-agenda-sorting-strategy '(tag-up priority-down))
		 (org-agenda-overriding-header "\nReunião Nelson\n------------------\n"))))
         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t)))
		("MC" "Carlos"
		 ((tags "+Carlos+TODO=\"NEXT\"|+Carlos+TODO=\"WAITING\"")
		  ))
		("O" "MOBILE+OUTSIDE"
		 ((tags "+MOBILE/NEXT")
		  (tags "+OUTSIDE/NEXT")
		  ))
		("W" "Waiting"
		 ((todo "WAITING")
		  ))

		("w" "Week-long"
		 ((agenda "" ((org-agenda-span 15)))
		  ))

		("P" "Printed agenda"
		 ((tags "+OUTSIDE+PLACE=\"\"/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nErrands (General)\n------------------\n")))
		  (tags "+MOBILE/NEXT"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nMobile\n------------------\n"))))
		 ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
		  (ps-landscape-mode t))
         ("~/gtd/Offline/OUTSIDE.txt"))
		)
      )

;; Effort and global properties
(setq org-global-properties '(("Effort_ALL". "0:01 0:05 0:15 0:25 0:50 1:30 3:00 4:00 6:00")))

;; Set global Column View format
(setq org-columns-default-format '"%50ITEM(Details) %1PRIORITY(P)  %7TODO(To Do) %5Effort(Effort){:} %6CLOCKSUM(Clock) %TAGS(Context)")

(setq org-use-property-inheritance t)

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 5)

;; Store analogic agendas when closing emacs
;; (add-hook 'kill-emacs-hook 'org-store-agenda-views)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(
        ("c" "IN" entry (file+headline "~/gtd/in.org" "IN")
         "* NEXT %?\n%U" :prepend nil)

        ("S" "toSTUDY" entry (file+headline "~/gtd/tasks.org" "TASKS")
         "* NEXT %? :toSTUDY: \n%u\n" :prepend 1 :empty-lines 1)

        ("e" "Escale")
        ("et" "Escale Task" entry (file+headline "~/gtd/escale.org" "TASKS")
         "* NEXT %? %^g \n%u\n" :prepend 1 :empty-lines 1)
        ("ep" "Escale Project" entry (file "~/gtd/escale.org")
         "* %?%\n" :prepend nil :empty-lines 1)
        ("es" "Escale This Sprint" entry (file+headline "~/gtd/escale.org" "THIS SPRINT")
         "* NEXT %? :@ESCALE: \n%u\n" :prepend 1 :empty-lines 1)

        ("g" "General")
        ("gt" "General Task" entry (file+headline "~/gtd/tasks.org" "TASKS")
         "* NEXT %? %^g\n%u\n" :prepend 1 :empty-lines 1)
        ("gw" "General Waiting" entry (file+headline "~/gtd/tasks.org" "TASKS")
         "* WAITING  %? %^g\n%u\n" :prepend 1 :empty-lines 1)

        ("i" "Items")
        ("im" "Item: Movies" entry (file+headline "~/gtd/someday.org" "Watch Movies")
         "* %u %?" :prepend t)
        ("ib" "Item: Books" entry (file+headline "~/gtd/someday.org" "Read Books")
         "* %u %?" :prepend t)
        ("ir" "Item: Read" entry (file+headline "~/gtd/someday.org" "Short reads")
         "* %u %?" :prepend t)
        ("is" "Item: Stuff to Buy" entry (file+headline "~/gtd/someday.org" "Stuff to Buy")
         "* %u %?" :prepend t)
        )
      )


;; 2020-01-19: Deactivate Org-gcal
;; (require 'org-gcal)
;; (defconst my-secrets-file "~/gtd/Files_Projects/.emacs_conf.d/secrets.el")

;; (if (file-exists-p my-secrets-file)
;; 	(load-file my-secrets-file)
;;   )

;; (if (boundp 'my-gcal-definition)
;;     (setq org-gcal-client-id my-gcal-client-id
;;           org-gcal-client-secret my-gcal-client-secret
;;           org-gcal-file-alist '(("fernandhenriqp@gmail.com" .  "~/gtd/.cal.org")
;;                                 ("fernando.pereira@escale.com.br" . "~/gtd/.esc_cal.org")
;;                                 )
;;           )
;;   )
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync nil t) ))
;; ;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))


;; GTD implementation
(setq org-tag-alist '((:startgroup . nil)
                      ("@HOME" . ?h)
                      ("@ESCALE" . ?e)
                      ("OUTSIDE" . ?o)
                      (:endgroup . nil)
                      (:startgroup . nil)
                      ("LowEnergy" . ?L)
                      ("HighEnergy" . ?H)
                      (:endgroup . nil)
                      ("toSTUDY" . ?s)
		      ("MOBILE" . ?m)
		      ("Battlestation" . ?b)
                      ("Thesis" . ?t)
		      )
      )


;;https://orgmode.org/manual/Tracking-TODO-state-changes.html#Tracking-TODO-state-changes
(setq org-todo-keywords
      '((sequence "TICKLED(T)" "SCHED(s)" "NEXT(n)" "WAITING(w)" "POSTPONED(p)" "|" "DONE(d)" "DELEGATED(o)" "Cancelled(c)")))

;; (setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-use-tag-inheritance t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode nil)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/gtd/projects.org" "~/gtd/escale.org" "~/gtd/tickler.org" "~/gtd/events.org" "~/gtd/birthdays.org" "~/gtd/tasks.org")))
 '(org-stuck-projects
   (quote
    ("+LEVEL=1/-DONE"
     ("TODO" "NEXT" "NEXTACTION")
     nil "")))
 '(package-selected-packages
   (quote
    (org-journal kubernetes dumb-jump org-download ob-async helm-rg org-roam ob-http terraform-mode jq-mode jq-format list-packages-ext js-comint forge jedi babel yaml-mode ace-window csv-mode atomic-chrome org-ref yasnippet-snippets company-auctex auctex yasnippet-classic-snippets sx exec-path-from-shell company-jedi highlight-indent-guides company-anaconda rtags diminish company-irony irony markdown-mode+ markdown-mode academic-phrases borg deferred helm-ag helm anaconda-mode zenburn-theme w3m visible-mark smartparens python-environment py-autopep8 powerline org noctilux-theme material-theme magit impatient-mode ggtags flycheck find-file-in-repository expand-region elpy ctags-update ctable avy auto-complete ag)))
 '(projectile-other-file-alist
   (quote
    (("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "h" "hh" "hpp")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" "")
     ("js" "js" "test.js"))))
 '(safe-local-variable-values
   (quote
    ((TeX-master . "../hydra.tex")
     (TeX-master . t)
     (eval add-hook
           (quote after-save-hook)
           (quote org-html-export-to-html)
           t t))))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
