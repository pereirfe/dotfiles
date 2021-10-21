;;; org-roam-rcp.el --- This configures the org-journal system

;;; Code:
(use-package org-roam
  :after org
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :hook (org-mode . fp-org-hide-properties)
  :bind (("C-c n r" . org-roam-buffer-toggle) ;; Use f1?
         ("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n d" . org-roam-find-directory)
         ("C-c n n" . org-roam-capture)
         ("C-c n m" . org-roam-dailies-capture-today)
         ("C-c n s" . helm-rg))

  :config
  (setq org-roam-directory "~/gtd/note-references")
  (setq org-roam-link-file-path-type 'absolute)

  (setq org-roam-completion-system 'helm)
  (setq org-roam-dailies-directory "meetings/")
  (setq org-roam-capture-templates
        '(("s" "Subject" plain "%?"
           :target (file+head "%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: :subject: \n\n -- tags :: \n -- keywords :: \n\n")
           :empty-lines 1
           :unnarrowed t)
          ("p" "Project Details" plain "%?"
           :target (file+head "project-details/%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: :project: \n\n -- Project Name:: ")
           :unnarrowed t)
          ("n" "Quick notes" entry "* %?"
           :target (file+head "notes/%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: :note: \n\n -- tags :: \n -- keywords :: \n\n")
           :unnarrowed t)
          ("i" "Incident" plain "%?"
           "%?"
           :target (file+head "incidents/%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: :note: \n\n -- tags :: \n -- keywords :: \n\n")
           :unnarrowed t)
          )
        )

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

  (defun fp-org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun fp-org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun fp-org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (org-show-properties)
      (org-hide-properties)))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (f-split dirs)))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:50} ${backlinkscount:6}")


  )

(provide 'org-roam-rcp)
;;; Commentary:
;;
;;; org-roam-rcp.el ends here
