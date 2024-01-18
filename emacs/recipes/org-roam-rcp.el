
;;; org-roam-rcp.el --- This configures the org-journal system

;;; Code:
(use-package org-roam
  :after org
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :hook
  (org-mode . fp-org-hide-properties)
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
           :target (file+head "%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: \n\n -- tags :: \n -- keywords :: \n\n")
           :empty-lines 1
           :unnarrowed t)
          ("p" "Project Details" plain "%?"
           :target (file+head "project-details/%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: :project: \n\n -- Project Name:: ")
           :unnarrowed t)
          ("n" "Quick notes" entry "* %?"
           :target (file+head "notes/%<%Y-%m-%d--%H-%M-%SZ>--${slug}.org" "#+title: ${title}\n#+filetags: \n\n -- tags :: \n -- keywords :: \n\n")
           :unnarrowed t)
          ("t" "Task solving" plain (file "~/gtd/note-references/Templates/task-solving.org")
           :if-new (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
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
          ("e" "Wildlife Journal"  entry (function org-roam--capture-get-point)
           "* %u"
           :file-name "meetings/journal-%<%Y>"
           :head "#+title: Journal\n#+roam_tags: journal \n"
           :unnarrowed t
           :olp ("Wildlife")
           )
          ("j" "Journal"  entry (function org-roam--capture-get-point)
           "* %u"
           :file-name "meetings/journal-%<%Y>"
           :head "#+title: Journal\n#+roam_tags: journal \n"
           :unnarrowed t
           :olp ("Personal Journal")
           )
          ("1" "1o1 Ft Regis " plain "* %u 1o1 Ft Regis\n%?"
           :target (file+head "1o1-regis.org" "#+title: 1o1's Ft Regis\n#+filetags: ccc wildlife 1o1\n")
           :prepend t
           :jump-to-captured t
           :empty-lines 1
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

  (setq org-roam-node-display-template "${directories:25} ${tags:15} ${title:50} ${backlinkscount:6}")

  ;; Run this function when having issues with id references not being found
  (defun fp-org-id-update-org-roam-files ()
    "Update Org-ID locations for all Org-roam files."
    (interactive)
    (org-id-update-id-locations (directory-files-recursively "~/gtd/note-references" "org$") ))

  )

(provide 'org-roam-rcp)
;;; Commentary:
;;
;;; org-roam-rcp.el ends here
