;;; org-journal-rcp.el --- This configures the org-journal system

;;; Code:
(use-package org-journal
 :init
 (setq org-journal-prefix-key "C-c j")
 :config
  (setq org-journal-dir "~/Dropbox/gtd/note-references/journal")
  (setq org-journal-file-format "%Y-%V")
  (setq org-journal-file-type 'weekly)

  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))

  (define-key org-journal-mode-map (kbd "C-c C-s") 'org-journal-save-entry-and-exit)

  (setq org-journal-time-prefix "\n** ")
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "#+TITLE: Weekly Journal\n#+TAGS: PREP(P) MEET(M) REVIEWED(R)\n#+STARTUP: folded\n\n")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

  (setq org-journal-file-header 'org-journal-file-header-func)
  )

(provide 'org-journal-rcp)
;;; Commentary:
;;
;;; org-journal-rcp.el ends here
