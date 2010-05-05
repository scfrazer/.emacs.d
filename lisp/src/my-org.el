;;; my-org.el

(require 'org-install)

(setq-default org-archive-location "%s_archive::"
              org-archive-mark-done nil
              org-archive-save-context-info '(itags ltags olpath)
              org-completion-use-ido t
              org-cycle-include-plain-lists t
              org-cycle-separator-lines 1
              org-display-custom-times t
              org-hide-leading-stars t
              org-id-track-globally nil
              org-log-done 'time
              org-modules nil
              org-priority-faces '((?A . (:foreground "LightPink2" :weight bold))
                                   (?B . (:foreground "SkyBlue1" :weight bold))
                                   (?C . (:foreground "DarkSeaGreen3" :weight bold)))
              org-read-date-popup-calendar nil
              org-replace-disputed-keys t
              org-special-ctrl-a/e t
              org-special-ctrl-k t
              org-startup-folded nil
              org-tags-column -80
              org-time-stamp-custom-formats '("<%a %b %d, %Y>" . "<%a %b %d, %Y %H:%M>")
              org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")
                                  (sequence "MAYBE" "SOMEDAY" "|" "CANCELED")
                                  (sequence "|" "REASSIGNED"))
              org-todo-keyword-faces '(("TODO"       . (:foreground "IndianRed1" :weight bold))
                                       ("STARTED"    . (:foreground "DeepSkyBlue1" :weight bold))
                                       ("WAITING"    . (:foreground "Yellow2" :weight bold))
                                       ("DONE"       . (:foreground "PaleGreen2" :weight bold))
                                       ("MAYBE"      . (:foreground "DeepSkyBlue4" :weight bold))
                                       ("SOMEDAY"    . (:foreground "Yellow4" :weight bold))
                                       ("CANCELED"   . (:foreground "PaleGreen4" :weight bold))
                                       ("REASSIGNED" . (:foreground "PaleGreen4" :weight bold)))
              org-yank-folded-subtrees nil)

(defun my-org-insert-heading ()
  "Insert a heading if on a blank line, or goto next line and insert heading."
  (interactive)
  (let ((add-checkbox (org-at-item-checkbox-p))
        (from-eol (eolp)))
    (call-interactively 'org-insert-heading)
    (unless from-eol
      (org-beginning-of-line))
    (when add-checkbox
      (insert "[ ] ")
      (org-update-checkbox-count))))

(defun my-org-handle-checkbox ()
  "On a checkbox: Toggle it.
On a heading: Add or update checkbox count.
Otherwise: Add a checkbox and update heading accordingly."
  (interactive)
  (cond ((org-at-item-checkbox-p)
         (org-toggle-checkbox))
        ((org-at-heading-p)
         (my-org-add-or-update-checkbox-count))
        (t
         (back-to-indentation)
         (insert "- [ ] ")
         (indent-according-to-mode)
         (save-excursion
           (org-back-to-heading)
           (my-org-add-or-update-checkbox-count)))))

(defun my-org-add-or-update-checkbox-count ()
  "Add or update a checkbox count."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\[[0-9]*/[0-9]*\]" (point-at-eol) t)
        (org-update-checkbox-count)
      (org-end-of-line)
      (insert " [/]")
      (org-update-checkbox-count))))

(defun my-org-insert-open-time-stamp ()
  "Put in an opening timestamp."
  (interactive)
  (save-excursion
    (insert "OPENED: ")
    (org-time-stamp-inactive t)))

(defun my-org-up-heading()
  "Go to heading, or up."
  (interactive)
  (if (org-at-heading-p)
      (org-up-heading-safe)
    (org-back-to-heading)))

(defun my-org-set-todo-state ()
  "Set todo state using ido."
  (interactive)
  (org-todo '(4)))

(define-prefix-command 'my-org-mode-map)
(define-key my-org-mode-map (kbd "RET") 'my-org-insert-heading)
(define-key my-org-mode-map (kbd "t") 'my-org-set-todo-state)
(define-key my-org-mode-map (kbd "#") 'org-priority)
(define-key my-org-mode-map (kbd ":") 'org-set-tags)
(define-key my-org-mode-map (kbd "!") 'my-org-insert-open-time-stamp)
(define-key my-org-mode-map (kbd "<") 'org-do-promote)
(define-key my-org-mode-map (kbd ">") 'org-do-demote)
(define-key my-org-mode-map (kbd "w") 'org-cut-subtree)
(define-key my-org-mode-map (kbd "y") 'org-paste-subtree)
(define-key my-org-mode-map (kbd "u") 'my-org-up-heading)
(define-key my-org-mode-map (kbd "p") 'outline-backward-same-level)
(define-key my-org-mode-map (kbd "n") 'outline-forward-same-level)
(define-key my-org-mode-map (kbd "s") 'org-sort-entries-or-items)
(define-key my-org-mode-map (kbd "x") 'my-org-handle-checkbox)
(define-key my-org-mode-map (kbd "TAB") 'org-cycle)
(define-key my-org-mode-map (kbd "a") 'org-archive-subtree)

(defun my-org-mode-hook ()
  (define-key org-mode-map (kbd "C-;") 'my-org-mode-map)
  (font-lock-add-keywords nil '(("OPENED:" (0 'org-special-keyword t))) 'add-to-end))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'my-org)
