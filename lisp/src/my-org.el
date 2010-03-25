;;; my-org.el

(require 'org-install)

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

(defun my-org-mode-hook ()
  (define-key org-mode-map (kbd "C-v RET") 'my-org-insert-heading)
  (define-key org-mode-map (kbd "C-v t") (lambda() (interactive) (org-todo '(4))))
  (define-key org-mode-map (kbd "C-v #") 'org-priority)
  (define-key org-mode-map (kbd "C-v :") 'org-set-tags)
  (define-key org-mode-map (kbd "C-v !") 'my-org-insert-open-time-stamp)
  (define-key org-mode-map (kbd "C-v <") 'org-do-promote)
  (define-key org-mode-map (kbd "C-v >") 'org-do-demote)
  (define-key org-mode-map (kbd "C-v w") 'org-cut-subtree)
  (define-key org-mode-map (kbd "C-v y") 'org-paste-subtree)
  (define-key org-mode-map (kbd "C-v u") 'my-org-up-heading)
  (define-key org-mode-map (kbd "C-v p") 'outline-backward-same-level)
  (define-key org-mode-map (kbd "C-v n") 'outline-forward-same-level)
  (define-key org-mode-map (kbd "C-v s") 'org-sort-entries-or-items)
  (define-key org-mode-map (kbd "C-v x") 'my-org-handle-checkbox)
  (define-key org-mode-map (kbd "C-v TAB") 'org-cycle)
  (define-key org-mode-map (kbd "C-v a") 'org-archive-subtree)
  (font-lock-add-keywords nil '(("OPENED:" (0 'org-special-keyword t))) 'add-to-end))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'my-org)
