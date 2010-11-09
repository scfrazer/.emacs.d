;;; my-org.el

(require 'org-install)

(setq-default org-archive-location "%s_archive::"
              org-archive-mark-done nil
              org-archive-reversed-order t
              org-archive-save-context-info '(itags ltags olpath)
              org-completion-use-ido t
              org-cycle-include-plain-lists t
              org-cycle-separator-lines 1
              org-display-custom-times t
              org-hide-leading-stars nil
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
        (add-todo (org-entry-is-todo-p))
        (from-eol (eolp)))
    (call-interactively 'org-insert-heading)
    (unless from-eol
      (org-beginning-of-line))
    (when add-checkbox
      (insert "[ ] ")
      (org-update-checkbox-count))
    (when (and add-todo (org-at-heading-p))
      (insert "TODO "))))

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

(eval-after-load "org"
  '(progn
     (define-abbrev org-mode-abbrev-table "t" "TODO ")
     (defun my-org-beginning-of-line (&optional arg)
       "Copy of `org-beginning-of-line', but skips over checkboxes as well."
       (interactive "P")
       (let ((pos (point))
             (special (if (consp org-special-ctrl-a/e)
                          (car org-special-ctrl-a/e)
                        org-special-ctrl-a/e))
             refpos)
         (if (org-bound-and-true-p line-move-visual)
             (beginning-of-visual-line 1)
           (beginning-of-line 1))
         (if (and arg (fboundp 'move-beginning-of-line))
             (call-interactively 'move-beginning-of-line)
           (if (bobp)
               nil
             (backward-char 1)
             (if (org-invisible-p)
                 (while (and (not (bobp)) (org-invisible-p))
                   (backward-char 1)
                   (beginning-of-line 1))
               (forward-char 1))))
         (when special
           (cond
            ((and (looking-at org-complex-heading-regexp)
                  (= (char-after (match-end 1)) ?\ ))
             (setq refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
                               (point-at-eol)))
             (goto-char
              (if (eq special t)
                  (cond ((> pos refpos) refpos)
                        ((= pos (point)) refpos)
                        (t (point)))
                (cond ((> pos (point)) (point))
                      ((not (eq last-command this-command)) (point))
                      (t refpos)))))
            ((org-at-item-p)
             (goto-char
              (if (eq special t)
                  (let ((headline-pos (match-end 4)))
                    (save-excursion
                      (goto-char headline-pos)
                      (when (looking-at "\\[.\\] ")
                        (setq headline-pos (+ headline-pos 4))))
                    (cond ((> pos headline-pos) headline-pos)
                          ((= pos (point)) headline-pos)
                          (t (point))))
                (cond ((> pos (point)) (point))
                      ((not (eq last-command this-command)) (point))
                      (t (match-end 4))))))))
         (org-no-warnings
          (and (featurep 'xemacs) (setq zmacs-region-stays t)))))))

(defun my-org-copy-file-link ()
  "Create a file link by line number in the kill ring."
  (interactive)
  (kill-new (concat "[[" (buffer-file-name) "::" (number-to-string (line-number-at-pos)) "]]")))

(define-prefix-command 'my-org-mode-map)
(define-key my-org-mode-map (kbd "!") 'my-org-insert-open-time-stamp)
(define-key my-org-mode-map (kbd "#") 'org-priority)
(define-key my-org-mode-map (kbd ":") 'org-set-tags)
(define-key my-org-mode-map (kbd "<") 'org-do-promote)
(define-key my-org-mode-map (kbd ">") 'org-do-demote)
(define-key my-org-mode-map (kbd "L") 'org-insert-link)
(define-key my-org-mode-map (kbd "RET") 'my-org-insert-heading)
(define-key my-org-mode-map (kbd "TAB") 'org-cycle)
(define-key my-org-mode-map (kbd "a") 'org-archive-subtree)
(define-key my-org-mode-map (kbd "f") 'org-open-at-point)
(define-key my-org-mode-map (kbd "l") 'org-store-link)
(define-key my-org-mode-map (kbd "n") 'outline-forward-same-level)
(define-key my-org-mode-map (kbd "p") 'outline-backward-same-level)
(define-key my-org-mode-map (kbd "s") 'org-sort-entries-or-items)
(define-key my-org-mode-map (kbd "t") 'my-org-set-todo-state)
(define-key my-org-mode-map (kbd "u") 'my-org-up-heading)
(define-key my-org-mode-map (kbd "w") 'org-cut-subtree)
(define-key my-org-mode-map (kbd "x") 'my-org-handle-checkbox)
(define-key my-org-mode-map (kbd "y") 'org-paste-subtree)

(defun my-org-mode-hook ()
  (define-key org-mode-map (kbd "C-;") 'my-org-mode-map)
  (define-key org-mode-map (kbd "C-a") 'my-org-beginning-of-line)
  (font-lock-add-keywords nil '(("OPENED:" (0 'org-special-keyword t))) 'add-to-end))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'my-org)
