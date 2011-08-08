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
              org-export-author-info nil
              org-export-creator-info nil
              org-export-time-stamp-file nil
              org-export-with-toc nil
              org-export-with-section-numbers nil
              org-export-html-inline-images t
              org-export-html-style-include-default nil
              org-export-html-validation-link ""
              org-export-htmlize-output-type 'inline-css
              org-hide-leading-stars nil
              org-id-track-globally nil
              org-imenu-depth 6
              org-link-frame-setup '((file . find-file))
              org-log-done 'time
              org-modules nil
              org-priority-faces '((?A . (:foreground "LightPink2" :weight bold))
                                   (?B . (:foreground "SkyBlue1" :weight bold))
                                   (?C . (:foreground "DarkSeaGreen3" :weight bold)))
              org-read-date-popup-calendar nil
              org-replace-disputed-keys t
              org-return-follows-link t
              org-src-fontify-natively t
              org-special-ctrl-a/e t
              org-special-ctrl-k t
              org-speed-commands-user '(("#" . org-priority)
                                        (":" . org-set-tags)
                                        ("<" . org-promote-subtree)
                                        (">" . org-demote-subtree)
                                        ("t" . my-org-set-todo-state)
                                        ("a" . org-archive-subtree)
                                        ("x" . my-org-handle-checkbox)
                                        ("w" . my-org-cut-subtree)
                                        ("y" . org-paste-subtree))
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
              org-use-speed-commands t
              org-yank-folded-subtrees nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-cut-subtree ()
  "org-cut-subtree then go to next."
  (interactive)
  (org-cut-subtree)
  (skip-syntax-forward " "))

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
         (if (looking-at "- ")
             (forward-char 2)
           (insert "- "))
         (insert "[ ] ")
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

(defun my-checkbox-statistics-hook ()
  (save-excursion
    (org-back-to-heading t)
    (when (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" (point-at-eol) t)
      (if (match-string 1)
          (when (equal (match-string 1) "100%")
            (org-todo 'done))
        (if (and (> (match-end 2) (match-beginning 2))
                 (equal (match-string 2) (match-string 3)))
            (org-todo 'done))))))

(add-hook 'org-checkbox-statistics-hook 'my-checkbox-statistics-hook)

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

(defadvice org-beginning-of-line (after my-org-beginning-of-line activate)
  (when (looking-at "\\[.\\] ")
    (forward-char 4)))

(defun my-org-copy-file-link ()
  "Create a file link by line number in the kill ring."
  (interactive)
  (kill-new (concat "[[" (buffer-file-name) "::" (number-to-string (line-number-at-pos)) "]]")))

(defadvice org-archive-subtree (after my-org-archive-subtree activate)
  (let* ((location (org-get-local-archive-location))
         (afile (org-extract-archive-file location))
         (buf (find-buffer-visiting afile)))
    (when buf
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "org"
  '(progn

     (define-abbrev org-mode-abbrev-table
       "td"
       "TODO ")

     (define-abbrev org-mode-abbrev-table
       "ti"
       "#+title: \n"
       (lambda () (backward-char 1)))

     (define-abbrev org-mode-abbrev-table
       "toc"
       "#+options: toc:t num:t\n")

     (define-abbrev org-mode-abbrev-table
       "email"
       "#+email_addrs: scfrazer\n")

     (define-abbrev org-mode-abbrev-table
       "new"
       "<new></new>"
       (lambda () (backward-char 6)))

     (define-abbrev org-mode-abbrev-table
       "exam"
       "#+begin_example\n\n#+end_example"
       (lambda () (backward-char 14)))

     (define-abbrev org-mode-abbrev-table
       "src"
       "#+begin_src \n\n#+end_src"
       (lambda ()
         (let ((mode (read-from-minibuffer "Mode? ")))
           (when (eobp)
             (insert "\n")
             (backward-char 1))
           (backward-char 11)
           (insert mode)
           (forward-char 1))))

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface my-org-new-face
  '((t (:background "navy")))
  "Visible tab chars."
  :group 'faces)

(defvar my-org-export-preprocess-replacement-alist
  '(("<new>" . "@<span class=\"new\">")
    ("</new>" . "@</span>"))
  "*Export preprocess replacements")

(defun my-org-export-preprocess-hook ()
  "Export preprocess hook."
  (goto-char (point-min))
  (dolist (item my-org-export-preprocess-replacement-alist)
    (let ((key (car item))
          (repl (cdr item)))
      (while (re-search-forward key nil t)
        (replace-match repl)))
    (goto-char (point-min))))

(add-hook 'org-export-preprocess-hook 'my-org-export-preprocess-hook)

(defvar my-org-float-tags t
  "*Float tags on right side of HTML output")

(defvar my-org-add-bullets nil)

(defadvice org-export-as-html (before my-org-export-as-html activate)
  "Hook to see if section numbers are going to be added."
  (let ((opt-plist
         (org-export-process-option-filters
          (org-combine-plists (org-default-export-plist)
                              ext-plist
                              (org-infile-export-plist)))))
    (setq my-org-add-bullets (not (plist-get opt-plist :section-numbers)))))

(defun my-org-export-html-final-hook ()
  "Export html final hook."
  (save-excursion

    (goto-char (point-min))
    (while (re-search-forward "<h[3-6].*?>" nil t)
      (insert (if my-org-float-tags "<div style='float:left'>" "")
              (if my-org-add-bullets "&bull; " "")))

    (when my-org-float-tags
      (goto-char (point-min))
      (while (re-search-forward "</h[3-6]>" nil t)
        (insert "<div style='clear:both'></div>")
        (goto-char (match-beginning 0))
        (insert "</div>")
        (forward-char 5)))

    (goto-char (point-min))
    (while (re-search-forward "<span class=\"tag\">" nil t)
      (when my-org-float-tags
        (goto-char (match-beginning 0))
        (insert "</div><div style='float:right'>")
        (re-search-forward "<span class=\"tag\">" nil t))
      (insert "[")
      (re-search-forward "</span>" nil t)
      (insert "]"))))

(add-hook 'org-export-html-final-hook 'my-org-export-html-final-hook)

(defun my-org-add-new ()
  "Add news around region if active, or add template."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (save-excursion
        (let ((beg (region-beginning)) (end (region-end)))
          (goto-char (max beg end))
          (insert "</new>")
          (goto-char (min beg end))
          (insert "<new>")))
    (insert "<new></new>")
    (backward-char 6)))

(defun my-org-remove-new ()
  "Remove the current new's."
  (interactive)
  (save-excursion
    (unless (looking-at "<new>")
      (re-search-backward "<new>"))
    (delete-char 5)
    (re-search-forward "</new>")
    (delete-char -6)))

(defun my-org-strip-all-new ()
  "Strip <new></new> from file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (item (list "<new>" "</new>"))
      (while (search-forward item nil t)
        (replace-match ""))
      (goto-char (point-min)))))

(defun my-org-new (&optional arg)
  "Add/remove/strip-all <new>."
  (interactive "P")
  (if arg
      (if (equal '(4) arg)
          (my-org-remove-new)
        (my-org-strip-all-new))
    (my-org-add-new)))

(defun my-org-link (&optional arg)
  "Store or insert link."
  (interactive "P")
  (if arg
      (call-interactively 'org-store-link)
    (call-interactively 'org-insert-link)))

(defun my-org-export-and-email ()
  "Export and email the file."
  (interactive)
  (save-excursion
    (org-export-as-html 3 nil nil "*exported-html*"))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+email_addrs: \\(.+\\)$" nil t)
      (let ((email-addrs (match-string-no-properties 1)))
        (with-current-buffer "*exported-html*"
          (goto-char (point-min))
          (insert "To: " email-addrs "\n")
          (insert "From: " (getenv "USER") "\n")
          (let ((pos (point)))
            (when (re-search-forward "<title>\\(.+\\)</title>" nil t)
              (goto-char pos)
              (insert "Subject: " (match-string-no-properties 1) "\n")))
          (insert "Content-type: text/html;\n")
          (call-process-region (point-min) (point-max) "/usr/sbin/sendmail" nil t nil email-addrs))
        (kill-buffer "*exported-html*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-mode-hook ()
  (setq org-use-sub-superscripts '{})
  (define-key org-mode-map (kbd "C-a") 'move-beginning-of-line)
  (define-key org-mode-map (kbd "C-e") 'move-end-of-line)
  (define-key org-mode-map (kbd "C-S-e") 'org-end-of-line)
  (define-key org-mode-map (kbd "C-c !") 'my-org-insert-open-time-stamp)
  (define-key org-mode-map (kbd "C-c RET") 'my-org-insert-heading)
  (define-key org-mode-map (kbd "C-c C-l") 'my-org-link)
  (define-key org-mode-map (kbd "C-c C-n") 'my-org-new)
  (define-key org-mode-map (kbd "C-c C-r") 'org-renumber-ordered-list)
  (define-key org-mode-map (kbd "C-c C-s") 'org-sort-entries-or-items)
  (define-key org-mode-map (kbd "C-c C-u") 'my-org-up-heading)
  (define-key org-mode-map (kbd "C-c C-w") 'org-cut-subtree)
  (define-key org-mode-map (kbd "C-c C-x") 'my-org-handle-checkbox)
  (define-key org-mode-map (kbd "C-c C-y") 'org-paste-subtree)
  (define-key org-mode-map (kbd "M-m") 'org-beginning-of-line)
  (font-lock-add-keywords nil '(("OPENED:" (0 'org-special-keyword t))) 'add-to-end)
  (font-lock-add-keywords nil '(("</?new>" (0 'my-org-new-face t))) 'add-to-end))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default org-export-html-style
"
<style type=\"text/css\">
body {
    margin: 1em;
    padding: 0;
    margin: 0;
    color: #000;
    font-family: \"Bitstream Vera Sans\", Verdana, sans-serif;
    font-size: 85%;
}

div#content {
    background: #fff;
    margin: 0;
    padding: 2em;
}

#table-of-contents {
    padding: .1em;
}

#table-of-contents li a,
#table-of-contents li a:hover,
#table-of-contents li a:visited,
#table-of-contents li a:hover {
    border: 0;
    color: #139;
}

h1 {
    font-family: \"BitStream Vera Sans\", Verdana;
    font-size: 180%;
    margin: -1em -1em .2em;
    padding: 0.75em 1em 0em 1em;
}

h2 {
    font-size: 150%;
    padding: .2em;
    border-bottom: 1px solid #888;
}

h3 {
    font-size: 120%;
}

.outline-text-3 {
    margin: 0em 1em;
}

h4 {
    font-size: 110%;
}

.outline-4 {
    margin: 0em 1em;
}

.outline-text-4 {
    margin: 0em 2em;
}

h2, h3, h4, h5, h6 {
    font-weight: normal;
    margin: 1em 0;
}

a {
    color: #139;
    text-decoration: none;
    padding: 1px;
    border: 1px solid #e0e0e0;
}

a:hover {
    border: 1px solid #000;
}

span.underline {
    text-decoration: underline;
}

pre {
    overflow: auto;
    border: 1pt solid #AEBDCC;
    background-color: #F3F5F7;
    padding: 5pt;
    font-family: courier, monospace;
}

code {
    border: 1px solid #ccc;
    background: #eee;
    padding: 1px;
}

table {
    border: 1px solid #777;
    font-size: 100%;
    border-collapse: collapse;
    margin: 1em 0;
}

th, td {
    border: 1px solid #777;
    padding: .3em;
    margin: 2px;
    vertical-align: top;
}

th {
    background: #eee;
}

ul { list-style-type: square; }

.todo { color: red; }
.WAITING { color: #cdcd00; }
.STARTED { color: blue; }
.done { color: green; }
.new { color: blue; }
.timestamp { color: gray }
.timestamp-kwd { color: #8b4500; }
.tag { color: blue; font-weight:normal }
.target { background-color: #551a8b; }
</style>
")

(provide 'my-org)
