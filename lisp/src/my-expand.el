;;; my-expand.el

(setq-default yas/dont-activate t
              yas/indent-line nil
              yas/fallback-behavior 'return-nil)

(require 'yasnippet)

(yas/initialize)

(defun my-yasnippet-exit-current ()
  "Exit current snippet"
  (interactive)
  (yas/exit-snippet (car (yas/snippets-at-point))))

(define-key yas/keymap (kbd "<return>") 'my-yasnippet-exit-current)

(defun my-expand-yasnippet-or-abbrev ()
  "Try to expand yasnippet, then expand abbrev if it fails."
  (interactive)
  (let (expanded)
    (yas/minor-mode 1)
    (setq expanded (yas/expand))
    (yas/minor-mode -1)
    (unless expanded
      (expand-abbrev))))

(defun my-yas-indent-line ()
  "Indent but don't insert anything into the text"
  (indent-according-to-mode)
  "")

(defun my-yas-e-mode-get-enclosing-struct ()
  "Get the enclosing struct/unit in e-mode."
  (save-excursion
    (backward-up-list)
    (re-search-backward "\\(^\\|[ \t]\\)\\([^ \t]+\\)")
    (match-string-no-properties 2)))

(require 'abbrev)

(define-abbrev global-abbrev-table
  "filename"
  ""
  (lambda() (insert (or buffer-file-name "*NOFILE*"))))

(define-abbrev global-abbrev-table
  "file"
  ""
  (lambda() (insert (if buffer-file-name (file-name-nondirectory buffer-file-name) "*NOFILE*"))))

(define-abbrev global-abbrev-table
  "td"
  ""
  (lambda() (insert comment-start "TODO")))

(define-abbrev global-abbrev-table
  "fix"
  ""
  (lambda() (insert comment-start "FIXME")))

(defadvice expand-abbrev (around my-expand-abbrev-advice activate)
  (if (and (member major-mode (list 'c++-mode 'c-mode 'cperl-mode))
           (looking-back "{"))
      (progn
        (insert "\n\n}")
        (indent-according-to-mode)
        (forward-line -1)
        (indent-according-to-mode))
    ad-do-it))

(provide 'my-expand)
