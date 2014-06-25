;;; my-abbrev.el

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
  "date"
  ""
  (lambda() (insert (format-time-string "%m/%d/%Y"))))

(define-abbrev global-abbrev-table
  "td"
  ""
  (lambda() (insert "TODO")))

(define-abbrev global-abbrev-table
  "fix"
  ""
  (lambda() (insert "FIXME")))

(define-abbrev global-abbrev-table
  "db"
  ""
  (lambda() (insert "DEBUG")))

(defadvice expand-abbrev (around my-expand-abbrev-advice activate)
  (if (looking-back "{")
      (progn
        (insert "\n\n}")
        (indent-according-to-mode)
        (forward-line -1)
        (indent-according-to-mode))
    ad-do-it))

(provide 'my-abbrev)
