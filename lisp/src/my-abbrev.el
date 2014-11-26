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

(provide 'my-abbrev)
