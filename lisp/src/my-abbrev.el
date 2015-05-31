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

(defadvice expand-abbrev (around my-expand-abbrev-advice activate)
  (let ((char (char-before)))
    (if (= (char-syntax char) ?\()
        (progn
          (insert "\n\n" (matching-paren char))
          (indent-according-to-mode)
          (forward-line -1)
          (indent-according-to-mode))
      ad-do-it)))

(provide 'my-abbrev)
