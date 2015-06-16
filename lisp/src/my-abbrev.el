;;; my-abbrev.el

(require 'abbrev)

(defun my-abbrev-expand-function ()
  "If after a paren, expand it.  Otherwise do default abbrev expansion."
  (let ((char (char-before)))
    (if (= (char-syntax char) ?\()
        (progn
          (insert "\n\n" (matching-paren char))
          (indent-according-to-mode)
          (forward-line -1)
          (indent-according-to-mode))
      (abbrev--default-expand))))

(setq-default abbrev-expand-function #'my-abbrev-expand-function)

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
