;;; my-pair.el

(defun my-pair-delete-forward ()
  "Forward delete paired chars."
  (interactive)
  (let ((char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"`" table))
    (save-excursion
      (forward-sexp)
      (delete-char -1))
    (delete-char 1)))

(defun my-pair-delete-backward ()
  "Backward delete paired chars."
  (interactive)
  (let ((char (char-before))
        (table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"`" table))
    (save-excursion
      (backward-sexp)
      (delete-char 1))
    (delete-char -1)))

(provide 'my-pair)
