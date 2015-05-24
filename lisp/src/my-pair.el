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

(defun corral-wrap-backward (open close)
  "Wrap OPEN and CLOSE delimiters around sexp, leaving point at OPEN."
  ;; If char before point is whitespace, move to end of sexp first
  (when (string-match-p "\\S-" (char-to-string (char-before)))
    (beginning-of-sexp))
  (insert open)
  (save-excursion
    (forward-sexp) (insert close))
  (backward-char))

(defun corral-wrap-forward (open close)
  "Wrap OPEN and CLOSE around sexp, leaving point at CLOSE."
  ;; If char before point is a word, move to beginning of sexp first
  (when (string-match-p "\\S-" (char-to-string (char-before)))
    (beginning-of-sexp))
  (insert open)
  (forward-sexp)
  (insert close))

(defun corral-shift-backward (open close)
  "Shift OPEN delimiter backward one sexp.  CLOSE is not moved."
  (cond
   ((eq (char-before) open)
    (backward-char) (corral-shift-backward open close))
   ((eq (char-after) open)
    (delete-char 1) (backward-sexp) (insert open) (backward-char))
   (t (backward-sexp) (forward-char) (corral-shift-backward open close))))

(defun corral-shift-forward (open close)
  "Without moving OPEN, shift CLOSE delimiter forward one sexp."
  (cond
   ((eq (char-after) close)
    (forward-char) (corral-shift-forward open close))
   ((eq (char-before) close)
    (delete-char -1) (forward-sexp) (insert close))
   (t (forward-sexp) (corral-shift-forward open close))))

(defun corral-parentheses-forward ()
  "Wrap parentheses around sexp, moving point to the closing parentheses."
  (interactive)
  (if (or (eq last-command 'corral-parentheses-forward)
          (eq last-command 'corral-parentheses-backward))
      (corral-shift-forward ?( ?))
    (corral-wrap-forward ?( ?))))

(defun corral-parentheses-backward ()
  "Wrap parentheses around sexp, moving point to the opening parentheses."
  (interactive)
  (if (or (eq last-command 'corral-parentheses-forward)
          (eq last-command 'corral-parentheses-backward))
      (corral-shift-backward ?( ?))
    (corral-wrap-backward ?( ?))))

(provide 'my-pair)
