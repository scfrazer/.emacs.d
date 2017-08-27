;;; my-pair.el

(defun my-pair-open-paren-dwim (&optional arg)
  "DWIM for open parenthesis.
If on the same opener, slurp left and enter slurping transient
mode.  If on a different opener, change to entered parens.
Otherwise (or if a prefix arg has been given) wrap current sexp,
go to beginning of sexp, and enter slurping transient mode."
  (interactive "*P")
  (let ((entered-char last-input-event)
        (current-char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?<)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      ;; Open paren
      (if (and (not arg) (= (char-syntax current-char) ?\())
          ;; Same opener
          (if (= entered-char current-char)
              (progn
                (my-pair-slurp-open)
                (my-pair-transient-mode))
            ;; Different opener
            (save-excursion
              (forward-sexp)
              (delete-char -1)
              (insert (matching-paren entered-char)))
            (delete-char 1)
            (insert entered-char)
            (backward-char))
        ;; Anything else
        (unless (or (bolp) (looking-back "\\s-+" (point-at-bol)))
          (condition-case nil
              (backward-sexp)
            ((scan-error) nil)))
        (insert entered-char)
        (forward-sexp)
        (insert (matching-paren entered-char))
        (backward-sexp)
        (my-pair-transient-mode)))))

(defun my-pair-close-paren-dwim (&optional arg)
  "DWIM for close parenthesis.
If after the same closer, slurp right and enter slurping
transient mode.  If after a different closer, change to entered
parens.  Otherwise (or if a prefix arg has been given) wrap
current sexp, go past end of sexp, and enter slurping transient
mode."
  (interactive "*P")
  (let ((entered-char last-input-event)
        (current-char (char-before))
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?>)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      ;; Close paren
      (if (and (not arg) (= (char-syntax current-char) ?\)))
          ;; Same closer
          (if (= entered-char current-char)
              (progn
                (my-pair-slurp-close)
                (my-pair-transient-mode))
            ;; Different closer
            (save-excursion
              (backward-sexp)
              (delete-char 1)
              (insert (matching-paren entered-char)))
            (delete-char -1)
            (insert entered-char))
        ;; Anything else
        (unless (or (bolp) (looking-back "\\s-+" (point-at-bol)))
          (condition-case nil
              (backward-sexp)
            ((scan-error) nil)))
        (insert (matching-paren entered-char))
        (forward-sexp)
        (insert entered-char)
        (my-pair-transient-mode)))))

(defun my-pair-slurp-open ()
  "Slurp at opener."
  (interactive "*")
  (let ((entered-char last-input-event)
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?<)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (if (= (char-syntax (char-after)) ?\()
          (my-pair-slurp-open-1)
        (when (= (char-syntax (char-before)) ?\))
          (save-excursion
            (backward-sexp)
            (my-pair-slurp-open-1)))))))

(defun my-pair-slurp-open-1 ()
  "Do the actual slurp open work."
  (let ((char (char-after)))
    (save-excursion
      (backward-sexp)
      (insert char))
    (delete-char 1)
    (backward-up-list)))

(defun my-pair-slurp-close ()
  "Slurp at closer."
  (interactive "*")
  (let ((entered-char last-input-event)
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?>)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (if (= (char-syntax (char-before)) ?\))
          (my-pair-slurp-close-1)
        (when (= (char-syntax (char-after)) ?\()
          (save-excursion
            (forward-sexp)
            (my-pair-slurp-close-1)))))))

(defun my-pair-slurp-close-1 ()
  "Do the actual slurp close work."
  (let ((char (char-before)))
    (save-excursion
      (forward-sexp)
      (insert char))
    (delete-char -1)
    (up-list)))

(defvar my-pair-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "(") 'my-pair-slurp-open)
    (define-key map (kbd ")") 'my-pair-slurp-close)
    (define-key map (kbd "[") 'my-pair-slurp-open)
    (define-key map (kbd "]") 'my-pair-slurp-close)
    (define-key map (kbd "{") 'my-pair-slurp-open)
    (define-key map (kbd "}") 'my-pair-slurp-close)
    (define-key map (kbd "<") 'my-pair-slurp-open)
    (define-key map (kbd ">") 'my-pair-slurp-close)
    map)
  "Keymap used in slurping transient mode.")

(defun my-pair-transient-mode ()
  "Start slurping transient mode."
  (set-transient-map my-pair-transient-map t))

(defun my-pair-quotes-dwim ()
  "DWIM for quotes.
If looking at different quotes, change to the entered quotes."
  (interactive "*")
  (let ((entered-char last-input-event)
        (current-char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (with-syntax-table table
      (when (= (char-syntax current-char) ?\")
        (save-excursion
          (forward-sexp)
          (delete-char -1)
          (insert entered-char))
        (delete-char 1)
        (insert entered-char)
        (backward-char)))))

(defun my-pair-delete ()
  "Delete paired chars."
  (interactive "*")
  (let* ((table (copy-syntax-table (syntax-table)))
        (char (char-before))
        (backward (member char '(?> ?\) ?\] ?\} ?\` ?\' ?\"))))
    (unless backward
      (setq char (char-after)))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"" table))
    (when (eq char ?\')
      (modify-syntax-entry ?\' "\"" table))
    (when (eq char ?\")
      (modify-syntax-entry ?\" "\"" table))
    (with-syntax-table table
      (if backward
          (progn
            (save-excursion
              (backward-sexp)
              (delete-char 1))
            (delete-char -1))
        (save-excursion
          (forward-sexp)
          (delete-char -1))
        (delete-char 1)))))

(defun my-pair-close-all ()
  "Close all open parens."
  (interactive "*")
  (let (closers)
    (save-excursion
      (while
          (condition-case nil
              (progn
                (backward-up-list)
                (push (matching-paren (char-after)) closers))
            ((scan-error) nil))))
    (apply #'insert (nreverse closers))))

(defun my-pair-step-out-backward ()
  "Step backward out of current list or string."
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (cond
     ((nth 3 pps)
      (goto-char (nth 8 pps)))
     ((nth 1 pps)
      (goto-char (nth 1 pps))))))

(defun my-pair-step-out-forward ()
  "Step forward out of current list or string."
  (interactive)
  (my-pair-step-out-backward)
  (forward-sexp))

(provide 'my-pair)
