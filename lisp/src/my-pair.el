;;; my-pair.el

(defun my-pair-open-paren-dwim ()
  "DWIM for open parenthesis.
If on the same opener, slurp left and enter transient mode.  If
on a different opener, change to entered parens.  If after the
matching closer, barf right and enter transient mode.  Otherwise
wrap current sexp, go to beginning of sexp, and enter transient
mode ."
  (interactive "*")
  (let ((entered-char last-input-event)
        (current-char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?<)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (cond
       ;; Open paren
       ((= (char-syntax current-char) ?\()
        ;; Same opener
        (if (= entered-char current-char)
            (progn
              (my-pair-slurp-barf-open)
              (my-pair-transient-mode))
          ;; Different opener
          (save-excursion
            (forward-sexp)
            (delete-char -1)
            (insert (matching-paren entered-char)))
          (delete-char 1)
          (insert entered-char)
          (backward-char)))
       ;; Matching closer
       ((= (char-before) (matching-paren entered-char))
        (my-pair-slurp-barf-open)
        (my-pair-transient-mode))
       ;; Anything else
       (t
        (unless (looking-back "\\s-+" (point-at-bol))
          (condition-case nil
              (backward-sexp)
            ((scan-error) nil)))
        (insert entered-char)
        (forward-sexp)
        (insert (matching-paren entered-char))
        (backward-sexp)
        (my-pair-transient-mode))))))

(defun my-pair-close-paren-dwim ()
  "DWIM for close parenthesis.
If after the same closer, slurp right and enter transient mode.
If after a different closer, change to entered parens.  If on the
matching opener, barf right and enter transient mode.  Otherwise
wrap current sexp, go past end of sexp, and enter transient
mode."
  (interactive "*")
  (let ((entered-char last-input-event)
        (current-char (char-before))
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?>)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (cond
       ;; Close paren
       ((= (char-syntax current-char) ?\))
        ;; Same closer
        (if (= entered-char current-char)
            (progn
              (my-pair-slurp-barf-close)
              (my-pair-transient-mode))
          ;; Different closer
          (save-excursion
            (backward-sexp)
            (delete-char 1)
            (insert (matching-paren entered-char)))
          (delete-char -1)
          (insert entered-char)))
       ;; Matching opener
       ((= (char-after) (matching-paren entered-char))
        (my-pair-slurp-barf-close)
        (my-pair-transient-mode))
       ;; Anything else
       (t
        (unless (looking-back "\\s-+" (point-at-bol))
          (condition-case nil
              (backward-sexp)
            ((scan-error) nil)))
        (insert (matching-paren entered-char))
        (forward-sexp)
        (insert entered-char)
        (my-pair-transient-mode))))))

(defun my-pair-slurp-barf-open ()
  "Slurp or barf from using opener."
  (interactive "*")
  (let ((entered-char last-input-event)
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?<)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (cond
       ((= (char-syntax (char-after)) ?\()
        (message "TODO slurp left"))
       ((= (char-syntax (char-before)) ?\))
        (message "TODO barf right"))))))

(defun my-pair-slurp-barf-close ()
  "Slurp or barf using closer."
  (interactive "*")
  (let ((entered-char last-input-event)
        (table (copy-syntax-table (syntax-table))))
    (when (eq entered-char ?>)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (cond
       ((= (char-syntax (char-before)) ?\))
        (message "TODO slurp right"))
       ((= (char-syntax (char-after)) ?\()
        (message "TODO barf left"))))))

(defvar my-pair-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "(") 'my-pair-slurp-barf-open)
    (define-key map (kbd ")") 'my-pair-slurp-barf-close)
    (define-key map (kbd "[") 'my-pair-slurp-barf-open)
    (define-key map (kbd "]") 'my-pair-slurp-barf-close)
    (define-key map (kbd "{") 'my-pair-slurp-barf-open)
    (define-key map (kbd "}") 'my-pair-slurp-barf-close)
    (define-key map (kbd "<") 'my-pair-slurp-barf-open)
    (define-key map (kbd ">") 'my-pair-slurp-barf-close)
    map)
  "Keymap used in slurping/barfing transient mode.")

(defun my-pair-transient-mode ()
  "Start slurping/barfing transient mode."
  (set-transient-map my-pair-transient-map t))

(defun my-pair-quotes-dwim ()
  "DWIM for quotes.
If looking at different quotes, change to the entered quotes."
  (interactive "*")
  (let ((entered-char last-input-event)
        (current-char (char-after)))
    (when (= (char-syntax current-char) ?\")
      (save-excursion
        (forward-sexp)
        (delete-char -1)
        (insert entered-char))
      (delete-char 1)
      (insert entered-char)
      (backward-char))))

(defun my-pair-delete-forward ()
  "Forward delete paired chars."
  (interactive "*")
  (let ((char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (when (eq char ?<)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"`" table))
    (with-syntax-table table
      (save-excursion
        (forward-sexp)
        (delete-char -1))
      (delete-char 1))))

(defun my-pair-delete-backward ()
  "Backward delete paired chars."
  (interactive "*")
  (let ((char (char-before))
        (table (copy-syntax-table (syntax-table))))
    (when (eq char ?>)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"`" table))
    (with-syntax-table table
      (save-excursion
        (backward-sexp)
        (delete-char 1))
      (delete-char -1))))

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
