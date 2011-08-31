;;; my-pair.el

(defun my-pair-insert (&optional arg)
  "Intelligently insert paired chars."
  (interactive "P")
  (let ((char last-input-event)
        (beg (point))
        end
        lisp-regexp-paren)
    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (1+ (region-end)))
      (deactivate-mark))
    (goto-char beg)
    (setq lisp-regexp-paren
          (and (or (eq major-mode 'emacs-lisp-mode)
                   (eq major-mode 'lisp-interaction-mode))
               (eq char ?\()
               (looking-back "\\\\\\\\" (point-at-bol))))
    (insert char)
    (unless (or arg (and (eq (char-before beg) ?\\)
                         (not lisp-regexp-paren)))
      (when end
        (goto-char end))
      (cond
       ;; Open paren
       ((eq char ?\()
        (if lisp-regexp-paren
            (insert "\\\\)")
          (insert ?\)))
        (unless end
          (if lisp-regexp-paren
              (backward-char 3)
            (backward-char))))
       ;; Open bracket
       ((eq char ?\[)
        (insert ?\])
        (unless end
          (backward-char)))
       ;; Open curly
       ((eq char ?\{)
        (insert ?\})
        (unless end
          (backward-char)))
       ;; Double-quote
       ((eq char ?\")
        (insert ?\")
        (unless end
          (backward-char)))
       ;; Single-quote
       ((eq char ?\')
        (when (eq major-mode 'python-mode)
          (let (ppss)
            (save-excursion
              (setq ppss (syntax-ppss beg)))
            (unless (or (nth 4 ppss) (nth 3 ppss))
              (insert ?\')
              (unless end
                (backward-char))))))
       ;; Backtick
       ((eq char ?`)
        (if (and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                 (nth 3 (syntax-ppss)))
            (insert ?\')
          (when (memq major-mode '(cperl-mode csh-mode sh-mode))
            (insert ?\`)
            (unless end
              (backward-char)))))))))

(defun my-pair-delete-forward ()
  "Forward delete paired chars."
  (interactive)
  (let ((char (char-after)))
    (cond ((memq char '(?\( ?\[ ?\{ ?\" ?\'))
           (save-excursion
             (forward-sexp)
             (delete-char -1))
           (delete-char 1))
          ((eq char ?\`)
           (save-excursion
             (when (search-forward "`" nil t)
               (delete-char -1)))
           (delete-char 1)))))

(defun my-pair-delete-backward ()
  "Backward delete paired chars."
  (interactive)
  (let ((char (char-before)))
    (cond ((memq char '(?\) ?\] ?\ ?\" ?\'))
           (save-excursion
             (backward-sexp)
             (delete-char 1))
           (delete-char -1))
          ((eq char ?\`)
           (save-excursion
             (when (search-backward "`" nil t)
               (delete-char 1)))
           (delete-char -1)))))

(provide 'my-pair)
