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
               (= char ?\()
               (looking-back "\\\\\\\\" (point-at-bol))))
    (insert char)
    (unless (or arg (and (= (char-before beg) ?\\)
                         (not lisp-regexp-paren)))
      (when end
        (goto-char end))
      (cond
       ;; Open paren
       ((= char ?\()
        (if lisp-regexp-paren
            (insert "\\\\)")
          (insert ?\)))
        (unless end
          (if lisp-regexp-paren
              (backward-char 3)
            (backward-char))))
       ;; Open bracket
       ((= char ?\[)
        (insert ?\])
        (unless end
          (backward-char)))
       ;; Open curly
       ((= char ?\{)
        (insert ?\})
        (unless end
          (backward-char)))
       ;; Double-quote
       ((= char ?\")
        (insert ?\")
        (unless end
          (backward-char)))
       ;; Single-quote
       ((= char ?')
        (when (eq major-mode 'python-mode)
          (let (ppss)
            (save-excursion
              (setq ppss (syntax-ppss beg)))
            (unless (or (nth 4 ppss) (nth 3 ppss))
              (insert ?')
              (unless end
                (backward-char))))))
       ;; Backtick
       ((= char ?`)
        (if (and (or (eq major-mode 'emacs-lisp-mode)
                     (eq major-mode 'lisp-interaction-mode))
                 (nth 3 (syntax-ppss)))
            (insert ?')
          (insert ?`))
        (unless end
          (backward-char)))))))

(defun my-pair-delete ()
  "Intelligently delete paired chars."
  (interactive)
  ;; TODO
  )

(provide 'my-pair)
