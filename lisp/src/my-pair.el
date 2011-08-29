;;; my-pair.el

(defun my-pair-insert (&optional arg)
  "Intelligently insert paired chars."
  (interactive "P")
  (let ((char last-input-event)
        (beg (point))
        end)
    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (1+ (region-end)))
      (deactivate-mark))
    (goto-char beg)
    (insert char)
    (unless arg
      (when end
        (goto-char end))
      (cond
       ;; Open paren
       ((= char ?\()
        (let (regexp-paren)
          (when (eq major-mode 'emacs-lisp-mode)
            (save-excursion
              (goto-char beg)
              (setq regexp-paren (looking-back "\\\\" (point-at-bol)))))
          (if regexp-paren
              (insert "\\\\)")
            (insert ?\)))
          (unless end
            (if regexp-paren
                (backward-char 3)
              (backward-char)))))
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
        (if (and (eq major-mode 'emacs-lisp-mode)
                 (nth 3 (syntax-ppss)))
            (insert ?')
          (insert ?`))
        (unless end
          (backward-char)))))))

(provide 'my-pair)
