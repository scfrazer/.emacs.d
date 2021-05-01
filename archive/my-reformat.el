;;; my-reformat.el

(defun my-reformat-comma-delimited-items (&optional end-col)
  "Put all comma-delimited items on one line, or each on its own line."
  (interactive "P")
  (my-reformat-delimited-items "," end-col))

(defun my-reformat-semi-delimited-items (&optional end-col)
  "Put all semi-delimited items on one line, or each on its own line."
  (interactive "P")
  (my-reformat-delimited-items ";" end-col))

(defun my-reformat-delimited-items (str end-col)
  "Put all STR-delimited items on one line, or each on its own line."
  (interactive "sDelimiting string? \nnEnding column? ")
  (save-excursion
    (backward-up-list)
    (let ((start (1+ (point))) end
          (start-line (line-number-at-pos))
          (items '()) collect)
      (forward-sexp)
      (setq end (1- (point)))
      ;; Parse items
      (let ((item-string
             (replace-regexp-in-string "\n" " " (buffer-substring-no-properties start end)))
            (item-strings '()) pos)
        (with-temp-buffer
          (insert item-string)
          (goto-char (point-min))
          ;; Trim inner whitespace, unless it's in a string
          (while (re-search-forward "[ \t]\\{2,\\}" nil t)
            (unless (nth 3 (syntax-ppss))
              (replace-match " ")))
          ;; Split items up
          (goto-char (point-min))
          (setq pos (point))
          (while (not (eobp))
            (forward-sexp)
            (when (looking-at (concat "\\s-*" str))
              (push (buffer-substring-no-properties pos (point)) item-strings)
              (search-forward str)
              (setq pos (point))))
          (push (buffer-substring-no-properties pos (point)) item-strings))
        (dolist (str item-strings)
          (let ((trimmed-str (and str (replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" str))))
            (when trimmed-str
              (push trimmed-str items)))))
      ;; Collect or disperse items
      (goto-char start)
      (if end-col
          (progn
            (delete-region start end)
            (unless (string-match "\\s-+$" str)
              (setq str (concat str " ")))
            (when items
              (insert (car items))
              (setq items (cdr items)))
            (dolist (item items)
              (if (<= (+ (current-column) (length str) (length item)) end-col)
                  (insert str item)
                (insert str)
                (skip-syntax-backward " ")
                (delete-region (point) (line-end-position))
                (insert "\n" item)
                (indent-according-to-mode))))
        (setq collect (not (= start-line (line-number-at-pos end))))
        (delete-region start end)
        (dolist (item items)
          (insert item str)
          (if collect
              (insert " ")
            (indent-according-to-mode)
            (insert "\n")))
        (delete-char -1)
        (delete-char (* -1 (length str)))))))

(provide 'my-reformat)
