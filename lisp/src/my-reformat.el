;;; my-reformat.el

(defun my-reformat-comma-delimited-items ()
  "Put all comma-delimited items on one line, or each on its own line."
  (interactive)
  (my-reformat-delimited-items ","))

(defun my-reformat-semi-delimited-items ()
  "Put all semi-delimited items on one line, or each on its own line."
  (interactive)
  (my-reformat-delimited-items ";"))

(defun my-reformat-delimited-items (str)
  "Put all STR-delimited items on one line, or each on its own line."
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
          (insert (replace-regexp-in-string "[ \t]+" " " item-string))
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
      (setq collect (not (= start-line (line-number-at-pos end))))
      (delete-region start end)
      (dolist (item items)
        (insert item str)
        (if collect
            (insert " ")
          (indent-according-to-mode)
          (insert "\n")))
      (delete-char -2))))

(provide 'my-reformat)
