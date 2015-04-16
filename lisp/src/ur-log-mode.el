;;; ur-log-mode.el

(defvar ur-log-mode-font-lock-keywords
  '(
    ("#.+$"
     (0 'font-lock-comment-face))
    ("^------.+$"
     (0 'font-lock-warning-face))
    )
  "Font locking for 'ur-log-mode'.")

(defvar ur-log-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table used in ur-log-mode buffers.")

(defun ur-log-mode-insert-first-comment ()
  "Insert comment from first file."
  (interactive)
  (let (beg end comment (comments ""))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "---------- Only add comments above this line. All other edits will be lost. ----------" nil t)
        (while (re-search-forward "^\\s-+/vob/sse" nil t)
          (forward-line 1)
          (setq beg (point))
          (while (and (not (eobp)) (not (looking-at "^\\s-*$")))
            (forward-line 1))
          (setq comment (buffer-substring-no-properties beg (point)))
          (unless (string-match (regexp-quote comment) comments)
            (setq comments (concat comments comment))))))
    (when comments
      (delete-blank-lines)
      (insert (replace-regexp-in-string "^\\s-*" "" comments)))))

(defun ur-log-mode-commit ()
  "Save and exit."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-emacs t)))

(define-derived-mode ur-log-mode text-mode "ur-log"
  :abbrev-table nil
  :syntax-table ur-log-mode-syntax-table
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "#"
        comment-end "")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ur-log-mode-font-lock-keywords t))
  (turn-on-font-lock)
  (font-lock-fontify-buffer)
  (local-set-key (kbd "C-c C-l") 'ur-log-mode-insert-first-comment)
  (local-set-key (kbd "C-c C-c") 'ur-log-mode-commit))

(add-to-list 'auto-mode-alist '("\\.txt\\.edit\\.[0-9]+\\'" . ur-log-mode))

(provide 'ur-log-mode)
