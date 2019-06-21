;;; rel-log-mode.el

(defvar rel-log-mode-font-lock-keywords
  '(
    ("^File .+$"
     (0 'font-lock-keyword-face))
    ("^\\s-+//.+$"
     (0 'font-lock-variable-name-face))
    ("^------.+$"
     (0 'font-lock-comment-face))
    )
  "Font locking for 'rel-log-mode'.")

(defun rel-log-mode-insert-first-comment ()
  "Insert comment from first file."
  (interactive)
  (let (beg end comment (comments ""))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "---------- Only add comments above this line. All other edits will be lost. ----------" nil t)
        (while (re-search-forward "^\\s-+//" nil t)
          (forward-line 1)
          (setq beg (point))
          (while (and (not (eobp)) (not (looking-at "^\\s-*$\\|^\\s-+//")))
            (forward-line 1))
          (setq comment (buffer-substring-no-properties beg (point)))
          (unless (string-match (regexp-quote comment) comments)
            (setq comments (concat comments comment))))))
    (when comments
      (delete-blank-lines)
      (insert (replace-regexp-in-string "^\\s-*" "" comments)))))

(defun rel-log-mode-commit ()
  "Save and exit."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-emacs t)))

(define-derived-mode rel-log-mode text-mode "rel-log"
  :abbrev-table nil
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "#"
        comment-end "")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rel-log-mode-font-lock-keywords t))
  (turn-on-font-lock)
  (font-lock-ensure)
  (local-set-key (kbd "C-c C-l") 'rel-log-mode-insert-first-comment)
  (local-set-key (kbd "C-c C-c") 'rel-log-mode-commit))

(add-to-list 'auto-mode-alist '("history\\.edit\\.[0-9]+\\'" . rel-log-mode))

(provide 'rel-log-mode)
