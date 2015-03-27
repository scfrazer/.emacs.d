;;; ur-log-mode.el

(defvar ur-log-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    table)
  "Syntax table used in ur-log-mode buffers.")

(defun ur-log-mode-commit ()
  "Save and exit."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-emacs t)))

(define-derived-mode ur-log-mode text-mode "ur-log"
  :abbrev-table nil
  :syntax-table ur-log-mode-syntax-table
  (setq comment-start "#")
  (turn-on-font-lock)
  (font-lock-fontify-buffer)
  (local-set-key (kbd "C-c C-c") 'ur-log-mode-commit))

(add-to-list 'auto-mode-alist '("\\.txt\\.edit\\.[0-9]+\\'" . ur-log-mode))

(provide 'ur-log-mode)
