;;; my-sql.el

(setq-default sqlplus-command "/usr/cisco/packages/oracle/current/bin/sqlplus"
              sqlplus-multi-output-tables-default-flag nil)

(require 'sql-indent)
(require 'sqlup-mode)

(defun my-sql-connect ()
  "Connect through sqlplus."
  (interactive)
  (let ((connect-string (save-excursion
                          (goto-char (point-min))
                          (when (looking-at "--\\s-+connect:\\s-+\\(.+\\)$")
                            (match-string-no-properties 1)))))
    (when connect-string
      (sqlplus connect-string (current-buffer)))))

(defun my-sqlplus-mode-hook ()
  (setq comment-start "-- "
        comment-end "")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sql-indent-line)
  (define-key sqlplus-mode-map (kbd "C-c C-c") 'my-sql-connect)
  (sqlup-mode 1))

(add-hook 'sqlplus-mode-hook 'my-sqlplus-mode-hook)

(provide 'my-sql)
