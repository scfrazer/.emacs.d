;;; my-sql.el

(setq-default sqlplus-command "/usr/cisco/packages/oracle/current/bin/sqlplus")

(eval-after-load "sqlplus"
  '(progn
     (defadvice orcl-mode (after my-sql-orcl-mode activate)
       (my-font-lock-show-whitespace -1))

     (defun sqlplus-get-potential-connect-string (file-path)
       (save-excursion
         (goto-char (point-min))
         (when (looking-at "--\\s-+connect:\\s-+\\(.+\\)$")
           (match-string 1))))))

(provide 'my-sql)
