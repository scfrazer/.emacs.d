;;; my-sql.el

(setq-default sqlplus-command "/usr/cisco/packages/oracle/current/bin/sqlplus"
              sqlplus-multi-output-tables-default-flag nil)

(eval-after-load "sqlplus"
  '(progn
     (require 'sql-indent)
     (require 'sqlup-mode)

     (defun sqlplus-get-potential-connect-string (file-path)
       (save-excursion
         (goto-char (point-min))
         (when (looking-at "--\\s-+connect:\\s-+\\(.+\\)$")
           (match-string 1))))

     (defun my-sqlplus-mode-hook ()
       (make-local-variable 'indent-line-function)
       (setq indent-line-function 'sql-indent-line)
       (sqlup-mode 1))

     (add-hook 'sqlplus-mode-hook 'my-sqlplus-mode-hook)))

(provide 'my-sql)
