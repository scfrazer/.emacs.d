;;; my-python.el

(require 'my-flymake)
(require 'python)

(setq-default python-check-command "pylint_etc_wrapper.py -c"
              python-continuation-offset 4
              python-indent 4)

(defun my-flymake-python ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 'my-flymake-create-temp))
         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "pylint_etc_wrapper.py" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '(python-mode my-flymake-python))

(defun my-python-mode-hook ()
  (flymake-mode 1))

(define-abbrev python-mode-abbrev-table
  "sup"
  "super"
  'my-python-insert-super-call)

(defun my-python-insert-super-call ()
  (interactive)
  (let (fn args class)
    (save-excursion
      (while (python-beginning-of-block)
        (when (looking-at "\\s-*def\\s-+\\([a-zA-Z0-9_]+\\)")
          (setq fn (match-string-no-properties 1))
          (save-excursion
            (setq args nil)
            (search-forward "(")
            (let ((beg (point))
                  (end (progn (backward-char) (forward-sexp) (1- (point))))
                  arg)
              (goto-char beg)
              (while (and (< (point) end)
                          (re-search-forward "\\s-*\\([a-zA-Z0-9_]+\\)" end t))
                (setq arg (match-string-no-properties 1))
                (unless (string= arg "self")
                  (push arg args))
                (if (looking-at "\\s-*,")
                    (goto-char (match-end 0))
                  (unless (looking-at "\\s-*)")
                    (forward-sexp))))))))
      (when (looking-at "\\s-*class\\s-+\\([a-zA-Z0-9_]+\\)")
        (setq class (match-string-no-properties 1))))
    (when (and fn class)
      (insert "(" class ", self)." fn "(")
      (when args
        (nreverse args)
        (dolist (arg args)
          (insert arg ", "))
        (delete-char -2))
      (insert ")"))))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(provide 'my-python)
