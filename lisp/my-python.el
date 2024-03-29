;;; my-python.el  -*- lexical-binding: t -*-

(require 'my-flymake)
(require 'python)

(setq-default python-continuation-offset 4
              python-indent 4
              python-flymake-command '("flymake_python3w")
              python-flymake-command-output-pattern (list "^[^:]+:\\([0-9]+\\): \\(WARNING\\|ERROR\\): \\(.+\\)$" 1 nil 2 3)
              python-flymake-msg-alist '(("WARNING" . :warning) ("ERROR" . :error))
              python-shell-interpreter "python3w")

(defun my-python-flymake (_ report-fn &rest _args)
  (unless (executable-find (car python-flymake-command))
    (error "Cannot find a suitable checker"))
  (when (process-live-p python--flymake-proc)
    (kill-process python--flymake-proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq python--flymake-proc
            (make-process
             :name "python-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer "*flymake-python*")
             :command (append python-flymake-command (list (buffer-file-name)))
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc python--flymake-proc))
                       (python--flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc))))))))))
(advice-add 'python-flymake :around #'my-python-flymake)

(defun my-python-tidy ()
  "Run formatter on buffer."
  (interactive "*")
  (save-buffer)
  (let ((line-num (line-number-at-pos)))
    (shell-command (concat "black " (buffer-file-name)))
    (revert-buffer t t)
    (goto-line line-num)))

(defun my-python-indent-shift-left (start end)
  (interactive "r")
  (deactivate-mark t)
  (python-indent-shift-left start end))

(defun my-python-indent-shift-right (start end)
  (interactive "r")
  (deactivate-mark t)
  (python-indent-shift-right start end))

(defun my-python-mode-hook ()
  (flymake-mode 1)
  (bind-keys :map python-mode-map
             ("C-c !" . python-switch-to-python)
             ("C-c <" . my-python-indent-shift-left)
             ("C-c >" . my-python-indent-shift-right)
             ("C-c |" . python-send-region))
  (setq forward-sexp-function nil))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(define-abbrev python-mode-abbrev-table
  "sup"
  "super"
  'my-python-insert-super-call)

(define-abbrev python-mode-abbrev-table
  "lint"
  "# pylint: disable=")

(defun my-python-insert-super-call ()
  (interactive)
  (let (fn args class)
    (save-excursion
      (while (python-nav-beginning-of-block)
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

(provide 'my-python)
