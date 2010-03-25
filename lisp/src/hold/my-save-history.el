(defvar my-save-history-varlist
  '(buffer-history
    command-history
    compile-history
    file-name-history
    find-tag-history
    function-history
    igrep-expression-history
    igrep-files-history
    list-command-history
    minibuffer-history
    query-replace-history
    read-command-history
    read-expression-history
    regexp-history
    shell-command-history
    variable-history
    regexp-search-ring
    search-ring)
  "*List of variables to save.")

(defvar my-save-history-max-length 20
  "*Max num of elements in each `my-save-history-varlist' variable to save.")
(defvar my-save-history-file "~/.emacs-history"
  "*File to save `my-save-history-varlist' variables in.")

(defun my-save-history-save ()
  "Save all histories in `my-save-history-varlist' to `my-save-history-file'"
  (interactive)
  (find-file my-save-history-file)
  (erase-buffer)
  (insert "(setq\n")
  (mapcar (lambda (x)
            (when (boundp x)
              (let ((ex (eval x)))
                (if (> (length ex) my-save-history-max-length)
                    (setcdr (nthcdr (1- my-save-history-max-length) ex) nil))
                (insert (format "  %s '%S\n" x ex)))))
          my-save-history-varlist)
  (insert ")\n")
  (save-buffer)
  (kill-buffer nil))

(defun my-save-history-load ()
  "Load histories from `my-save-history-file'"
  (interactive)
  (if (file-exists-p my-save-history-file)
      (load-file my-save-history-file)))

(add-hook 'after-init-hook 'my-save-history-load)
(add-hook 'kill-emacs-hook 'my-save-history-save)

(provide 'my-save-history)
