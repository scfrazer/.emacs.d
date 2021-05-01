;;; my-markdown.el  -*- lexical-binding: t; -*-

(setq-default markdown-fontify-code-blocks-natively t
              markdown-hide-markup nil
              markdown-list-item-bullets '("•" "◦" "▪" "▫")
              markdown-unordered-list-item-prefix "* ")

;; Task states
(defconst my-markdown-task-keywords
  '(("TODO"       . (0 '(:foreground "red3" :weight bold)))
    ("STARTED"    . (0 '(:foreground "blue4" :weight bold)))
    ("WAITING"    . (0 '(:foreground "darkorange3" :weight bold)))
    ("DONE"       . (0 '(:foreground "green4" :weight bold)))
    ("MAYBE"      . (0 '(:inherit font-lock-doc-face)))
    ("SOMEDAY"    . (0 '(:inherit font-lock-doc-face)))
    ("CANCELED"   . (0 '(:inherit font-lock-comment-face)))
    ("REASSIGNED" . (0 '(:inherit font-lock-comment-face)))))
(font-lock-add-keywords 'gfm-mode my-markdown-task-keywords)
(font-lock-add-keywords 'markdown-mode my-markdown-task-keywords)

(defconst my-markdown-task-states (mapcar #'car my-markdown-task-keywords))
(defconst my-markdown-task-re (concat ".*\\(" (string-join my-markdown-task-states "\\|") "\\)"))
(defun my-markdown-set-task-state ()
  "Set the current task state."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward my-markdown-task-re (point-at-eol) t)
    (backward-word)
    (when-let ((state (completing-read "Set task state:" my-markdown-task-states)))
      (kill-word 1)
      (insert state)
      (backward-word))))

;; Have markdown-do learn about task states
(defun my-markdown-do (orig-fun)
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at my-markdown-task-re))
      (my-markdown-set-task-state)
    (apply orig-fun '())))
(advice-add 'markdown-do :around #'my-markdown-do)

;; Lint

(defvar markdown-flymake-command "markdown-lint")
(defvar-local markdown--flymake-proc nil)

(defun my-markdown-flymake (report-fn &rest _args)
  (unless (executable-find markdown-flymake-command)
    (error "Cannot find a suitable checker"))
  (when (process-live-p markdown--flymake-proc)
    (kill-process markdown--flymake-proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq markdown--flymake-proc
            (make-process
             :name "markdown-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *markdown-flymake*")
             :command (list markdown-flymake-command (buffer-file-name))
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc markdown--flymake-proc))
                       (my-markdown-flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc))))))))))

(defun my-markdown-flymake-parse-output (source proc report-fn)
  "Parse output from checker."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp
            "^[^:]+:\\([0-9]+\\): \\(MD[0-9]+ .+\\)$"
            nil t)
     for msg = (match-string 2)
     for (beg . end) = (flymake-diag-region
                        source
                        (string-to-number (match-string 1)))
     collect (flymake-make-diagnostic source
                                      beg
                                      end
                                      :warning
                                      msg)
     into diags
     finally (funcall report-fn diags))))

;; Headers

(defconst markdown-regex-header
  "^\\(?:\\([^\r\n\t -].*\\)\n\\(?:\\(=+\\)\\|\\(-+\\)\\)\\|\\(#+[ \t]*\\)\\(.*?\\)\\([ \t]*#*\\)\\)$"
  "Allow space after #")

(defconst markdown-regex-header-atx
  "^\\(#+\\)[ \t]*\\(.*?\\)[ \t]*\\(#*\\)$"
  "Allow space after #")

;; Always show header markup
(defun my-markdown-fontify-headings (orig-fun last)
  (let ((markdown-hide-markup nil))
    (apply orig-fun (list last))))
(advice-add 'markdown-fontify-headings :around #'my-markdown-fontify-headings)

;; Mode hook

(defun my-markdown-mode-hook ()
  (my-word-wrap-on-hook)
  (add-hook 'flymake-diagnostic-functions 'my-markdown-flymake nil t)
  (flymake-mode 1))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; TODO Keybinds ... maybe promote/demote/move/etc. as hydra?

(provide 'my-markdown)
