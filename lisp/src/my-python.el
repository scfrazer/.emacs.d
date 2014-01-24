;;; my-python.el

(require 'my-flymake)
(require 'python)

(setq-default python-check-command "pylint_etc_wrapper.py -c"
              python-continuation-offset 4
              python-indent 4
              python-shell-interpreter (if (and (getenv "HOST") (string-match "lx30" (getenv "HOST")))
                                           "/router/bin/python-2.7.4"
                                         "/usr/bin/python"))


(defun my-flymake-python ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 'my-flymake-create-temp))
         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "pylint_etc_wrapper.py" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '(python-mode my-flymake-python))

(defun my-python-mode-hook ()
  (flymake-mode 1)
  (define-key python-mode-map (kbd "C-c !") 'python-switch-to-python)
  (define-key python-mode-map (kbd "C-c |") 'python-send-region))

(defun python-imenu-create-index ()
  "Fix declaration item and reverse function calls."
  (unless (boundp 'python-recursing)    ; dynamically bound below
    ;; Normal call from Imenu.
    (goto-char (point-min))
    ;; Without this, we can get an infloop if the buffer isn't all
    ;; fontified.  I guess this is really a bug in syntax.el.  OTOH,
    ;; _with_ this, imenu doesn't immediately work; I can't figure out
    ;; what's going on, but it must be something to do with timers in
    ;; font-lock.
    ;; This can't be right, especially not when jit-lock is not used.  --Stef
    ;; (unless (get-text-property (1- (point-max)) 'fontified)
    ;;   (font-lock-fontify-region (point-min) (point-max)))
    )
  (let (index-alist)                    ; accumulated value to return
    (while (re-search-forward
            (rx line-start (0+ space)   ; leading space
                (or (group "def") (group "class"))         ; type
                (1+ space) (group (1+ (or word ?_))))      ; name
            nil t)
      (unless (python-in-string/comment)
        (let ((pos (match-beginning 0))
              (name (match-string-no-properties 3)))
          (if (match-beginning 2)       ; def or class?
              (setq name (concat "class " name)))
          (save-restriction
            (narrow-to-defun)
            (let* ((python-recursing t)
                   (sublist (python-imenu-create-index)))
              (if sublist
                  (progn (push (cons "<Declaration>" pos) sublist)
                         (push (cons name (nreverse sublist)) index-alist))
                (push (cons name pos) index-alist)))))))
    (unless (boundp 'python-recursing)
      ;; Look for module variables.
      (let (vars)
        (goto-char (point-min))
        (while (re-search-forward
                (rx line-start (group (1+ (or word ?_))) (0+ space) "=")
                nil t)
          (unless (python-in-string/comment)
            (push (cons (match-string 1) (match-beginning 1))
                  vars)))
        (setq index-alist (nreverse index-alist))
        (if vars
            (push (cons "Module variables"
                        vars);;(nreverse vars))
                  index-alist))))
    index-alist))

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
