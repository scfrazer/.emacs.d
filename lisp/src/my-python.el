;;; my-python.el  -*- lexical-binding: t -*-

(require 'my-flymake)
(require 'python)

(setq-default python-continuation-offset 4
              python-indent 4
              python-flymake-command '("flymake_python")
              python-flymake-command-output-pattern (list "^[^:]+:\\([0-9]+\\): \\(WARNING\\|ERROR\\): \\(.+\\)$" 1 nil 2 3)
              python-flymake-msg-alist '(("WARNING" . :warning) ("ERROR" . :error))
              python-shell-interpreter (if (and (getenv "HOST") (string-match "asic-vm" (getenv "HOST")))
                                           "/router/bin/python3-3.5.0"
                                         "/usr/bin/python"))

;; (defun my-flymake-python ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'my-flymake-create-temp))
;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
;;     (list "pylint_etc_wrapper.py" (list local-file))))
;;
;; (add-to-list 'flymake-allowed-file-name-masks '(python-mode my-flymake-python))

(defun my-python-flymake (orig-fun report-fn &rest _args)
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
             :buffer (generate-new-buffer " *python-flymake*")
             :command (list (car python-flymake-command) (buffer-file-name))
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc python--flymake-proc))
                       (python--flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc))))))))))
(advice-add 'python-flymake :around #'my-python-flymake)

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

(provide 'my-python)
