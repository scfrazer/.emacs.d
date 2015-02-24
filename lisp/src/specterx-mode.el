;;; specterx-mode.el --- Verilog with embedded Perl (ugh)

(require 'sv-mode)

(defgroup specterx-mode nil
  "Verilog with embedded Perl mode."
  :group 'languages)

(defface specterx-directive-face
  '((t (:foreground "#005fff")))
  "Face for SpecterX directives."
  :group 'specterx-mode)

(defvar specterx-directive-regexp
  (concat "specter\\s-+\\("
          (regexp-opt '("test" "init_regs" "fixport" "fixpins"
                        "internal" "implode" "perl" "depend"
                        "pinfunc"))
          "\\|copy[ \t]+\\(on\\|off\\)"
          "\\|jeeves[ \t]+\\(spec\\|verilog\\)[ \t]+\\(on\\|off\\)"
          "\\)")
  "SpecterX directives.")

(defun specterx-insert-delimiters ()
  "Insert <%%> pair."
  (interactive "*")
  (insert "<%%>")
  (forward-char -2))

(defun specterx-comment-region (beg end &optional arg)
  "Comment the region, and take care of SpecterX directives."
  (funcall (default-value 'comment-region-function) beg end arg)
  (save-excursion
    (goto-char beg)
    (while (search-forward "<%" end t)
      (replace-match "<#"))
    (goto-char beg)
    (while (search-forward "%>" end t)
      (replace-match "#>"))
    (goto-char beg)
    (while (re-search-forward specterx-directive-regexp end t)
      (replace-match (concat "X" (match-string 0))))))

(defun specterx-uncomment-region (beg end &optional arg)
  "Uncomment the region, and take care of SpecterX directives."
  (funcall (default-value 'uncomment-region-function) beg end arg)
  (save-excursion
    (goto-char beg)
    (while (search-forward "<#" end t)
      (replace-match "<%"))
    (goto-char beg)
    (while (search-forward "#>" end t)
      (replace-match "%>"))
    (while (re-search-forward (concat "X\\(" specterx-directive-regexp "\\)") end t)
      (replace-match (match-string 1)))))

(defvar specterx-mode-syntax-table
  (let ((table (copy-syntax-table sv-mode-syntax-table)))
    (modify-syntax-entry ?% ". 23b" table)
    (modify-syntax-entry ?< ". 1b" table)
    (modify-syntax-entry ?> ". 4b" table)
    table)
  "Syntax table used in specterx-mode buffers.")

(define-derived-mode specterx-mode sv-mode "SpecterX"
  "Mode for editing Verilog with embedded Perl."
  :abbrev-table nil
  :syntax-table specterx-mode-syntax-table
  (let ((keywords (list
                   (cons "\\(<%\\|%>\\|@perl\\)"
                         '(0 'specterx-directive-face t))
                   (cons "[a-zA-Z0-9_]\\([*%]\\)"
                         '(1 'specterx-directive-face t))
                   (cons "\\([*%]\\)[a-zA-Z0-9_]"
                         '(1 'specterx-directive-face t))
                   (cons (concat "\\_<" specterx-directive-regexp)
                         '(0 'specterx-directive-face t)))))
    (setq ff-other-file-alist '(("\\.s$" (".v"))))
    (font-lock-add-keywords nil keywords 'add-to-end)
    (set (make-local-variable 'comment-region-function) 'specterx-comment-region)
    (set (make-local-variable 'uncomment-region-function) 'specterx-uncomment-region)))

(define-key specterx-mode-map (kbd "C-c <") 'specterx-insert-delimiters)

(provide 'specterx-mode)
