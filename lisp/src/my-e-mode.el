;;; my-e-mode.el

(require 'e-mode)

(define-abbrev e-mode-abbrev-table
  "pre"
  ""
  (lambda () (insert (e-mode-abbrev-get-prefix)))
  nil (list :case-fixed nil))

(define-abbrev e-mode-abbrev-table
  "PRE"
  ""
  (lambda () (insert (upcase (e-mode-abbrev-get-prefix))))
  nil (list :case-fixed nil))

(define-abbrev e-mode-abbrev-table
  "ro"
  "read_only()"
  (lambda () (backward-char)))

(define-abbrev e-mode-abbrev-table "bit" "as_a(bit)")
(define-abbrev e-mode-abbrev-table "bool" "as_a(bool)")
(define-abbrev e-mode-abbrev-table "uint" "as_a(uint)")

(defvar my-e-mode-field-as-constraint nil)

(defun my-e-mode-copy-field-as-constraint ()
  "Copy the field at point as a constraint."
  (interactive)
  (save-excursion
    (let (field subtype struct package bound)
      (setq field (buffer-substring (progn (skip-syntax-backward "w_") (point))
                                    (progn (skip-syntax-forward "w_") (point))))
      (backward-up-list)
      (re-search-backward "\\_<\\(when\\|extend\\|struct\\|unit\\)\\_>")
      (save-excursion
        (re-search-forward "\\(\\_<like\\_>\\|{\\)")
        (setq bound (match-beginning 0)))
      (re-search-forward "[ \t\n]+\\(\\([a-zA-Z0-9_']+[ \t\n]+\\)*\\)\\([a-zA-Z0-9_:]+\\)" bound)
      (setq subtype (match-string 1))
      (setq struct (match-string 3))
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*package\\s-+\\([a-zA-Z0-9_]+\\)" nil t)
        (setq package (match-string 1)))
      (unless (or (string-match "^csco" struct)
                  (string-match "::" struct)
                  (null package))
        (setq struct (concat package "::" struct)))
      (setq my-e-mode-field-as-constraint
            (concat "extend " (or subtype " ") struct " {\n"
                    "    keep " field " == TODO;\n"
                    "};\n"))
      (message (concat "Copied field '" field "' as constraint")))))

(defun my-e-mode-paste-field-as-constraint ()
  "Paste the copied constraint from `my-e-mode-copy-field-as-constraint'."
  (interactive)
  (let ((start (point)))
    (insert my-e-mode-field-as-constraint)
    (indent-region start (point))))

(define-key e-mode-map (kbd "<f11>") 'my-e-mode-copy-field-as-constraint)
(define-key e-mode-map (kbd "<S-f11>") 'my-e-mode-paste-field-as-constraint)

(provide 'my-e-mode)
