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
  "read_only"
  (lambda ()
    (unless (looking-at "\\s-*\(")
      (save-excursion
        (insert "(")
        (end-of-line)
        (delete-region (point) (progn (skip-chars-backward " \t;") (point)))
        (insert ");"))
      (forward-char))))

(define-abbrev e-mode-abbrev-table
  "ron"
  "read_only"
  (lambda ()
    (save-excursion
      (insert "(")
      (when (re-search-forward "\\(and\\|or\\|;\\)" nil t)
        (goto-char (match-beginning 0))
        (skip-syntax-backward "-"))
      (insert ")"))))

(define-abbrev e-mode-abbrev-table "bit" "as_a(bit)")
(define-abbrev e-mode-abbrev-table "bool" "as_a(bool)")
(define-abbrev e-mode-abbrev-table "uint" "as_a(uint)")

(defun my-e-mode-extend-item ()
  "Copy a field/method/tcm as an extension."
  (interactive)
  (save-excursion
    (let (item type subtype struct package bound)
      (skip-syntax-forward "^w_")
      (setq item (buffer-substring (progn (skip-syntax-backward "w_") (point))
                                   (progn (skip-syntax-forward "w_") (point))))
      (if (looking-at "\\s-*:")
          (setq type 'field)
        (setq type 'method)
        (setq item
              (buffer-substring
               (progn (skip-syntax-backward "w_")
                      (point))
               (progn (re-search-forward "\\_<is\\([ \t\n]+\\(first\\|also\\|only\\|empty\\|undefined\\)\\|[ \t\n]*{\\)" nil t)
                      (goto-char (match-beginning 0))
                      (skip-syntax-backward "-")
                      (point)))))
      (backward-up-list)
      (re-search-backward "\\_<\\(when\\|extend\\|struct\\|unit\\)\\_>")
      (save-excursion
        (re-search-forward "\\(\\_<like\\_>\\|{\\)")
        (setq bound (match-beginning 0)))
      (re-search-forward "[ \t\n]+\\(\\([a-zA-Z0-9_']+[ \t\n]+\\)*\\)\\([a-zA-Z0-9_:]+\\)" bound)
      (setq subtype (match-string 1))
      (setq struct (match-string 3))
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*package\\s-+\\([a-zA-Z0-9_]+\\)\\s-*;" nil t)
        (setq package (match-string 1)))
      (unless (or (string-match "^csco" struct)
                  (string-match "::" struct)
                  (null package))
        (setq struct (concat package "::" struct)))
      (if (equal type 'field)
          (progn
            (kill-new (concat "extend " (or subtype " ") struct " {\n"
                              "    keep " item " == TODO;\n"
                              "};\n"))
            (message "Field constraint copied"))
        (kill-new (concat "extend " (or subtype " ") struct " {\n"
                          "    " item " is also {\n"
                          "        // TODO\n"
                          "    };\n};\n"))
        (message "Method extension copied")))))

(define-key e-mode-map (kbd "<f11>") 'my-e-mode-extend-item)

(provide 'my-e-mode)
