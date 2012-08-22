;;; specterx-mode.el --- Verilog with embedded Perl (ugh)

(require 'sv-mode)

(defgroup specterx-mode nil
  "Verilog with embedded Perl mode."
  :group 'languages)

(defface specterx-directive-face
  '((t (:foreground "tomato3")))
  "Face for SpecterX directives."
  :group 'specterx-mode)

(define-derived-mode specterx-mode sv-mode "SpecterX"
  "Mode for editing Verilog with embedded Perl."
  :abbrev-table nil
  :syntax-table nil
  (let ((keywords (list
                   (cons "\\(<%\\|%>\\|@perl\\)"
                         '(0 'specterx-directive-face t))
                   (cons "[a-zA-Z0-9_]\\([*%]\\)"
                         '(1 'specterx-directive-face t))
                   (cons "\\([*%]\\)[a-zA-Z0-9_]"
                         '(1 'specterx-directive-face t))
                   (cons (concat "specter\\s-+\\("
                                 (regexp-opt '("test" "init_regs" "fixport" "fixpins"
                                               "internal" "implode" "perl" "depend"
                                               "pinfunc"))
                                 "\\|copy[ \t]+\\(on\\|off\\)"
                                 "\\|jeeves[ \t]+\\(spec\\|verilog\\)[ \t]+\\(on\\|off\\)"
                                 "\\)")
                         '(0 'specterx-directive-face t)))))
    (setq ff-other-file-alist '(("\\.s$" (".v"))))
    (font-lock-add-keywords nil keywords 'add-to-end)))

(provide 'specterx-mode)
