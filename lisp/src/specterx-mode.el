;;; specterx-mode.el --- Verilog with embedded Perl (ugh)

(require 'verilog-mode)

(defgroup specterx-mode nil
  "Verilog with embedded Perl mode."
  :group 'languages)

(defface specterx-directive-face
  '((t (:foreground "tomato" :bold t :weight bold)))
  "Face for SpecterX directives."
  :group 'specterx-mode)

(defface specterx-block-connect-face
  '((t (:background "#353560")))
  "Face for SpecterX block-connect wires."
  :group 'specterx-mode)

(define-derived-mode specterx-mode verilog-mode "SpecterX"
  "Mode for editing Verilog with embedded Perl."
  (let ((keywords (list
                   (cons "\\(<%\\|%>\\|@perl\\)"
                         '(0 'specterx-directive-face t))
                   (cons "<[^%].*?>"
                         '(0 'specterx-directive-face t))
                   (cons "\\(\\.[A-Za-z*%][A-Za-z0-9_*%]+\\)\\s-*("
                         '(1 font-lock-variable-name-face t))
                   (cons "[a-zA-Z0-9_]+__[a-zA-Z0-9_]+__"
                         '(0 'specterx-block-connect-face t))
                   (cons "[^/]\\([*%]\\)[^/]"
                         '(1 'specterx-directive-face t))
                   (cons (concat "specter\\s-+\\("
                                 (regexp-opt '("test" "init_regs" "fixport" "fixpins"
                                               "internal" "implode" "perl" "depend"
                                               "pinfunc"))
                                 "\\|copy[ \t]+\\(on\\|off\\)"
                                 "\\|jeeves[ \t]+\\(spec\\|verilog\\)[ \t]+\\(on\\|off\\)"
                                 "\\)")
                         '(0 'specterx-directive-face t)))))
  (font-lock-add-keywords nil keywords 'add-to-end)))

(provide 'specterx-mode)
