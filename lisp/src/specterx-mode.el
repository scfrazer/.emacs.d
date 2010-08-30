;;; specterx-mode.el --- Verilog with embedded Perl (ugh)

(require 'verilog-mode)

(defgroup specterx-mode nil
  "Verilog with embedded Perl mode."
  :group 'languages)

(defface specterx-directive-face
  '((t (:foreground "tomato3")))
  "Face for SpecterX directives."
  :group 'specterx-mode)

(defun specterx-mode-offset-background-color ()
  (let* ((bg-color (cdr (assoc 'background-color (frame-parameters))))
         (vals (mapcar (lambda (val) (lsh val -8)) (color-values bg-color))))
    (if (eq frame-background-mode 'dark)
        (setq vals (mapcar (lambda (val) (min 255 (+ val #x1c))) vals))
      (setq vals (mapcar (lambda (val) (max 0 (- val #x1c))) vals)))
    (apply 'format "#%02x%02x%02x" vals)))

(defface specterx-block-connect-face
  (list (list t `(:background ,(specterx-mode-offset-background-color))))
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
