;;; my-abbrev.el

(require 'abbrev)

;; (defun my-abbrev-expand-function ()
;;   "If after a paren, expand it.  Otherwise do default abbrev expansion."
;;   (let ((char (char-before)))
;;     (if (= (char-syntax char) ?\()
;;         (progn
;;           (insert "\n\n" (matching-paren char))
;;           (indent-according-to-mode)
;;           (forward-line -1)
;;           (indent-according-to-mode))
;;       (abbrev--default-expand))))
;; (setq-default abbrev-expand-function #'my-abbrev-expand-function)

(define-abbrev global-abbrev-table
  "filename"
  ""
  (lambda() (insert (or buffer-file-name "*NOFILE*"))))

(define-abbrev global-abbrev-table
  "file"
  ""
  (lambda()
    (let ((filename (buffer-file-name (and (minibufferp) (window-buffer (minibuffer-selected-window))))))
      (insert (if filename
                  (if current-prefix-arg
                      (file-name-sans-extension (file-name-nondirectory filename))
                    (file-name-nondirectory filename))
                "*NOFILE*")))))

(define-abbrev global-abbrev-table
  "case"
  ""
  (lambda ()
    (let ((beg (point)))
      (cond
       ((member major-mode '(emacs-lisp-mode lisp-interaction-mode))
        (insert "(case some-variable\n (match-1\n (do-stuff))\n (t\n (do-default-stuff)))")
        (indent-region beg (point))
        (goto-char (+ beg 6)))
       ((member major-mode '(c-mode c++-mode web-mode php-mode))
        (insert "switch(some_variable) {\n case match_1:\n do_stuff();\n break;\n default:\n do_default_stuff();\n }")
        (indent-region beg (point))
        (goto-char (+ beg 7)))
       ((member major-mode '(sv-mode specterx-mode verilog-mode))
        (insert "case(some_variable)\n match_1: begin\n do_stuff();\n end\n default: begin\n do_default_stuff()\n end\n endcase")
        (indent-region beg (point))
        (goto-char (+ beg 5)))
       ((equal major-mode 'python-mode)
        (insert "if some_variable == match_1:\n do_stuff()\n else:\n do_default_stuff()")
        (indent-region beg (point))
        (goto-char (+ beg 3)))
       ((member major-mode '(cperl-mode cpl-mode))
        (insert "if ($some_variable == match_1) {\n do_stuff();\n } else {\n do_default_stuff();\n }")
        (indent-region beg (point))
        (goto-char (+ beg 4)))
       ((equal major-mode 'csh-mode)
        (insert "switch($some_variable)\n case match_1:\n do_stuff\n breaksw\n default:\n do_default_stuff\n break\n endsw")
        (indent-region beg (point))
        (goto-char (+ beg 7)))
       ((equal major-mode 'sh-mode)
        (insert "case $some_variable in\n match_1)\n do_stuff();\n ;;\n *)\n do_default_stuff();\n ;;\n esac")
        (indent-region beg (point))
        (goto-char (+ beg 5)))
       ))))

(define-abbrev global-abbrev-table
  "elif"
  ""
  (lambda ()
    (let ((beg (point)))
      (cond
       ((member major-mode '(emacs-lisp-mode lisp-interaction-mode))
        (insert "(cond\n ((equal some-variable match-1)\n (do-stuff))\n (t\n (do-default-stuff)))\n")
        (indent-region beg (point))
        (goto-char beg)
        (forward-line 1)
        (back-to-indentation)
        (forward-char 2))
       ((member major-mode '(c-mode c++-mode web-mode php-mode))
        (insert "else if(some_variable) {\n do_stuff();\n }")
        (indent-region beg (point))
        (goto-char (+ beg 8)))
       ((member major-mode '(sv-mode specterx-mode verilog-mode))
        (insert "else if(some_variable) begin\n do_stuff();\n end")
        (indent-region beg (point))
        (goto-char (+ beg 8)))
       ((equal major-mode 'python-mode)
        (insert "elif some_variable:\n do_stuff()")
        (indent-region beg (point))
        (goto-char (+ beg 3)))
       ((member major-mode '(cperl-mode cpl-mode))
        (insert "elsif($some_variable) {\n do_stuff();\n }")
        (indent-region beg (point))
        (goto-char (+ beg 6)))
       ((equal major-mode 'csh-mode)
        (insert "else if ( $some_variable == 'match_1' ) then\n do_suff")
        (indent-region beg (point))
        (goto-char (+ beg 10)))
       ((equal major-mode 'sh-mode)
        (insert "else if [[ $some_variable =~ match_1 ]]; then\n do_stuff;")
        (indent-region beg (point))
        (goto-char (+ beg 11)))
       ))))

(provide 'my-abbrev)
