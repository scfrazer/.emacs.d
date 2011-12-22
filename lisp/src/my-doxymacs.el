;;; my-doxymacs.el

(require 'doxymacs)

(setq-default doxymacs-JavaDoc-blank-singleline-comment-template
              '("/** " > p " */" >))

(setq-default doxymacs-JavaDoc-blank-multiline-comment-template
              '("/**" > n "* " p > n "*/" >))

(setq-default doxymacs-JavaDoc-function-comment-template
              '((let ((next-func (doxymacs-find-next-func)))
                  (if next-func
                      (list
                       'l
                       "/**" '> 'n
                       " * " 'p '> 'n
                       (when (cdr (assoc 'args next-func))
                         '(l " *" '> 'n))
                       (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
                       (unless (string-match (regexp-quote (cdr (assoc 'return next-func)))
                                             doxymacs-void-types)
                         '(l " *" '> 'n " * " (doxymacs-doxygen-command-char)
                             "return " (p "Returns: ") > n))
                       " */" '>
                       (unless (looking-at "\\s-*$")
                         'n))
                    (error "Can't find next function declaration.")
                    (nil)))))

(setq-default doxymacs-Qt-blank-multiline-comment-template
              '("/*!" > n "* " p > n "*/" >))

(setq-default doxymacs-Qt-function-comment-template
              '((let ((next-func (doxymacs-find-next-func)))
                  (if next-func
                      (list
                       'l
                       "/*!" '> 'n
                       " * \\brief " 'p '> 'n
                       (when (cdr (assoc 'args next-func))
                         '(l " *" '> 'n))
                       (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
                       (unless (string-match (regexp-quote (cdr (assoc 'return next-func)))
                                             doxymacs-void-types)
                         '(l " *" '> 'n " * " (doxymacs-doxygen-command-char)
                             "return " (p "Returns: ") > n))
                       " */" '>
                       (unless (looking-at "\\s-*$")
                         'n))
                    (error "Can't find next function declaration.")
                    (nil)))))

(defun doxymacs-parm-tempo-element (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
        (cond
         ((string= doxymacs-doxygen-style "JavaDoc")
          (list 'l " * " (doxymacs-doxygen-command-char)
                "param " (car parms) " " (list 'p prompt) '> 'n
                (doxymacs-parm-tempo-element (cdr parms))))
         ((string= doxymacs-doxygen-style "Qt")
          (list 'l " * " (doxymacs-doxygen-command-char)
                "param " (car parms) " " (list 'p prompt) '> 'n
                (doxymacs-parm-tempo-element (cdr parms))))
         ((string= doxymacs-doxygen-style "C++")
          (list 'l "/// " (doxymacs-doxygen-command-char)
                "param " (car parms) " " (list 'p prompt) '> 'n
                (doxymacs-parm-tempo-element (cdr parms))))
         (t
          (doxymacs-invalid-style))))
    nil))

(setq-default doxymacs-doxygen-style "Qt")

(provide 'my-doxymacs)
