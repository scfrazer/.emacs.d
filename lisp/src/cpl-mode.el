;;; cpl-mode.el

(require 'cperl-mode)

(define-derived-mode cpl-mode cperl-mode "c-pl"
  "Mode for editing Perl with C++ // comments."
  :abbrev-table cperl-mode-abbrev-table
  :syntax-table cperl-mode-syntax-table
  (set (make-local-variable 'syntax-propertize-function)
       (lambda (start end)
         (goto-char start)
         (setq cperl-syntax-done-to start)
         (cpl-fontify-syntaxically end))))

(defun cpl-fontify-syntaxically (end)
  "Add C++ style // comments."
  (cperl-fontify-syntaxically end)
  (funcall (syntax-propertize-rules ("\\(//\\).*\\(\n\\)" (1 "< c") (2 "> c"))) (point) end))

(defun base-cpl-mode-hook ()
  (when (fboundp 'flymake-mode)
    (flymake-mode 0)))
(add-hook 'cpl-mode-hook 'base-cpl-mode-hook)

(provide 'cpl-mode)
