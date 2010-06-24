;;; my-cc-mode.el

(require 'doxymacs)
(require 'find-file)

;; C++

(c-add-style "strou2" '("stroustrup"
                        (c-basic-offset . 4)
                        (c-comment-only-line-offset 0 . 0)
                        (c-block-comment-prefix . "")))
(setq c-default-style "strou2")

(defun my-c-mode-common-hook ()
  (setq comment-start "// ")
  (setq comment-end "" )
  (setq ll-debug-print-filename nil)
  (define-key c-mode-base-map "/" nil)
  (define-key c-mode-base-map (kbd "C-c C-o") 'ff-get-other-file)
  (define-key c-mode-base-map (kbd "C-c d e") 'my-c-make-function-from-prototype)
  (font-lock-add-keywords nil
                          (list (cons (concat "^.*/\*\\s-*\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)")
                                      (list '(1 'my-todo-face t))))
                          'add-to-end)
;;   (c-setup-filladapt)
;;   (filladapt-mode 1)
  (abbrev-mode -1)
  (c-set-style "strou2"))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq cc-other-file-alist
      '(("\\.cc$"
         (".h" ".hh"))
        ("\\.hh$"
         (".cc" ".C"))
        ("\\.c$"
         (".h"))
        ("\\.h$"
         (".cc" ".cpp" ".cxx" ".c" ".C" ".CC"))
        ("\\.C$"
         (".h" ".H" ".hh" ))
        ("\\.H$"
         (".C" ".CC"))
        ("\\.CC$"
         (".HH" ".H" ".hh" ".h"))
        ("\\.HH$"
         (".CC"))
        ("\\.cxx$"
         (".hh" ".h"))
        ("\\.cpp$"
         (".hh" ".h"))))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Make function from prototype

(defun my-c-make-function-from-prototype ()
  "Turn a function prototype into a skeleton implementation."
  (interactive)
  (let (ret-val fcn-name args const namespaces start-of-fcn)
    (save-excursion
      (end-of-line)
      (c-beginning-of-statement 1)
      (beginning-of-line)
      (when (re-search-forward
             "\\s-*\\(.*\\)\\s-+\\([-a-zA-Z0-9_!=<>~]+\\)\\s-*[(]" nil t)
        (setq ret-val (match-string 1))
        (setq ret-val (replace-regexp-in-string "\\(virtual\\|static\\)\\s-*" "" ret-val))
        (setq fcn-name (match-string 2))
        (when (re-search-forward "\\([^)]*\\)[)]" nil t)
          (setq args (match-string 1))
          (setq args (replace-regexp-in-string "\\s-*=.+?," "," args))
          (setq args (replace-regexp-in-string "\\s-*=.+?)" ")" args))
          (setq args (replace-regexp-in-string "\\s-*=.+?$" "" args))
          (if (looking-at "\\s-*const")
              (setq const " const")
            (setq const ""))
          (condition-case nil
              (while 't
                (backward-up-list 1)
                (when (re-search-backward
                     "\\(class\\|namespace\\|struct\\)\\s-+\\([a-zA-Z0-9_]+\\)" nil t)
                  (setq namespaces (concat (match-string 2) "::" namespaces))))
            (error nil)))))
    ;; Switch to other file and insert implementation
    (ff-get-other-file)
    (setq start-of-fcn (point))
    (insert (concat ret-val (unless (string= ret-val "") "\n") namespaces fcn-name "(" args ")" const))
    (insert "\n{\n/** @todo Fill in this function. */\n}\n")
    (unless (eobp)
      (insert "\n"))
    (indent-region start-of-fcn (point) nil)
    (goto-char start-of-fcn)
    (when (fboundp 'doxymacs-insert-function-comment)
      (doxymacs-insert-function-comment))))

;; Doxymacs

(setq-default doxymacs-JavaDoc-blank-singleline-comment-template
              '("/** " > p " */" > n ))

(setq-default doxymacs-JavaDoc-blank-multiline-comment-template
              '("/**" > n "* " p > n "*/" > n ))

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
                       " */" '> 'n 'n)
                    (error "Can't find next function declaration.")
                    (nil)))))

(add-to-list 'doxymacs-doxygen-keywords (list "\\([@\\\\]\\(todo\\|fixme\\)\\)\\>"
                                              '(0 'my-todo-face prepend)))

(add-hook 'c++-mode-hook 'doxymacs-mode)
(add-hook 'c-mode-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (eq major-mode 'c-mode)
      (doxymacs-font-lock))
  (if (eq major-mode 'c++-mode)
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; Done

(provide 'my-cc-mode)
