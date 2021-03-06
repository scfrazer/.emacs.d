;;; my-cc-mode.el

(require 'my-doxymacs)
(require 'find-file)
(require 'sb-imenu-ctags)
(require 'align)

;; Style

(c-add-style "strou2" '("stroustrup"
                        (c-basic-offset . 2)
                        (c-comment-only-line-offset 0 . 0)
                        (c-block-comment-prefix . "")))
(c-add-style "strou4" '("stroustrup"
                        (c-basic-offset . 4)
                        (c-comment-only-line-offset 0 . 0)
                        (c-offsets-alist . ((innamespace . [0])))
                        (c-block-comment-prefix . "")))
(c-add-style "allman" '("bsd"
                        (topmost-intro . -)
                        (c-offsets-alist . ((inextern-lang . 0)))
                        (c-basic-offset . 4)))
(setq c-default-style '((c-mode . "allman")
                        (cc-mode . "allman")
                        (c++-mode . "allman")
                        (php-mode . "strou4")
                        (java-mode . "strou4")))

;; Config

(dolist (type (list "unique_ptr" "shared_ptr" "weak_ptr" "auto_ptr"))
  (add-to-list 'c++-font-lock-extra-types type))

;; Align

(add-to-list 'align-rules-list
             '(my-c++-colon-delimiter
               (regexp . "\\(\\s-+\\):\\(\\s-+\\)")
               (group . (1 2))
               (modes . align-c++-modes)))

;; Modeline format is annoying

(defun my-c-update-modeline (orig-fun)
  (force-mode-line-update))
(advice-add 'c-update-modeline :around #'my-c-update-modeline)

;; Tidy

(defvar my-cc-mode-tidy-executable "clang-format")

(defun my-cc-mode-tidy ()
  "Run formatter on buffer."
  (interactive "*")
  (save-buffer)
  (let ((line-num (line-number-at-pos)))
    (shell-command (concat my-cc-mode-tidy-executable " -style=file -i " (buffer-file-name)))
    (revert-buffer t t)
    (goto-line line-num)))

;; Hooks

(defun my-c-mode-common-hook ()
  (abbrev-mode -1)
  (setq comment-start "// ")
  (setq comment-end "" )
  (highlight-indent-guides-mode 1)
  (flymake-mode 1)
  (setq imenu-create-index-function 'sb-imenu-ctags-create-index)
  (define-key c-mode-base-map (kbd "C-c C-f") 'doxymacs-insert-function-comment)
  (define-key c-mode-base-map (kbd "C-c C-s") 'my-cc-create-skeleton-from-prototype))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Other file

(setq cc-other-file-alist
      '(
        ("\\.cpp$"
         (".hpp" ".h"))
        ("\\.hpp$"
         (".cpp"))
        ("\\.cc$"
         (".h" ".hh"))
        ("\\.hh$"
         (".cc" ".C"))
        ("\\.c$"
         (".h"))
        ("\\.h$"
         (".c" ".cc" ".cpp" ".cxx" ".C" ".CC"))
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
        ))

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Abbrevs

(define-abbrev c++-mode-abbrev-table
  "up"
  "std::unique_ptr<>"
  (lambda ()
    (backward-char 1)))

(define-abbrev c++-mode-abbrev-table
  "sp"
  "std::shared_ptr<>"
  (lambda ()
    (backward-char 1)))

(define-abbrev c++-mode-abbrev-table
  "wp"
  "std::weak_ptr<>"
  (lambda ()
    (backward-char 1)))

(define-abbrev c++-mode-abbrev-table
  "mp"
  ""
  (lambda ()
    (if (looking-back "\\s-+\\([a-zA-Z0-9_]+\\)\\s-*=\\s-*" (point-at-bol))
        (let ((pos (point))
              (var (match-string-no-properties 1))
              std var-type pointer-type)
          (save-excursion
            (c-beginning-of-defun)
            (when (re-search-forward (concat "\\(std::\\)?\\(unique\\|shared\\|weak\\)_ptr<\\(.+\\)>\\s-+" var) pos t)
              (setq std (match-string-no-properties 1))
              (setq pointer-type (match-string-no-properties 2))
              (setq var-type (match-string-no-properties 3))))
          (when pointer-type
            (insert std "make_" pointer-type "<" var-type ">();")
            (backward-char 2)))
      (insert "std::make_unique<>()")
      (backward-char 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work with C++ functions

(defun my-cc-get-namespaces ()
  "Get the current namespace as a list."
  (let (namespaces)
    (save-excursion
      (condition-case nil
          (while 't
            (backward-up-list 1)
            (when (re-search-backward "\\(class\\|namespace\\|struct\\)\\s-+\\([a-zA-Z0-9_]+\\)" (point-at-bol) t)
              (push (match-string-no-properties 2) namespaces)))
        (error nil)))
    namespaces))

(defun my-cc-parse-function-signature ()
  "Parse the current function's signature."
  (let (name args const namespaces returns)
    (save-excursion
      (back-to-indentation)
      (forward-char)
      (c-beginning-of-statement 1)
      (when (re-search-forward "\\([^ \t]+\\)\\s-+\\([^(]+?\\)\\s-*[(]" nil t)
        (setq returns (replace-regexp-in-string "\\(virtual\\|static\\)\\s-*" "" (match-string-no-properties 1)))
        (setq namespaces (split-string (match-string-no-properties 2) "::" t))
        (setq name (car (last namespaces)))
        (setq namespaces (butlast namespaces))
        (when (re-search-forward "\\([^)]*\\)[)]" nil t)
          (setq args (replace-regexp-in-string "\n" " " (match-string-no-properties 1)))
          (setq args (replace-regexp-in-string "\\s-*=.+?," "," args))
          (setq args (replace-regexp-in-string "\\s-+," "," args))
          (setq args (replace-regexp-in-string "\\`\\s-+" "" args))
          (setq args (replace-regexp-in-string "\\s-+\\'" "" args))
          (setq args (replace-regexp-in-string "\\s-\\{2,\\}" " " args))
          (setq const (looking-at "\\s-*const"))
          (setq namespaces (append (my-cc-get-namespaces) namespaces)))))
    (list name args const namespaces returns)))

(defun my-cc-create-skeleton-from-prototype ()
  "Turn a function prototype into a skeleton implementation."
  (interactive)
  (let* ((sig (my-cc-parse-function-signature))
         (name (nth 0 sig))
         (args (nth 1 sig))
         (const (nth 2 sig))
         (function-namespaces (nth 3 sig))
         (returns (nth 4 sig))
         current-namespaces pos)
    (ff-get-other-file)
    (setq current-namespaces (my-cc-get-namespaces))
    (setq pos (point))
    (catch 'done
      (dolist (ns current-namespaces)
        (if (and function-namespaces (string-match ns (car function-namespaces)))
            (setq function-namespaces (cdr function-namespaces))
          (throw 'done t))))
    (insert "\n")
    (forward-line -1)
    (c-indent-line)
    (insert-char ?/ (- 80 (- (point) (point-at-bol))))
    (insert "\n\n")
    (insert returns (if (string= returns "") "" " "))
    (dolist (ns function-namespaces)
      (insert ns "::"))
    (insert name "(" args ")" (if const " const " " "))
    (insert "{\n}")
    (unless (save-excursion (forward-line 1) (eobp))
      (insert "\n"))
    (indent-region pos (point) nil)
    (goto-char pos)
    (forward-line 3)))

(defun my-cc-other-file (&optional arg)
  "Go to other file.  With prefix arg go to function definition/implementation in other file."
  (interactive "P")
  (if (not arg)
      (ff-get-other-file)
    (let* ((sig (my-cc-parse-function-signature))
           (name (nth 0 sig))
           (namespaces (nth 3 sig))
           (returns (nth 4 sig))
           namespace-end)
      (ff-get-other-file)
      (widen)
      (goto-char (point-min))
      (setq namespace-end (point-max))
      (catch 'done
        (while namespaces
          (if (re-search-forward (concat "\\(class\\|namespace\\|struct\\)\\s-+" (car namespaces) "\\s-*[:{]") namespace-end t)
              (progn
                (setq namespaces (cdr namespaces))
                (backward-char)
                (search-forward "{")
                (save-excursion
                  (backward-char)
                  (forward-sexp)
                  (setq namespace-end (point))))
            (throw 'done t))))
      (when namespaces
        (nreverse namespaces)
        (dolist (ns namespaces)
          (setq name (concat ns "::" name))))
      (unless (re-search-forward (concat "^\\s-*" returns "\\s-+" name "\\s-*(") nil t)
        (re-search-forward (concat "^.+" name "\\s-*(") nil t))
      (back-to-indentation)
      (recenter))))

;; Doxymacs

(add-hook 'c++-mode-hook 'doxymacs-mode)
(add-hook 'c-mode-hook 'doxymacs-mode)

;; Done

(provide 'my-cc-mode)
