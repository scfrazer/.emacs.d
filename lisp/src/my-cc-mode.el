;;; my-cc-mode.el

(require 'my-doxymacs)
(require 'find-file)

;; Style

(c-add-style "strou2" '("stroustrup"
                        (c-basic-offset . 4)
                        (c-comment-only-line-offset 0 . 0)
                        (c-block-comment-prefix . "")))
(setq c-default-style "strou2")

;; Default imenu is not very good

(defvar my-cc-mode-ctags-executable "ctags")

(defun my-cc-mode-imenu-create-index-function ()
  (let ((filename (buffer-file-name))
        (mode major-mode)
        (pos nil)
        (prev-pos -1)
        (item-alist '()))
    (with-temp-buffer
      (shell-command
       (concat my-cc-mode-ctags-executable " -e "
               (cond ((equal mode 'c++-mode)
                      "--language-force=c++ --c++-kinds=cdfmnpstuvx")
                     ((equal mode 'java-mode)
                      "--language-force=java --java-kinds=-efg"))
               " --extra=+q -f- " filename) t)
      (goto-char (point-max))
      (while (not (bobp))
        (forward-line -1)
        (when (looking-at ".+\\(.+\\)[0-9]+,\\([0-9]+\\)")
          (setq pos (1+ (string-to-number (match-string-no-properties 2))))
          (unless (= pos prev-pos)
            (push (cons (match-string-no-properties 1) pos) item-alist))
          (setq prev-pos pos))))
    item-alist))

;; Uncrustify

(defvar my-cc-mode-uncrustify-executable "uncrustify")

(defun my-cc-mode-uncrustify ()
  "Run uncrustify on marked region, or entire buffer."
  (interactive "*")
  (let ((pos (point))
        beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point-min)
            end (point-max)))
    (shell-command-on-region beg end (concat my-cc-mode-uncrustify-executable " -q --no-backup -l CPP") nil t)
    (goto-char pos)
    (beginning-of-line)
    (recenter)))

(defun my-java-mode-uncrustify ()
  "Run uncrustify on marked region, or entire buffer."
  (interactive "*")
  (let ((pos (point))
        beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point-min)
            end (point-max)))
    (shell-command-on-region beg end (concat my-cc-mode-uncrustify-executable " -q --no-backup") nil t)
    (goto-char pos)
    (beginning-of-line)
    (recenter)))

;; Hooks

(defun my-c-mode-common-hook ()
  (setq comment-start "// ")
  (setq comment-end "" )
  (setq imenu-create-index-function 'my-cc-mode-imenu-create-index-function)
  (define-key c-mode-base-map "/" nil)
  (define-key c-mode-base-map (kbd "C-c C-s") 'my-cc-create-skeleton-from-prototype)
  (c-set-style "strou2"))

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
        ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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
    (insert "{\n}\n")
    (indent-region pos (point) nil)
    (goto-char pos)
    (forward-line 3)))

;; Doxymacs

(add-hook 'c++-mode-hook 'doxymacs-mode)
(add-hook 'c-mode-hook 'doxymacs-mode)

;; Done

(provide 'my-cc-mode)
