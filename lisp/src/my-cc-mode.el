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
                      "--language-force=c++ --c++-kinds=-egmstuvn+p")
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

;; Hooks

(defun my-c-mode-common-hook ()
  (setq comment-start "// ")
  (setq comment-end "" )
  (setq imenu-create-index-function 'my-cc-mode-imenu-create-index-function)
  (define-key c-mode-base-map "/" nil)
  (define-key c-mode-base-map (kbd "C-c d e") 'my-c-make-function-from-prototype)
  (abbrev-mode -1)
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

(add-hook 'c++-mode-hook 'doxymacs-mode)
(add-hook 'c-mode-hook 'doxymacs-mode)

;; Done

(provide 'my-cc-mode)
