;;; my-grep.el

(require 'grep)

(setq-default grep-highlight-matches 'always
              grep-find-ignored-directories (list ".git" ".hg" ".bzr" ".svn")
              grep-find-ignored-files (list ".#*" "*.o" "*~" "*.so" "*.a" "*.elc" "*.lib" "*.lo" "*.la" "*.pyc")
              grep-files-aliases '(
                                   ("all" . "*")
                                   ("c" . "*.h *.c")
                                   ("cc" . "*.h *.cc")
                                   ("cpp" . "*.hpp *.cpp")
                                   ("dv" . "*.sv *.svh *.cpp *.hpp")
                                   ("el" . "*.el")
                                   ("pl" . "*.pl *.pm")
                                   ("py" . "*.py")
                                   ("rtl" . "*.s *.v *.vh")
                                   ("vtt" . "*.java *.php *.json *.html *.js")
                                   ))

(grep-apply-setting 'grep-template "grep <C> -nH -d skip -I <X> -E -e <R> <F>")
(grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 -e grep -nH -I -E -e <R> <C>")

(defun my-grep-setup-hook ()
  (setenv "TERM" "xterm-color"))

(add-hook 'grep-setup-hook 'my-grep-setup-hook)

(defun grep-read-files (regexp)
  "Read files arg for interactive grep."
  (let* ((bn (or (buffer-file-name)
                 (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
         (fn (and bn
                  (stringp bn)
                  (file-name-nondirectory bn)))
         (default "*")
         (files (completing-read
                 (concat "Search for \"" regexp
                         "\" in files"
                         (if default (concat " (default " default ")"))
                         ": ")
                 'read-file-name-internal
                 nil nil nil 'grep-files-history
                 (delete-dups
                  (delq nil (append (list default)
                                    (mapcar 'car grep-files-aliases)))))))
    (and files
         (or (cdr (assoc files grep-files-aliases))
             files))))

(defun my-grep-find-tag-function ()
  "Get default grep string from region if possible."
  (if (and current-prefix-arg (mark t))
      (progn
        (setq current-prefix-arg nil)
        (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
    (or (funcall (or (get major-mode 'find-tag-default-function)
                     'find-tag-default))
        "")))

(defun my-lgrep (&optional arg)
  "Like `lgrep', but with \\[universal-argument] take default string from region."
  (interactive)
  (let ((find-tag-default-function 'my-grep-find-tag-function))
    (call-interactively 'lgrep)))

(defun my-rgrep (&optional arg)
  "Like `rgrep', but with \\[universal-argument] take default string from region."
  (interactive)
  (let ((find-tag-default-function 'my-grep-find-tag-function))
    (call-interactively 'rgrep)))

(provide 'my-grep)
