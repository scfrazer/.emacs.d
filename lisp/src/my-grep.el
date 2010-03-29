;;; my-grep.el

(require 'grep)

(setq-default grep-highlight-matches t
              grep-find-ignored-directories (list ".git" ".hg" ".bzr" ".svn"))

(grep-apply-setting 'grep-template "grep -nH --color -d skip -I -E -e <R> <C> <F>")
(grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 -e grep -nH --color -I -E -e <R> <C>")

(defun my-grep-setup-hook ()
  (setenv "TERM" "rxvt"))

(add-hook 'grep-setup-hook 'my-grep-setup-hook)

(provide 'my-grep)
