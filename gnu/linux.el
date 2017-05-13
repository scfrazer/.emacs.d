;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Work stuff
(when (string= (getenv "SHELL") "/bin/tcsh")

  ;; ClearCase VOBs can have problems listing space

  (defvar ls-vob "~/.emacs.d/bin/ls-vob")
  (when (file-exists-p ls-vob)
    (setq dired-free-space-program nil)
    (setq insert-directory-program ls-vob))

  ;; Fix grep color issue

  (eval-after-load "grep"
    '(progn
       (grep-apply-setting 'grep-template "grep -nH -d skip -I -E -e <R> <C> <F>")
       (grep-apply-setting 'grep-find-template "(find <D> <X> -type f <F> -exec grep -nH -I -E -e <R> <C> {} \\; > /dev/tty) >& /dev/null"))))

;; Theme
(load-theme 'smf-light t)
