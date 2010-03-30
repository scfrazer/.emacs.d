;; Fix backspace/delete

(normal-erase-is-backspace-mode 1)

;; Work stuff

(when (string= (getenv "SHELL") "/bin/tcsh")

  ;; ClearCase VOBs can have problems listing space

  (defvar ls-vob "~/.emacs.d/bin/ls-vob")
  (when (file-exists-p ls-vob)
    (setq dired-free-space-program nil)
    (setq insert-directory-program ls-vob))

  ;; Fix grep color issue

  (grep-apply-setting 'grep-template "setenv GREP_COLOR \"01;31\" ; grep -nH -d skip -I -E -e <R> <C> <F>")
  (grep-apply-setting 'grep-find-template "setenv GREP_COLOR \"01;31\" ; find <D> <X> -type f <F> -print0 | xargs -0 -e grep -nH -I -E -e <R> <C>"))
