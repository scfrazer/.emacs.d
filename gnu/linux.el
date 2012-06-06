;; Fix backspace/delete

(if window-system
    (progn
      (normal-erase-is-backspace-mode 1)
      (blue))
  (blue)
  (set-face-background 'default "color-235")
  (set-face-foreground 'font-lock-comment-face "color-244")
  (set-face-foreground 'font-lock-comment-delimiter-face "color-244")
  (set-face-foreground 'isearch "color-253")
  (set-face-background 'isearch "color-166")
  (set-face-foreground 'isearch-lazy-highlight-face "color-253")
  (set-face-background 'isearch-lazy-highlight-face "color-23")
  (my-keys-define "C-_" 'dabbrev-expand))

;; Work stuff

(when (string= (getenv "SHELL") "/bin/tcsh")

  ;; ClearCase VOBs can have problems listing space

  (defvar ls-vob "~/.emacs.d/bin/ls-vob")
  (when (file-exists-p ls-vob)
    (setq dired-free-space-program nil)
    (setq insert-directory-program ls-vob))

  ;; Fix grep color issue

  (grep-apply-setting 'grep-template "setenv GREP_COLOR \"01;31\" ; grep -nH -d skip -I -E -e <R> <C> <F>")
  (grep-apply-setting 'grep-find-template "setenv GREP_COLOR \"01;31\" ; (find <D> <X> -type f <F> -exec grep -nH -I -E -e <R> <C> {} \\; > /dev/tty) >& /dev/null"))
