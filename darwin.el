(setq default-frame-alist
      '((width . 120) (height . 50)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")")

(setq exec-path (append exec-path (list "/usr/local/bin"
                                        "/usr/local/git/bin"
                                        "/usr/X11/bin"
                                        "/opt/local/bin")))

(require 'grep)
(grep-apply-setting 'grep-template "grep -nH -d skip -I -E -e <R> <C> <F>")
(grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 grep -nH -I -E -e <R> <C>")

(load-theme 'smf-misterioso t)
