(setq default-frame-alist
      '((width . 120) (height . 56)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")")
(set-default-font "-apple-Inconsolatax-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

;(set-face-attribute 'my-buffer-face-mode-face nil :family "Terminus" :height 120)

(setq exec-path (append exec-path (list "/usr/local/bin"
                                        "/usr/local/git/bin"
                                        "/usr/X11/bin"
                                        "/opt/local/bin")))

(setq-default save-interprogram-paste-before-kill nil
              select-active-regions nil
              my-set-cursor-color-read-only-color "yellow3"
              my-set-cursor-color-normal-color "green3")

(grep-apply-setting 'grep-template "grep -nH -d skip -I -E -e <R> <C> <F>")
(grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 grep -nH -I -E -e <R> <C>")

;; Theme

(white)
