(setq default-frame-alist
      '((width . 120) (height . 60)))

;(insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")\n")
(set-default-font "-apple-Fixed-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(setq grep-program "egrep")
(setq igrep-program "egrep")
(setq igrep-find-program "find")

;; Theme

(my-theme-deeper-blue)
