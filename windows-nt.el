(setq default-frame-alist
      '((width . 120) (height . 60)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")")
(set-default-font "-outline-Office Code Pro-normal-normal-normal-mono-18-*-*-*-c-*-iso8859-1")

(load-theme 'smf-bright t)
