(setq default-frame-alist
      '((width . 120) (height . 52)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")")
(set-default-font "-outline-Office Code Pro-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")

(setq-default dired-listing-switches "-al")

(load-theme 'smf-light t)
