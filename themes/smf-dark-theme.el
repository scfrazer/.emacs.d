(deftheme smf-dark
  "Dark colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-dark

;; `(default                      ((t :foreground "white" :background "gray10")))
 `(font-lock-builtin-face       ((t :foreground "lightskyblue3" :bold t)))
 `(font-lock-comment-face       ((t :foreground "gray50" :italic t)))
 `(font-lock-constant-face      ((t :foreground "deepskyblue3" :italic t)))
 `(font-lock-doc-face           ((t :foreground "thistle4" :italic t)))
 `(font-lock-function-name-face ((t :foreground "slategray3" :underline t)))
 `(font-lock-keyword-face       ((t :foreground "steelblue3" :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground "lightgoldenrod2")))
 `(font-lock-string-face        ((t :foreground "mistyrose3")))
 `(font-lock-type-face          ((t :foreground "cadetblue3")))
 `(font-lock-variable-name-face ((t :foreground "paleturquoise3")))

 )


(provide-theme 'smf-dark)
