(deftheme smf-misterioso
  "Misterioso colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-misterioso

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 44))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 107) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 30))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 223) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 50) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 214))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 220))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 166))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 80))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 186))))

 )

(provide-theme 'smf-misterioso)
