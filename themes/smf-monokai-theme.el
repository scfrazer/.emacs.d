(deftheme smf-monokai
  "Monokai colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-monokai

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 197))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 95) :slant italic)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 141))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 95))))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 148))))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 197))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 197))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 186))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 81))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 208))))

 )

(provide-theme 'smf-monokai)
