(deftheme smf-leuven
  "Leuven colors.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-leuven

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 26))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 245) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 130))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 22) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 24) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 21) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 244))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 28))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 61))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 133))))

 )

(provide-theme 'smf-leuven)
