(deftheme smf-distinguished
  "Distinguished colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-distinguished

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 137))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 243) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 94))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 243) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 179) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 173) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 247))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 143))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 67))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 66))))

 )

(provide-theme 'smf-distinguished)
