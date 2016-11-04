(deftheme smf-dark
  "Dark colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-dark

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 210))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 102) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 149))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 223))))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 178))))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 39))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 220))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 180))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 123))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 84))))

 )

(provide-theme 'smf-dark)
