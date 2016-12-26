(deftheme smf-ample
  "Ample colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-ample

 `(default                      ((t :foreground ,(smf-color 252))))
 `(font-lock-builtin-face       ((t :foreground ,(smf-color 146))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 243) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 139))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 101) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 150) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 109))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 173))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 180))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 137))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 150))))

 )

(provide-theme 'smf-ample)
