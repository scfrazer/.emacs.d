(deftheme smf-darktooth
  "Darktooth colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-darktooth

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 208))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 243) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 95))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 151) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 246) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 130) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 110))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 66))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 66))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 66))))

 )

(provide-theme 'smf-darktooth)
