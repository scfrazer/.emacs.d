(deftheme smf-flatland
  "Flatland colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-flatland

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 209))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 244) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 150))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 244))))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 74))))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 209))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 189))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 189))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 74))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 228))))

 )

(provide-theme 'smf-flatland)
