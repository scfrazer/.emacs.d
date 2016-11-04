(deftheme smf-zenburn
  "Zenburn colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-zenburn

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 73))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 102) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 151))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 151))))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 116) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 223) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 111))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 174))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 109))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 144))))

 )

(provide-theme 'smf-zenburn)
