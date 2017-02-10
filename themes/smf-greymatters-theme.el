(deftheme smf-greymatters
  "Greymatters colors.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-greymatters

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 95))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 246) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 58))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 23) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 95) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 60))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 136))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 23))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 58))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 58))))

 )

(provide-theme 'smf-greymatters)
