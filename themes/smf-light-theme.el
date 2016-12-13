(deftheme smf-light
  "Light colors.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-light

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 94))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 248) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 106))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 244) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 130) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 19))))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 136))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 90))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 31))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 28))))

 )

(provide-theme 'smf-light)
