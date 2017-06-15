(deftheme smf-github
  "Github colors.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-github

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 94))))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 246) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 60))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 24) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 97) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 125) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 136))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 24))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 67))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 30))))

 )

(provide-theme 'smf-github)
