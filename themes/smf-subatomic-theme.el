(deftheme smf-subatomic
  "Subatomic colors.")

(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-subatomic

 `(font-lock-builtin-face       ((t :foreground ,(smf-color 214) :bold t)))
 `(font-lock-comment-face       ((t :foreground ,(smf-color 60) :italic t)))
 `(font-lock-constant-face      ((t :foreground ,(smf-color 15))))
 `(font-lock-doc-face           ((t :foreground ,(smf-color 133) :italic t)))
 `(font-lock-function-name-face ((t :foreground ,(smf-color 109) :underline t)))
 `(font-lock-keyword-face       ((t :foreground ,(smf-color 214) :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground ,(smf-color 181))))
 `(font-lock-string-face        ((t :foreground ,(smf-color 149))))
 `(font-lock-type-face          ((t :foreground ,(smf-color 110))))
 `(font-lock-variable-name-face ((t :foreground ,(smf-color 109))))

 )

(provide-theme 'smf-subatomic)
