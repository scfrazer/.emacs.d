(deftheme smf-light
  "Light colors.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-light

 `(font-lock-builtin-face       ((t :foreground "darkgoldenrod4" :bold t)))
 `(font-lock-comment-face       ((t :foreground "gray66" :italic nil)))
 `(font-lock-constant-face      ((t :foreground "purple4" :italic nil)))
 `(font-lock-doc-face           ((t :foreground "slategray4" :italic nil)))
 `(font-lock-function-name-face ((t :foreground "tan4" :underline t)))
 `(font-lock-keyword-face       ((t :foreground "blue4" :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground "yellow4")))
 `(font-lock-string-face        ((t :foreground "violetred4")))
 `(font-lock-type-face          ((t :foreground "deepskyblue4")))
 `(font-lock-variable-name-face ((t :foreground "darkolivegreen")))

 )

(provide-theme 'smf-light)
