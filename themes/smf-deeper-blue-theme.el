(deftheme smf-deeper-blue
  "Modified deeper-blue colors.")

(load-theme 'deeper-blue t)
(load-theme 'smf-base-dark t)

(custom-theme-set-faces
 'smf-deeper-blue

 `(font-lock-builtin-face       ((t :foreground "coral3")))
 `(font-lock-constant-face      ((t :foreground "plum3")))
 `(font-lock-keyword-face       ((t :foreground "deepskyblue3")))
 `(font-lock-variable-name-face ((t :foreground "darkseagreen3")))

 )

(provide-theme 'smf-deeper-blue)
