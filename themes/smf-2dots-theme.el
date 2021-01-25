(deftheme smf-2dots
  "Colors from the game two-dots.")

(load-theme 'smf-base-light t)

(custom-theme-set-faces
 'smf-2dots

 `(font-lock-builtin-face       ((t :foreground "#425772" :bold t)))
 `(font-lock-comment-face       ((t :foreground "gray66" :italic t)))
 `(font-lock-constant-face      ((t :foreground "#8f6c82" :italic t)))
 `(font-lock-doc-face           ((t :foreground "slategray4" :italic t)))
 `(font-lock-function-name-face ((t :foreground "#f7bd6b" :underline t)))
 `(font-lock-keyword-face       ((t :foreground "#293863" :bold t)))
 `(font-lock-preprocessor-face  ((t :foreground "#fcd9a3")))
 `(font-lock-string-face        ((t :foreground "#651d33")))
 `(font-lock-type-face          ((t :foreground "#75c497")))
 `(font-lock-variable-name-face ((t :foreground "#649e85")))

 )

(provide-theme 'smf-2dots)
