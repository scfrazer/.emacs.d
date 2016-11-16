(deftheme smf-base-dark
  "Base dark colors.")

(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'smf-base t)

(custom-theme-set-faces
 'smf-base-dark

   `(default ((((type graphic) (background dark)) (:background "#1d2937" :foreground "#f2f2f2"))))
   `(cursor  ((((type graphic) (background dark)) (:background "#00cd00" :foreground "black"))))

   `(bm-face                        ((t :foreground ,(smf-color 231) :background ,(smf-color 95))))
   `(diff-added                     ((t :foreground "green" :background ,(smf-color 237))))
   `(diff-removed                   ((t :foreground "red" :background ,(smf-color 237))))
   `(error                          ((t :foreground ,(smf-color 210) :background ,(smf-color 52))))
   `(escape-glyph                   ((t :foreground ,(smf-color 166))))
   `(header-line                    ((t :foreground ,(smf-color 255) :background "black")))
   `(isearch                        ((t :foreground ,(smf-color 231) :background ,(smf-color 203))))
   `(js2-object-property            ((t :foreground ,(smf-color 152))))
   `(lazy-highlight                 ((t :foreground ,(smf-color 231) :background ,(smf-color 66))))
   `(link                           ((t :foreground ,(smf-color 81) :italic t :underline t)))
   `(link-visited                   ((t :foreground ,(smf-color 212) :italic t :underline t)))
   `(linum                          ((t :foreground ,(smf-color 143) :background "black" :italic t)))
   `(outline-1                      ((t :foreground ,(smf-color 45))))
   `(outline-2                      ((t :foreground ,(smf-color 44))))
   `(outline-3                      ((t :foreground ,(smf-color 43))))
   `(outline-4                      ((t :foreground ,(smf-color 42))))
   `(outline-5                      ((t :foreground ,(smf-color 41))))
   `(show-mark-face                 ((t :foreground nil :background ,(smf-color 239))))
   `(success                        ((t :foreground ,(smf-color 120) :background ,(smf-color 22))))
   `(trailing-whitespace            ((t :background ,(smf-color 52))))
   `(warning                        ((t :foreground ,(smf-color 226) :background ,(smf-color 58))))
   `(web-mode-html-attr-custom-face ((t :foreground ,(smf-color 106))))
   `(web-mode-html-attr-name-face   ((t :foreground ,(smf-color 108))))
   `(web-mode-html-attr-value-face  ((t :foreground ,(smf-color 137))))
   `(web-mode-html-tag-face         ((t :foreground ,(smf-color 110))))
   `(whitespace-tab                 ((t :background ,(smf-color 237))))

   )

(provide-theme 'smf-base-dark)
