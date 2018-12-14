(deftheme smf-base-light
  "Base light colors.")

(customize-set-variable 'frame-background-mode 'light)
(load-theme 'smf-base t)

(custom-theme-set-faces
 'smf-base-light

 `(bm-face                        ((t :foreground "black" :background ,(smf-color 221))))
 `(caution                        ((t :foreground "black" :background ,(smf-color 215))))
 `(cursor                         ((t :background "deeppink2")))
 `(diff-refine-added              ((t :foreground "black" :background ,(smf-color 157))))
 `(diff-refine-removed            ((t :foreground "black" :background ,(smf-color 217))))
 `(easy-escape-delimiter-face     ((t :foreground "green4" :background "gray95")))
 `(ediff-current-diff-A           ((t :foreground "black" :background ,(smf-color 153))))
 `(ediff-even-diff-A              ((t :foreground "black" :background ,(smf-color 253))))
 `(ediff-fine-diff-A              ((t :foreground "black" :background ,(smf-color 215))))
 `(ediff-odd-diff-A               ((t :foreground "black" :background ,(smf-color 253))))
 `(error                          ((t :foreground ,(smf-color 231) :background ,(smf-color 124))))
 `(flymake-error                  ((t :background "mistyrose")))
 `(flymake-warning                ((t :background "lightyellow")))
 `(header-line                    ((t :foreground ,(smf-color 252) :background ,(smf-color 238))))
 `(isearch                        ((t :background "coral2" :foreground ,(smf-color 231))))
 `(lazy-highlight                 ((t :background "coral4" :foreground ,(smf-color 231))))
 `(link                           ((t :foreground ,(smf-color 26) :italic t :underline t)))
 `(link-visited                   ((t :foreground ,(smf-color 93) :italic t :underline t)))
 `(line-number                    ((t :foreground ,(smf-color 17) :background ,(smf-color 253) :italic t)))
 `(line-number-current-line       ((t :inherit line-number :inverse-video t)))
 `(match                          ((t :inherit ediff-fine-diff-A)))
 `(mc/cursor-face                 ((t :foreground "white" :background ,(smf-color 162))))
 `(my-display-table-face          ((t :foreground ,(smf-color 17) :background ,(smf-color 253) :bold t)))
 `(org-block-background           ((t :background "ivory")))
 `(org-code                       ((t :inherit org-block-background :bold t)))
 `(outline-1                      ((t :foreground ,(smf-color 26) :bold t :underline t)))
 `(outline-2                      ((t :foreground ,(smf-color 32))))
 `(outline-3                      ((t :foreground ,(smf-color 89))))
 `(outline-4                      ((t :foreground ,(smf-color 130))))
 `(p4o-merge-face                 ((t :background "lightyellow")))
 `(show-mark-face                 ((t :foreground "black" :background ,(smf-color 195))))
 `(speedbar-button-face           ((t :foreground "black")))
 `(sqlplus-table-odd-rows-face    ((t :background ,(smf-color 254))))
 `(success                        ((t :foreground "black" :background ,(smf-color 157))))
 `(trailing-whitespace            ((t :background ,(smf-color 255))))
 `(warning                        ((t :foreground "black" :background ,(smf-color 227))))
 `(web-mode-html-attr-custom-face ((t :foreground ,(smf-color 143))))
 `(web-mode-html-attr-name-face   ((t :foreground ,(smf-color 65))))
 `(web-mode-html-attr-value-face  ((t :foreground ,(smf-color 138))))
 `(web-mode-html-tag-face         ((t :foreground ,(smf-color 67))))

 )

(provide-theme 'smf-base-light)
