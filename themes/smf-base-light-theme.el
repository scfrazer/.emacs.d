(deftheme smf-base-light
  "Base light colors.")

(customize-set-variable 'frame-background-mode 'light)
(load-theme 'smf-base t)

(custom-theme-set-faces
 'smf-base-light

 `(ahs-definition-face                 ((t :background "honeydew2")))
 `(ahs-face                            ((t :background "azure2")))
 `(ahs-plugin-defalt-face              ((t :inherit ahs-face)))
 `(bm-face                             ((t :background "lemonchiffon" :extend t)))
 `(caution                             ((t :foreground "black" :background "orange")))
 `(cursor                              ((t :background "deeppink2")))
 `(diff-changed                        ((t :background "#dfefff")))
 `(diff-file-header                    ((t :background "slategray2" :bold t)))
 `(diff-refine-added                   ((t :foreground "black" :background ,(smf-color 157))))
 `(diff-refine-removed                 ((t :foreground "black" :background ,(smf-color 217))))
 `(dired-marked                        ((t :background "lightgoldenrodyellow")))
 `(dired-subtree-depth-1-face          ((t :background "snow")))
 `(dired-subtree-depth-2-face          ((t :background "ghostwhite")))
 `(dired-subtree-depth-3-face          ((t :background "whitesmoke")))
 `(dired-subtree-depth-4-face          ((t :background "floralwhite")))
 `(dired-subtree-depth-5-face          ((t :background "oldlace")))
 `(dired-subtree-depth-6-face          ((t :background "linen")))
 `(easy-escape-delimiter-face          ((t :foreground "green4" :background "gray95")))
 `(ediff-current-diff-A                ((t :foreground "black" :background ,(smf-color 153))))
 `(ediff-even-diff-A                   ((t :foreground "black" :background ,(smf-color 253))))
 `(ediff-fine-diff-A                   ((t :foreground "black" :background ,(smf-color 215))))
 `(ediff-odd-diff-A                    ((t :foreground "black" :background ,(smf-color 253))))
 `(eldoc-highlight-function-argument   ((t :background "lightsteelblue2" :bold t)))
 `(error                               ((t :foreground ,(smf-color 231) :background ,(smf-color 124))))
 `(flymake-error                       ((t :background "mistyrose")))
 `(flymake-note                        ((t :background "papayawhip")))
 `(flymake-warning                     ((t :background "lightyellow")))
 `(flyspell-duplicate                  ((t :background "thistle1")))
 `(flyspell-incorrect                  ((t :background "thistle1")))
 `(font-lock-regexp-grouping-construct ((t :background "whitesmoke" :bold t)))
 `(header-line                         ((t :foreground ,(smf-color 252) :background ,(smf-color 238))))
 `(hl-line                             ((t :inherit nil :background "seashell2")))
 `(ido-first-match                     ((t :inherit font-lock-string-face :bold t)))
 `(ido-only-match                      ((t :foreground "darkgreen" :bold t)))
 `(ido-vertical-first-match-face       ((t :inherit font-lock-string-face :bold t)))
 `(ido-vertical-only-match-face        ((t :foreground "darkgreen" :bold t)))
 `(ido-vertical-match-face             ((t :inherit font-lock-builtin-face :underline t)))
 `(info-header-xref                    ((t :foreground "skyblue2" :italic t)))
 `(isearch                             ((t :background "coral2" :foreground ,(smf-color 231))))
 `(lazy-highlight                      ((t :background "lightsteelblue" :foreground "black")))
 `(line-number                         ((t :foreground ,(smf-color 17) :background ,(smf-color 253) :italic t :underline nil)))
 `(line-number-current-line            ((t :inherit line-number :inverse-video t :underline nil)))
 `(link                                ((t :foreground ,(smf-color 26) :italic t :underline t)))
 `(link-visited                        ((t :foreground ,(smf-color 93) :italic t :underline t)))
 `(markdown-code-face                  ((t :background "gray97")))
 `(markdown-table-face                 ((t :foreground "steelblue4")))
 `(match                               ((t :background "lavenderblush1")))
 `(mc/cursor-face                      ((t :foreground "white" :background ,(smf-color 162))))
 `(my-display-table-face               ((t :foreground ,(smf-color 17) :background ,(smf-color 253) :bold t)))
 `(org-block-background                ((t :background "ivory")))
 `(org-code                            ((t :inherit org-block-background :bold t)))
 `(outline-1                           ((t :foreground ,(smf-color 26) :bold t)))
 `(outline-2                           ((t :foreground ,(smf-color 32) :bold t)))
 `(outline-3                           ((t :foreground "cyan4" :bold t)))
 `(outline-4                           ((t :foreground ,(smf-color 130) :bold t)))
 `(p4o-merge-face                      ((t :background "lightyellow")))
 `(popup-tip-face                      ((t :background "gray85" :foreground "black" :bold t :italic t)))
 `(rg-literal-face                     ((t :foreground "lightblue")))
 `(rg-regexp-face                      ((t :foreground "yellow")))
 `(selectrum-current-candidate         ((t :background "lightsteelblue1")))
 `(show-mark-face                      ((t :foreground "black" :background ,(smf-color 195))))
 `(speedbar-button-face                ((t :foreground "black")))
 `(sqlplus-table-odd-rows-face         ((t :background ,(smf-color 254))))
 `(success                             ((t :foreground "black" :background ,(smf-color 157))))
 `(trailing-whitespace                 ((t :background "gray88")))
 `(warning                             ((t :foreground "black" :background ,(smf-color 227))))
 `(web-mode-html-attr-custom-face      ((t :foreground ,(smf-color 143))))
 `(web-mode-html-attr-name-face        ((t :foreground ,(smf-color 65))))
 `(web-mode-html-attr-value-face       ((t :foreground ,(smf-color 138))))
 `(web-mode-html-tag-face              ((t :foreground ,(smf-color 67))))

 )

(provide-theme 'smf-base-light)
