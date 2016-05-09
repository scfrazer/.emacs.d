(deftheme smf
  "Terminal colors.")

(defun smf-color (color-num)
  "Return the color name from a color number."
  (if (display-graphic-p)
      (let (r g b)
        (setq color-num (- color-num 16))
        (if (< color-num 216)
            (progn
              (setq r (/ color-num 36)
                    g (/ (mod color-num 36) 6)
                    b (mod color-num 6))
              (setq r (if (zerop r) 0 (+ (* r 40) 55)))
              (setq g (if (zerop g) 0 (+ (* g 40) 55)))
              (setq b (if (zerop b) 0 (+ (* b 40) 55))))
          (setq r (+ (* (- color-num 216) 10) 8)
                g r
                b r))
        (format "#%02X%02X%02X" r g b))
    (format "color-%d" color-num)))

(defface caution
  `((((class color) (background dark)) :foreground ,(smf-color 215) :background ,(smf-color 94))
    (((class color) (background light)) :foreground "black" :background ,(smf-color 208)))
  "Caution face."
  :group 'faces)

(let ((dark '((class color) (background dark)))
      (light '((class color) (background light))))
  (custom-theme-set-faces
   'smf

   `(default ((((type graphic) (background dark)) (:background "#1d2937" :foreground "#f2f2f2"))))
   `(cursor  ((((type graphic) (background dark)) (:background "#00cd00" :foreground "black"))))

   `(Man-overstrike                      ((t :inherit link)))
   `(Man-underline                       ((t :inherit font-lock-variable-name-face)))
   `(avy-lead-face                       ((t :foreground ,(smf-color 255) :background ,(smf-color 161) :slant normal)))
   `(avy-lead-face-0                     ((t :foreground ,(smf-color 255) :background ,(smf-color 25) :slant normal)))
   `(bm-face                             ((,dark :background ,(smf-color 17)) (,light :background ,(smf-color 223))))
   `(compilation-column-number           ((t :inherit font-lock-variable-name-face)))
   `(compilation-error                   ((t :inherit error)))
   `(compilation-info                    ((t :inherit link)))
   `(compilation-line-number             ((t :inherit font-lock-preprocessor-face)))
   `(compilation-mode-line-exit          ((t :inherit success)))
   `(compilation-mode-line-fail          ((t :inherit error)))
   `(compilation-mode-line-run           ((t :inherit warning)))
   `(compilation-warning                 ((t :inherit warning)))
   `(ediff-fine-diff-A                   ((,dark :background ,(smf-color 52))))
   `(ediff-fine-diff-B                   ((,dark :background ,(smf-color 22))))
   `(error                               ((,dark :foreground ,(smf-color 210) :background ,(smf-color 52)) (,light :foreground ,(smf-color 255) :background ,(smf-color 124))))
   `(escape-glyph                        ((,dark :foreground ,(smf-color 166))))
   `(font-lock-builtin-face              ((,dark :foreground ,(smf-color 210)) (,light :foreground ,(smf-color 94))))
   `(font-lock-comment-delimiter-face    ((t :inherit font-lock-comment-face)))
   `(font-lock-comment-face              ((,dark :foreground ,(smf-color 102) :slant italic) (,light :foreground ,(smf-color 248) :slant italic)))
   `(font-lock-constant-face             ((,dark :foreground ,(smf-color 149)) (,light :foreground ,(smf-color 22))))
   `(font-lock-doc-face                  ((,dark :foreground ,(smf-color 223)) (,light :foreground ,(smf-color 244))))
   `(font-lock-doc-string-face           ((t :inherit font-lock-doc-face)))
   `(font-lock-function-name-face        ((,dark :foreground ,(smf-color 178)) (,light :foreground ,(smf-color 130))))
   `(font-lock-keyword-face              ((,dark :foreground ,(smf-color 39)) (,light :foreground ,(smf-color 19))))
   `(font-lock-preprocessor-face         ((,dark :foreground ,(smf-color 220)) (,light :foreground ,(smf-color 136))))
   `(font-lock-regexp-grouping-backslash ((t :weight bold)))
   `(font-lock-regexp-grouping-construct ((t :weight bold)))
   `(font-lock-string-face               ((,dark :foreground ,(smf-color 180)) (,light :foreground ,(smf-color 90))))
   `(font-lock-type-face                 ((,dark :foreground ,(smf-color 123)) (,light :foreground ,(smf-color 24))))
   `(font-lock-variable-name-face        ((,dark :foreground ,(smf-color 84)) (,light :foreground ,(smf-color 29))))
   `(font-lock-warning-face              ((t :inherit warning)))
   `(flymake-errline                     ((,light (:inherit nil :background "color-224"))))
   `(header-line                         ((,dark :foreground ,(smf-color 255) :background "black")))
   `(highlight                           ((t :inherit region)))
   `(hl-line                             ((,dark (:background ,(smf-color 237) :inherit nil))))
   `(ido-first-match                     ((t :inherit warning)))
   `(ido-only-match                      ((t :inherit success)))
   `(ido-subdir                          ((t :inherit font-lock-keyword-face)))
   `(isearch                             ((,dark :foreground ,(smf-color 255) :background ,(smf-color 203)) (,light :background "magenta" :foreground ,(smf-color 255))))
   `(js2-error                           ((t :inherit error)))
   `(js2-external-variable               ((t :inherit warning)))
   `(js2-function-param                  ((t :inherit font-lock-variable-name-face)))
   `(js2-jsdoc-tag                       ((t :foreground "color-66" :slant italic)))
   `(js2-jsdoc-type                      ((t :foreground "color-67" :slant italic)))
   `(js2-jsdoc-value                     ((t :foreground "color-180" :slant italic)))
   `(js2-warning                         ((t :inherit warning)))
   `(lazy-highlight                      ((,dark :foreground ,(smf-color 255) :background ,(smf-color 66)) (,light :background "color-225" :foreground "black")))
   `(link                                ((,dark :foreground ,(smf-color 81) :slant italic :underline t)))
   `(link-visited                        ((,dark :foreground ,(smf-color 212) :slant italic :underline t)))
   `(linum                               ((,dark :foreground ,(smf-color 143) :background "black" :slant italic)))
   `(magit-diff-file-heading             ((t :weight normal)))
   `(magit-section-heading               ((,dark :foreground ,(smf-color 222))))
   `(match                               ((,dark :foreground ,(smf-color 255) :background ,(smf-color 62))))
   `(minibuffer-prompt                   ((t :inherit font-lock-keyword-face)))
   `(mode-line                           ((t :foreground ,(smf-color 255) :background ,(smf-color 24))))
   `(mode-line-buffer-id                 ((t :foreground ,(smf-color 220)) (,light :foreground ,(smf-color 20))))
   `(mode-line-inactive                  ((t :foreground ,(smf-color 255) :background ,(smf-color 102))))
   `(my-debug-face                       ((t :inherit caution)))
   `(my-ibuffer-read-only-face           ((t :inherit caution)))
   `(my-read-only-face                   ((t :inherit caution)))
   `(outline-1                           ((,dark :foreground ,(smf-color 45))))
   `(outline-2                           ((,dark :foreground ,(smf-color 44))))
   `(outline-3                           ((,dark :foreground ,(smf-color 43))))
   `(outline-4                           ((,dark :foreground ,(smf-color 42))))
   `(outline-5                           ((,dark :foreground ,(smf-color 41))))
   `(region                              ((,dark :foreground ,(smf-color 255) :background ,(smf-color 31))))
   `(show-mark-face                      ((,dark :foreground nil :background ,(smf-color 23))))
   `(show-paren-match-face               ((t :inherit success)))
   `(show-paren-mismatch-face            ((t :inherit error)))
   `(sqlplus-table-head-face             ((,dark :foreground ,(smf-color 45) :background "black" :slant italic)))
   `(success                             ((,dark :foreground ,(smf-color 120) :background ,(smf-color 22)) (,light :foreground "black" :background ,(smf-color 157))))
   `(trailing-whitespace                 ((,dark :foreground ,(smf-color 245) :background ,(smf-color 235)) (,light :background ,(smf-color 195))))
   `(underline                           ((t :underline t)))
   `(warning                             ((,dark :foreground ,(smf-color 226) :background ,(smf-color 58)) (,light :background ,(smf-color 228))))
   `(web-mode-html-attr-custom-face      ((,dark :foreground ,(smf-color 106))))
   `(web-mode-html-attr-equal-face       ((t :inherit web-mode-html-attr-name-face)))
   `(web-mode-html-attr-name-face        ((,dark :foreground ,(smf-color 108))))
   `(web-mode-html-attr-value-face       ((,dark :foreground ,(smf-color 137))))
   `(web-mode-html-tag-bracket-face      ((t :inherit web-mode-html-tag-face)))
   `(web-mode-html-tag-face              ((,dark :foreground ,(smf-color 110))))
   `(whitespace-empty                    ((t :inherit trailing-whitespace)))
   `(whitespace-tab                      ((t :inherit trailing-whitespace)))
   `(whitespace-trailing                 ((t :inherit trailing-whitespace)))

   ))

(provide-theme 'smf)
