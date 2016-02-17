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
  `((t :foreground ,(smf-color 215) :background ,(smf-color 94)))
  "Caution face."
  :group 'faces)

(let ((dark '((class color) (background dark)))
      (light '((class color) (background light))))
  (custom-theme-set-faces
   'smf

   `(default ((((type graphic) (background dark)) (:background "#1d2937" :foreground "#f2f2f2"))))
   `(cursor  ((((type graphic) (background dark)) (:background "#00cd00" :foreground "black"))))

   `(avy-lead-face                       ((,dark :foreground "white" :background ,(smf-color 161) :slant normal :weight bold)))
   `(avy-lead-face-0                     ((,dark :foreground "white" :background ,(smf-color 25) :slant normal :weight bold)))
   `(bm-face                             ((,dark :foreground ,(smf-color 51) :background ,(smf-color 17))))
   `(compilation-column-number           ((t :inherit font-lock-variable-name-face)))
   `(compilation-error                   ((t :inherit error)))
   `(compilation-info                    ((t :inherit link)))
   `(compilation-line-number             ((t :inherit font-lock-preprocessor-face)))
   `(compilation-mode-line-exit          ((t :inherit success)))
   `(compilation-mode-line-fail          ((t :inherit error)))
   `(compilation-mode-line-run           ((t :inherit warning)))
   `(compilation-warning                 ((t :inherit warning)))
   `(error                               ((,dark :foreground ,(smf-color 210) :background ,(smf-color 52))))
   `(escape-glyph                        ((,dark :foreground ,(smf-color 166))))
   `(font-lock-builtin-face              ((,dark :foreground ,(smf-color 210))))
   `(font-lock-comment-delimiter-face    ((t :inherit font-lock-comment-face)))
   `(font-lock-comment-face              ((,dark :foreground ,(smf-color 102) :slant italic) (,light :foreground ,(smf-color 60) :slant italic)))
   `(font-lock-constant-face             ((,dark :foreground ,(smf-color 149))))
   `(font-lock-doc-face                  ((,dark :foreground ,(smf-color 223))))
   `(font-lock-doc-string-face           ((t :inherit font-lock-doc-face)))
   `(font-lock-function-name-face        ((,dark :foreground ,(smf-color 178))))
   `(font-lock-keyword-face              ((,dark :foreground ,(smf-color 39) :weight bold)))
   `(font-lock-preprocessor-face         ((,dark :foreground ,(smf-color 220))))
   `(font-lock-regexp-grouping-backslash ((t :weight bold)))
   `(font-lock-regexp-grouping-construct ((t :weight bold)))
   `(font-lock-string-face               ((,dark :foreground ,(smf-color 180))))
   `(font-lock-type-face                 ((,dark :foreground ,(smf-color 123))))
   `(font-lock-variable-name-face        ((,dark :foreground ,(smf-color 84))))
   `(font-lock-warning-face              ((t :inherit warning)))
   `(header-line                         ((,dark :foreground "white" :background "black")))
   `(highlight                           ((t :inherit region)))
   `(hl-line                             ((,dark (:background ,(smf-color 237) :inherit nil))))
   `(ido-first-match                     ((t :inherit warning)))
   `(ido-only-match                      ((t :inherit success)))
   `(ido-subdir                          ((t :inherit font-lock-keyword-face)))
   `(isearch                             ((,dark :foreground "black" :background ,(smf-color 211))))
   `(js2-error                           ((t :inherit error)))
   `(js2-external-variable               ((t :inherit warning)))
   `(js2-function-param                  ((t :inherit font-lock-variable-name-face)))
   `(js2-warning                         ((t :inherit warning)))
   `(lazy-highlight                      ((,dark :foreground "white" :background ,(smf-color 66))))
   `(link                                ((,dark :foreground ,(smf-color 81) :underline t)))
   `(link-visited                        ((,dark :foreground ,(smf-color 212) :underline t)))
   `(match                               ((,dark :foreground "white" :background ,(smf-color 62))))
   `(minibuffer-prompt                   ((t :inherit font-lock-keyword-face)))
   `(mode-line                           ((,dark :foreground ,(smf-color 231) :background ,(smf-color 24))))
   `(mode-line-buffer-id                 ((,dark :foreground ,(smf-color 220) :weight bold)))
   `(mode-line-inactive                  ((,dark :foreground ,(smf-color 231) :background ,(smf-color 102))))
   `(my-debug-face                       ((t :inherit caution)))
   `(my-read-only-face                   ((t :inherit caution)))
   `(region                              ((,dark :foreground ,(smf-color 231) :background ,(smf-color 31))))
   `(show-mark-face                      ((,dark :foreground nil :background ,(smf-color 23))))
   `(show-paren-match-face               ((t :inherit success)))
   `(show-paren-mismatch-face            ((t :inherit error)))
   `(success                             ((,dark :foreground ,(smf-color 120) :background ,(smf-color 22))))
   `(trailing-whitespace                 ((,dark :foreground ,(smf-color 245) :background ,(smf-color 236))))
   `(warning                             ((,dark :foreground ,(smf-color 226) :background ,(smf-color 58))))
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
