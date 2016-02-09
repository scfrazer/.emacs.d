(deftheme smf
  "Terminal colors.")

(defun tty-hex-color (color-num)
  "Return the hex value for a 256-color term color number."
  "#5f5f87")
;;   (let* ((color-values (tty-color-values (format "color-%d" color-num)))
;;          (red (/ (nth 0 color-values) 256))
;;          (green (/ (nth 1 color-values) 256))
;;          (blue (/ (nth 2 color-values) 256)))
;;     (format "#%02X%02X%02X" red green blue)))

(let ((dark '((class color) (background dark)))
      (light '((class color) (background light))))
  (custom-theme-set-faces
   'smf

   `(default ((((type graphic) (background dark)) (:background "#1d2937" :foreground "#f2f2f2"))))
   `(cursor  ((((type graphic) (background dark)) (:background "#00cd00" :foreground "black"))))

   `(avy-lead-face                       ((,dark :foreground "white" :background "color-161" :slant normal :weight bold)))
   `(avy-lead-face-0                     ((,dark :foreground "white" :background "color-25" :slant normal :weight bold)))
   `(bm-face                             ((,dark :foreground "white" :background "color-58")))
   `(compilation-column-number           ((t :inherit font-lock-variable-name-face)))
   `(compilation-error                   ((t :inherit error)))
   `(compilation-info                    ((t :inherit link)))
   `(compilation-line-number             ((t :inherit font-lock-preprocessor-face)))
   `(compilation-mode-line-exit          ((t :inherit success)))
   `(compilation-mode-line-fail          ((t :inherit error)))
   `(compilation-mode-line-run           ((t :inherit warning)))
   `(compilation-warning                 ((t :inherit warning)))
   `(error                               ((,dark :foreground "color-231" :background "color-124")))
   `(escape-glyph                        ((,dark :foreground "color-166")))
   `(font-lock-builtin-face              ((,dark :foreground "color-210")))
   `(font-lock-comment-delimiter-face    ((t :inherit font-lock-comment-face)))
   `(font-lock-comment-face              ((,dark :foreground "color-102" :slant italic) (,light :foreground ,(tty-hex-color 60) :slant italic)))
   `(font-lock-constant-face             ((,dark :foreground "color-149")))
   `(font-lock-doc-face                  ((,dark :foreground "color-223")))
   `(font-lock-doc-string-face           ((t :inherit font-lock-doc-face)))
   `(font-lock-function-name-face        ((,dark :foreground "color-178")))
   `(font-lock-keyword-face              ((,dark :foreground "color-39" :weight bold)))
   `(font-lock-preprocessor-face         ((,dark :foreground "color-220")))
   `(font-lock-regexp-grouping-backslash ((t :weight bold)))
   `(font-lock-regexp-grouping-construct ((t :weight bold)))
   `(font-lock-string-face               ((,dark :foreground "color-180")))
   `(font-lock-type-face                 ((,dark :foreground "color-123")))
   `(font-lock-variable-name-face        ((,dark :foreground "color-84")))
   `(font-lock-warning-face              ((t :inherit warning)))
   `(highlight                           ((t :inherit region)))
   `(hl-line                             ((,dark (:background "color-237" :inherit nil))))
   `(ido-first-match                     ((t :inherit warning)))
   `(ido-only-match                      ((t :inherit success)))
   `(ido-subdir                          ((t :inherit font-lock-keyword-face)))
   `(isearch                             ((,dark :foreground "black" :background "color-211")))
   `(js2-error                           ((t :inherit error)))
   `(js2-external-variable               ((t :inherit warning)))
   `(js2-function-param                  ((t :inherit font-lock-variable-name-face)))
   `(js2-warning                         ((t :inherit warning)))
   `(lazy-highlight                      ((,dark :foreground "white" :background "color-66")))
   `(link                                ((,dark :foreground "color-81" :underline t)))
   `(link-visited                        ((,dark :foreground "color-212" :underline t)))
   `(match                               ((,dark :foreground "white" :background "color-62")))
   `(minibuffer-prompt                   ((t :inherit font-lock-keyword-face)))
   `(mode-line                           ((,dark :foreground "color-231" :background "color-24")))
   `(mode-line-buffer-id                 ((,dark :foreground "color-220" :weight bold)))
   `(mode-line-inactive                  ((,dark :foreground "color-231" :background "color-102")))
   `(region                              ((,dark :foreground "color-231" :background "color-31")))
   `(show-mark-face                      ((,dark :foreground nil :background "color-23")))
   `(show-paren-match-face               ((t :inherit success)))
   `(show-paren-mismatch-face            ((t :inherit error)))
   `(success                             ((,dark :foreground "color-231" :background "color-28")))
   `(trailing-whitespace                 ((,dark :foreground "color-244" :background "color-236")))
   `(warning                             ((,dark :foreground "black" :background "color-184")))
   `(web-mode-html-attr-custom-face      ((,dark :foreground "color-106")))
   `(web-mode-html-attr-equal-face       ((t :inherit web-mode-html-attr-name-face)))
   `(web-mode-html-attr-name-face        ((,dark :foreground "color-108")))
   `(web-mode-html-attr-value-face       ((,dark :foreground "color-137")))
   `(web-mode-html-tag-bracket-face      ((t :inherit web-mode-html-tag-face)))
   `(web-mode-html-tag-face              ((,dark :foreground "color-110")))
   `(whitespace-empty                    ((t :inherit trailing-whitespace)))
   `(whitespace-tab                      ((t :inherit trailing-whitespace)))
   `(whitespace-trailing                 ((t :inherit trailing-whitespace)))

   ))

(provide-theme 'smf)
