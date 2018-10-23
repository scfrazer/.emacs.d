(deftheme smf-base
  "Base colors.")

(defun smf-color (color-num)
  "Return the color name from a color number."
  (if (or (display-graphic-p)
          (= (display-color-cells) 16777216))
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
  `((((class color)) :foreground ,(smf-color 202)))
  "Caution face."
  :group 'faces)

(custom-theme-set-faces
 'smf-base

 `(Man-overstrike                          ((t :inherit link)))
 `(Man-underline                           ((t :inherit font-lock-variable-name-face)))
 `(avy-lead-face                           ((t :foreground ,(smf-color 226) :background ,(smf-color 161))))
 `(avy-lead-face-0                         ((t :foreground ,(smf-color 161) :background ,(smf-color 25))))
 `(compilation-column-number               ((t :inherit font-lock-variable-name-face)))
 `(compilation-error                       ((t :inherit error)))
 `(compilation-info                        ((t :inherit link)))
 `(compilation-line-number                 ((t :inherit font-lock-preprocessor-face)))
 `(compilation-mode-line-exit              ((t :inherit success)))
 `(compilation-mode-line-fail              ((t :inherit error)))
 `(compilation-mode-line-run               ((t :inherit warning)))
 `(compilation-warning                     ((t :inherit warning)))
 `(easy-escape-delimiter-face              ((t :inherit font-lock-keyword-face)))
 `(easy-escape-face                        ((t :inherit easy-escape-delimiter-face)))
 `(ediff-current-diff-B                    ((t :inherit ediff-current-diff-A)))
 `(ediff-even-diff-B                       ((t :inherit ediff-even-diff-A)))
 `(ediff-fine-diff-B                       ((t :inherit ediff-fine-diff-A)))
 `(ediff-odd-diff-B                        ((t :inherit ediff-odd-diff-A)))
 `(font-lock-comment-delimiter-face        ((t :inherit font-lock-comment-face)))
 `(font-lock-doc-string-face               ((t :inherit font-lock-doc-face)))
 `(font-lock-regexp-grouping-backslash     ((t :bold t)))
 `(font-lock-regexp-grouping-construct     ((t :bold t)))
 `(font-lock-warning-face                  ((t :inherit warning)))
 `(git-timemachine-minibuffer-author-face  ((t :inherit font-lock-keyword-face)))
 `(git-timemachine-minibuffer-detail-face  ((t :inherit font-lock-string-face)))
 `(highlight                               ((t :inherit region)))
 `(hl-line                                 ((t :inherit nil :underline t)))
 `(ido-first-match                         ((t :inherit warning)))
 `(ido-only-match                          ((t :inherit success)))
 `(ido-subdir                              ((t :inherit font-lock-keyword-face)))
 `(js2-error                               ((t :inherit error)))
 `(js2-external-variable                   ((t :inherit flymake-warnline)))
 `(js2-function-param                      ((t :inherit font-lock-variable-name-face)))
 `(js2-jsdoc-tag                           ((t :foreground ,(smf-color 66) :italic t)))
 `(js2-jsdoc-type                          ((t :foreground ,(smf-color 67) :italic t)))
 `(js2-jsdoc-value                         ((t :foreground ,(smf-color 180) :italic t)))
 `(js2-warning                             ((t :inherit flymake-warnline)))
 `(minibuffer-prompt                       ((t :inherit font-lock-keyword-face)))
 `(mode-line                               ((t :foreground ,(smf-color 255) :background ,(smf-color 24))))
 `(mode-line-buffer-id                     ((t :foreground ,(smf-color 220))))
 `(mode-line-inactive                      ((t :foreground ,(smf-color 252) :background ,(smf-color 102))))
 `(my-debug-face                           ((t :inherit caution)))
 `(my-ibuffer-read-only-face               ((t :inherit caution)))
 `(my-narrow-face                          ((t :foreground "black" :background ,(smf-color 226))))
 `(my-read-only-face                       ((t :foreground "black" :background ,(smf-color 202))))
 `(org-level-1                             ((t :inherit outline-1 :underline t)))
 `(region                                  ((t :foreground ,(smf-color 231) :background ,(smf-color 31))))
 `(show-paren-match                        ((t :inherit success)))
 `(show-paren-mismatch                     ((t :inherit error)))
 `(sqlplus-table-head-face                 ((t :foreground ,(smf-color 45) :background "black" :italic t)))
 `(underline                               ((t :underline t)))
 `(web-mode-current-element-highlight-face ((t :bold t :underline t)))
 `(web-mode-error-face                     ((t :inherit error)))
 `(web-mode-function-call-face             ((t :inherit font-lock-function-name-face :underline nil)))
 `(web-mode-html-attr-equal-face           ((t :inherit web-mode-html-attr-name-face)))
 `(web-mode-html-tag-bracket-face          ((t :inherit web-mode-html-tag-face)))
 `(whitespace-empty                        ((t :inherit trailing-whitespace)))
 `(whitespace-tab                          ((t :inherit trailing-whitespace)))
 `(whitespace-trailing                     ((t :inherit trailing-whitespace)))

 )

(provide-theme 'smf-base)
