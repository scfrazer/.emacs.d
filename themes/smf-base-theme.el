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

(defface outline-1
  '((t :inherit font-lock-function-name-face))
  "Level 1."
  :group 'outlines)

(defface outline-2
  '((t :inherit font-lock-variable-name-face))
  "Level 2."
  :group 'outlines)

(defface outline-3
  '((t :inherit font-lock-keyword-face))
  "Level 3."
  :group 'outlines)

(defface outline-4
  '((t :inherit font-lock-comment-face))
  "Level 4."
  :group 'outlines)

(defface outline-5
  '((t :inherit font-lock-type-face))
  "Level 5."
  :group 'outlines)

(defface outline-6
  '((t :inherit font-lock-constant-face))
  "Level 6."
  :group 'outlines)

(custom-theme-set-faces
 'smf-base

 `(Man-overstrike                           ((t :inherit link)))
 `(Man-underline                            ((t :inherit font-lock-variable-name-face)))
 `(avy-lead-face                            ((t :foreground ,(smf-color 226) :background ,(smf-color 161) :bold t)))
 `(avy-lead-face-0                          ((t :foreground ,(smf-color 161) :background ,(smf-color 25) :bold t)))
 `(compilation-column-number                ((t :inherit font-lock-variable-name-face)))
 `(compilation-error                        ((t :inherit error)))
 `(compilation-info                         ((t :inherit link)))
 `(compilation-line-number                  ((t :inherit font-lock-preprocessor-face)))
 `(compilation-mode-line-exit               ((t :inherit success)))
 `(compilation-mode-line-fail               ((t :inherit error)))
 `(compilation-mode-line-run                ((t :inherit warning)))
 `(compilation-warning                      ((t :inherit warning)))
 `(completions-annotations                  ((t :italic nil)))
 `(completions-common-part                  ((t :inherit font-lock-builtin-face)))
 `(completions-first-difference             ((t :inherit font-lock-keyword-face)))
 `(cperl-nonoverridable-face                ((t :inherit font-lock-builtin-face)))
 `(dired-directory                          ((t :inherit font-lock-keyword-face)))
 `(dired-header                             ((t :inherit font-lock-function-name-face)))
 `(dired-ignored                            ((t :inherit font-lock-comment-face)))
 `(dired-marked                             ((t :bold t :italic t)))
 `(dired-perm-write                         ((t :inherit warning)))
 `(dired-symlink                            ((t :inherit font-lock-type-face)))
 `(easy-escape-delimiter-face               ((t :inherit font-lock-keyword-face)))
 `(easy-escape-face                         ((t :inherit easy-escape-delimiter-face)))
 `(ediff-current-diff-B                     ((t :inherit ediff-current-diff-A)))
 `(ediff-even-diff-B                        ((t :inherit ediff-even-diff-A)))
 `(ediff-fine-diff-B                        ((t :inherit ediff-fine-diff-A)))
 `(ediff-odd-diff-B                         ((t :inherit ediff-odd-diff-A)))
 `(font-lock-comment-delimiter-face         ((t :inherit font-lock-comment-face)))
 `(font-lock-doc-string-face                ((t :inherit font-lock-doc-face)))
 `(font-lock-regexp-grouping-backslash      ((t :inherit font-lock-regexp-grouping-construct)))
 `(font-lock-regexp-grouping-construct      ((t :bold t)))
 `(font-lock-warning-face                   ((t :inherit warning)))
 `(git-timemachine-minibuffer-author-face   ((t :inherit font-lock-keyword-face)))
 `(git-timemachine-minibuffer-detail-face   ((t :inherit font-lock-string-face)))
 `(highlight                                ((t :inherit region)))
 `(highlight-indent-guides-character-face   ((t :inherit font-lock-comment-face :italic nil)))
 `(highlight-indent-guides-even-face        ((t :inherit highlight-indent-guides-character-face)))
 `(highlight-indent-guides-odd-face         ((t :inherit highlight-indent-guides-character-face)))
 `(hl-line                                  ((t :inherit nil :underline t)))
 `(icomplete-first-match                    ((t :inherit font-lock-string-face :bold t)))
 `(ido-subdir                               ((t :inherit font-lock-keyword-face)))
 `(ido-vertical-first-match-face            ((t :inherit default :bold t)))
 `(ido-vertical-only-match-face             ((t :inherit font-lock-variable-name-face :bold t)))
 `(ido-vertical-match-face                  ((t :inherit font-lock-builtin-face :bold t :underline t)))
 `(info-menu-star                           ((t :inherit font-lock-string-face)))
 `(info-title-1                             ((t :inherit outline-1)))
 `(info-title-2                             ((t :inherit outline-2)))
 `(info-title-3                             ((t :inherit outline-3)))
 `(info-title-4                             ((t :inherit outline-4)))
 `(markdown-code-face                       ((t :inherit default :italic t)))
 `(markdown-gfm-checkbox-face               ((t :inherit font-lock-builtin-face :bold t)))
 `(markdown-header-face                     ((t :inherit outline-1 :underline t)))
 `(markdown-header-face-1                   ((t :inherit outline-1 :underline t)))
 `(markdown-header-face-2                   ((t :inherit outline-2 :underline t)))
 `(markdown-header-face-3                   ((t :inherit outline-3 :underline t)))
 `(markdown-header-face-4                   ((t :inherit outline-4 :underline t)))
 `(markdown-header-face-5                   ((t :inherit outline-5 :underline t)))
 `(markdown-header-face-6                   ((t :inherit outline-6 :underline t)))
 `(markdown-list-face                       ((t :inherit default)))
 `(markdown-table-face                      ((t :inherit default)))
 `(minibuffer-prompt                        ((t :inherit font-lock-keyword-face)))
 `(mode-line                                ((t :foreground ,(smf-color 255) :background ,(smf-color 24))))
 `(mode-line-buffer-id                      ((t :foreground ,(smf-color 220))))
 `(mode-line-inactive                       ((t :foreground ,(smf-color 252) :background ,(smf-color 102))))
 `(my-debug-face                            ((t :inherit caution)))
 `(my-ibuffer-read-only-face                ((t :inherit caution)))
 `(my-narrow-face                           ((t :foreground "black" :background ,(smf-color 226))))
 `(my-read-only-face                        ((t :foreground "black" :background ,(smf-color 202))))
 `(org-level-1                              ((t :inherit outline-1))) ;; :underline t)))
 `(region                                   ((t :foreground ,(smf-color 231) :background ,(smf-color 31))))
 `(rg-match-position-face                   ((t :inherit line-number)))
 `(rg-line-number-face                      ((t :inherit line-number)))
 `(rg-toggle-off-face                       ((t nil)))
 `(rg-toggle-on-face                        ((t :inherit warning)))
 `(show-paren-match                         ((t :inherit success)))
 `(show-paren-mismatch                      ((t :inherit error)))
 `(sqlplus-table-head-face                  ((t :foreground ,(smf-color 45) :background "black" :italic t)))
 `(underline                                ((t :underline t)))
 `(verilog-font-lock-grouping-keywords-face ((t :inherit font-lock-keyword-face)))
 `(web-mode-current-element-highlight-face  ((t :bold t :underline t)))
 `(web-mode-error-face                      ((t :inherit error)))
 `(web-mode-function-call-face              ((t :inherit font-lock-function-name-face :underline nil)))
 `(web-mode-html-attr-equal-face            ((t :inherit web-mode-html-attr-name-face)))
 `(web-mode-html-tag-bracket-face           ((t :inherit web-mode-html-tag-face)))
 `(wgrep-delete-face                        ((t :inherit diff-removed)))
 `(wgrep-done-face                          ((t :inherit diff-added)))
 `(wgrep-face                               ((t :inherit diff-changed)))
 `(wgrep-file-face                          ((t nil)))
 `(wgrep-reject-face                        ((t :inherit warning)))
 `(whitespace-empty                         ((t :inherit trailing-whitespace)))
 `(whitespace-tab                           ((t :inherit trailing-whitespace)))
 `(whitespace-trailing                      ((t :inherit trailing-whitespace)))

 )

(provide-theme 'smf-base)
