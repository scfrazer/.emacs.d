(deftheme smf
  "Terminal colors.")

(let ((dark '((class color) (background dark)))
      (light '((class color) (background light))))
  (custom-theme-set-faces
   'smf

   `(avy-lead-face                       ((,dark :foreground "white" :background "color-161" :slant normal :weight bold)))
   `(avy-lead-face-0                     ((,dark :foreground "white" :background "color-25" :slant normal :weight bold)))
   `(avy-lead-face-1                     ((,dark :foreground "white" :background "gray" :slant normal :weight bold)))
   `(avy-lead-face-2                     ((,dark :foreground "white" :background "color-207" :slant normal :weight bold)))
   `(bm-face                             ((,dark :foreground nil :background "color-89")))
   `(compilation-column-number           ((t :inherit font-lock-variable-name-face)))
   `(compilation-error                   ((t :inherit error)))
   `(compilation-info                    ((t :inherit link)))
   `(compilation-line-number             ((t :inherit font-lock-preprocessor-face)))
   `(compilation-mode-line-exit          ((t :inherit success)))
   `(compilation-mode-line-fail          ((t :inherit error)))
   `(compilation-mode-line-run           ((t :inherit warning)))
   `(compilation-warning                 ((t :inherit warning)))
   `(error                               ((,dark :foreground "color-231" :background "color-124")))
   `(font-lock-builtin-face              ((,dark :foreground "color-210")))
   `(font-lock-comment-delimiter-face    ((t :inherit font-lock-comment-face)))
   `(font-lock-comment-face              ((,dark :foreground "color-102" :slant italic) (,light :foreground "color-60" :slant italic)))
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
   `(font-lock-warning-face              ((t :inherit error)))
   `(highlight                           ((t :inherit region)))
   `(ido-first-match                     ((t :inherit warning)))
   `(ido-only-match                      ((t :inherit success)))
   `(ido-subdir                          ((t :inherit font-lock-keyword-face)))
   `(isearch                             ((,dark :foreground "black" :background "color-211")))
   `(js2-error                           ((t :inherit error)))
   `(js2-external-variable               ((t :inherit warning)))
   `(js2-function-param                  ((t :inherit font-lock-variable-name-face)))
   `(lazy-highlight                      ((,dark :foreground "white" :background "color-66")))
   `(link                                ((,dark :foreground "color-81" :underline t)))
   `(link-visited                        ((,dark :foreground "color-212" :underline t)))
   `(minibuffer-prompt                   ((t :inherit font-lock-keyword-face)))
   `(mode-line                           ((,dark :foreground "color-231" :background "color-24")))
   `(mode-line-buffer-id                 ((,dark :foreground "color-220" :weight bold)))
   `(mode-line-inactive                  ((,dark :foreground "color-231" :background "color-102")))
   `(my-tab-face                         ((,dark :background "color-95")))
   `(region                              ((,dark :foreground "color-231" :background "color-31")))
   `(show-mark-face                      ((,dark :foreground nil :background "color-242")))
   `(show-paren-match-face               ((t :inherit success)))
   `(show-paren-mismatch-face            ((t :inherit error)))
   `(success                             ((,dark :foreground "color-231" :background "color-28")))
   `(trailing-whitespace                 ((,dark :background "color-238")))
   `(warning                             ((,dark :foreground "black" :background "color-184")))

   ))

(provide-theme 'smf)

;;    `(button                                  ((,dark (:foreground "color-30"))))
;;    `(compilation-column-number               ((,dark (:foreground "color-55"))))
;;    `(compilation-error                       ((,dark (:foreground "red" :weight bold))))
;;    `(compilation-info                        ((,dark (:foreground "color-28"))))
;;    `(compilation-line-number                 ((,dark (:foreground "color-18"))))
;;    `(compilation-mode-line-exit              ((,dark (:foreground "color-15" :background "green"))))
;;    `(compilation-mode-line-fail              ((,dark (:foreground "color-15" :background "red"))))
;;    `(compilation-mode-line-run               ((,dark (:foreground "black" :background "color-152"))))
;;    `(cperl-array-face                        ((,dark (:foreground "color-62" :background nil))))
;;    `(cperl-hash-face                         ((,dark (:foreground "cyan" :background nil))))
;;    `(cperl-nonoverridable-face               ((,dark (:foreground "color-94"))))
;;    `(custom-group-tag                        ((,dark (:foreground "color-75"))))
;;    `(custom-variable-tag                     ((,dark (:foreground "color-75"))))
;;    `(diff-added                              ((,dark (:foreground "green" :background nil))))
;;    `(diff-changed                            ((,dark (:foreground "color-21" :background nil))))
;;    `(diff-refine-added                       ((,dark (:foreground "color-22" :background "color-194"))))
;;    `(diff-refine-change                      ((,dark (:foreground "color-18" :background "color-153"))))
;;    `(diff-refine-removed                     ((,dark (:foreground "color-160" :background "color-224"))))
;;    `(diff-removed                            ((,dark (:foreground "red" :background nil))))
;;    `(dired-marked                            ((,dark (:foreground "color-15" :background "color-32"))))
;;    `(ediff-current-diff-A                    ((,dark (:foreground "black" :background "color-179"))))
;;    `(ediff-current-diff-B                    ((,dark (:foreground "black" :background "color-152"))))
;;    `(ediff-even-diff-A                       ((,dark (:foreground "black" :background "color-254"))))
;;    `(ediff-even-diff-B                       ((,dark (:foreground "black" :background "color-254"))))
;;    `(ediff-fine-diff-A                       ((,dark (:background "color-221"))))
;;    `(ediff-fine-diff-B                       ((,dark (:background "color-81"))))
;;    `(ediff-odd-diff-A                        ((,dark (:foreground "black" :background "color-251"))))
;;    `(ediff-odd-diff-B                        ((,dark (:foreground "black" :background "color-251"))))
;;    `(escape-glyph                            ((,dark (:foreground "color-124" :background "color-230"))))
;;    `(etags-select-highlight-tag-face         ((,dark (:foreground "color-254" :background "color-25"))))
;;    `(flymake-errline                         ((,dark (:background "color-224" :inherit nil))))
;;    `(flymake-warnline                        ((,dark (:background "color-230" :inherit nil ))))
;;    `(hi-blue                                 ((,dark (:foreground "black" :background "color-117"))))
;;    `(hi-green                                ((,dark (:foreground "black" :background "color-120"))))
;;    `(hi-pink                                 ((,dark (:foreground "black" :background "color-218"))))
;;    `(hi-yellow                               ((,dark (:foreground "black" :background "color-220"))))
;;    `(hl-line                                 ((,dark (:background "color-230" :inherit nil))))
;;    `(info-header-node                        ((,dark (:foreground "green"))))
;;    `(info-header-xref                        ((,dark (:foreground "color-39" :inherit info-xref))))
;;    `(info-menu-star                          ((,dark (:foreground "black"))))
;;    `(info-node                               ((,dark (:foreground "color-39"))))
;;    `(info-title-1                            ((,dark (:foreground "color-100"))))
;;    `(info-title-2                            ((,dark (:foreground "color-106"))))
;;    `(info-xref                               ((,dark (:foreground "color-12" :inherit link))))
;;    `(isearch-fail                            ((,dark (:foreground "color-15" :background "red"))))
;;    `(isearch-lazy-highlight-face             ((,dark (:foreground "black" :background "color-181"))))
;;    `(magit-blame-heading                     ((,dark (:background "color-253"))))
;;    `(magit-branch-local                      ((,dark (:foreground "color-74"))))
;;    `(magit-branch-remote                     ((,dark (:foreground "color-65"))))
;;    `(magit-diff-added                        ((,dark (:inherit diff-added))))
;;    `(magit-diff-added-highlight              ((,dark (:inherit diff-refine-added))))
;;    `(magit-diff-base                         ((,dark (:inherit diff-changed))))
;;    `(magit-diff-base-highlight               ((,dark (:inherit diff-refine-change))))
;;    `(magit-diff-conflict-heading             ((,dark (:inherit error))))
;;    `(magit-diff-context                      ((,dark :inherit font-lock-comment-face)))
;;    `(magit-diff-context-highlight            ((,dark (:foreground "color-240" :background "color-253"))))
;;    `(magit-diff-file-heading-highlight       ((,dark (:inherit diff-file-header))))
;;    `(magit-diff-file-heading-selection       ((,dark (:foreground "color-216" :inherit diff-file-header))))
;;    `(magit-diff-removed                      ((,dark (:inherit diff-removed))))
;;    `(magit-diff-removed-highlight            ((,dark (:inherit diff-refine-removed))))
;;    `(magit-log-date                          ((,dark (:inherit font-lock-comment-face))))
;;    `(magit-log-graph                         ((,dark (:inherit font-lock-comment-face))))
;;    `(magit-refname                           ((,dark (:inherit font-lock-comment-face))))
;;    `(magit-section-heading                   ((,dark (:foreground "color-172"))))
;;    `(magit-section-highlight                 ((,dark (:background "color-254"))))
;;    `(magit-sequence-head                     ((,dark (:foreground "color-110"))))
;;    `(magit-sequence-part                     ((,dark (:foreground "color-172"))))
;;    `(magit-sequence-stop                     ((,dark (:foreground "color-65"))))
;;    `(magit-tag                               ((,dark (:foreground "color-172"))))
;;    `(match                                   ((,dark (:background "color-223"))))
;;    `(my-debug-face                           ((,dark (:foreground "black" :background "color-208"))))
;;    `(my-fixme-face                           ((,dark (:foreground "color-15" :background "red"))))
;;    `(my-ibuffer-current-face                 ((,dark (:foreground "color-165" :background nil))))
;;    `(my-modified-face                        ((,dark (:foreground "color-254" :background "color-124"))))
;;    `(my-narrow-face                          ((,dark (:foreground "black" :background "color-11"))))
;;    `(my-read-only-face                       ((,dark (:foreground "black" :background "color-208"))))
;;    `(my-todo-face                            ((,dark (:foreground "black" :background "color-11"))))
;;    `(org-checkbox-statistics-done            ((,dark (:foreground "color-28"))))
;;    `(org-checkbox-statistics-todo            ((,dark (:foreground "color-95"))))
;;    `(org-date                                ((,dark (:foreground "color-24"))))
;;    `(org-document-title                      ((,dark (:foreground "color-100"))))
;;    `(org-special-keyword                     ((,dark (:foreground "color-94"))))
;;    `(org-tag                                 ((,dark (:foreground "color-69"))))
;;    `(org-todo                                ((,dark (:foreground "color-95"))))
;;    `(outline-1                               ((,dark (:inherit font-lock-function-name-face))))
;;    `(outline-2                               ((,dark (:foreground "color-33"))))
;;    `(outline-3                               ((,dark (:foreground "color-67"))))
;;    `(outline-4                               ((,dark (:foreground "color-62"))))
;;    `(outline-5                               ((,dark (:foreground "color-39"))))
;;    `(primary-selection                       ((,dark (:background "color-20"))))
;;    `(sh-heredoc                              ((,dark (:foreground "color-136"))))
;;    `(sh-quoted-exec                          ((,dark (:foreground "color-141"))))
;;    `(sqlplus-table-head-face                 ((,dark (:foreground "color-254" :background "color-236" :underline nil))))
;;    `(sqlplus-table-odd-rows-face             ((,dark (:background "color-252"))))
;;    `(vertical-border                         ((,dark (:foreground "color-248"))))
;;    `(web-mode-current-element-highlight-face ((,dark (:background "color-193"))))
;;    `(web-mode-html-attr-name-face            ((,dark (:foreground "color-109"))))
