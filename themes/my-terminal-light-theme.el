(deftheme my-terminal-light
  "Terminal with light background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-terminal-light

   `(ace-jump-face-foreground ((,class (:background "color-254"))))
   `(bm-face ((,class (:foreground nil :background "color-227"))))
   `(button ((,class (:foreground "color-30"))))
   `(compilation-column-number ((,class (:foreground "color-22"))))
   `(compilation-error ((,class (:foreground "red"))))
   `(compilation-info ((,class (:foreground "color-28"))))
   `(compilation-line-number ((,class (:foreground "color-18"))))
   `(cperl-array-face ((,class (:background nil :foreground "color-62" :weight normal))))
   `(cperl-hash-face ((,class (:background nil :foreground "cyan" :slant normal :weight normal))))
   `(cperl-nonoverridable-face ((,class (:foreground "color-94"))))
   `(cursor ((,class (:background "color-28"))))
   `(custom-group-tag ((,class (:foreground "color-75"))))
   `(custom-variable-tag ((,class (:foreground "color-75"))))
   `(diff-added ((,class (:background nil :foreground "green"))))
   `(diff-removed ((,class (:background nil :foreground "red"))))
   `(dired-marked ((,class (:background "color-32" :foreground "color-231"))))
   `(ediff-current-diff-A ((,class (:background "color-179" :foreground "black"))))
   `(ediff-current-diff-B ((,class (:background "color-152" :foreground "black"))))
   `(ediff-even-diff-A ((,class (:background "color-254" :foreground "Black"))))
   `(ediff-even-diff-B ((,class (:background "color-254" :foreground "black"))))
   `(ediff-fine-diff-A ((,class (:background "color-221"))))
   `(ediff-fine-diff-B ((,class (:background "color-81"))))
   `(ediff-odd-diff-A ((,class (:background "color-251" :foreground "black"))))
   `(ediff-odd-diff-B ((,class (:background "color-251" :foreground "Black"))))
   `(error ((,class (:foreground "red"))))
   `(etags-select-highlight-tag-face ((,class (:foreground "color-254" :background "color-25"))))
   `(flymake-errline ((,class (:inherit nil :background "color-224"))))
   `(flymake-warnline ((,class (:inherit nil :background "color-230"))))
   `(font-lock-builtin-face ((,class (:foreground "color-94"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "color-246"))))
   `(font-lock-comment-face ((,class (:foreground "color-246"))))
   `(font-lock-constant-face ((,class (:foreground "color-22"))))
   `(font-lock-doc-face ((,class (:foreground "color-130"))))
   `(font-lock-doc-string-face ((,class (:foreground "color-130"))))
   `(font-lock-function-name-face ((,class (:foreground "color-88"))))
   `(font-lock-keyword-face ((,class (:foreground "color-19"))))
   `(font-lock-preprocessor-face ((,class (:foreground "color-136"))))
   `(font-lock-reference-face ((,class (:foreground "color-209"))))
   `(font-lock-string-face ((,class (:foreground "color-90"))))
   `(font-lock-type-face ((,class (:foreground "color-24"))))
   `(font-lock-variable-name-face ((,class (:foreground "color-29"))))
   `(font-lock-warning-face ((,class (:foreground "red"))))
   `(fringe ((,class (:background "color-254"))))
   `(header-line ((,class (:underline nil :foreground "color-254" :background "color-236"))))
   `(highlight ((,class (:background "color-117" :foreground "black"))))
   `(hl-line ((,class (:inherit nil :background "color-230"))))
   `(ido-first-match ((,class (:weight normal :foreground "black" :background "color-157"))))
   `(ido-only-match ((,class (:weight normal :foreground "color-254" :background "color-28"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "color-39"))))
   `(info-header-xref ((,class (:inherit info-xref :foreground "color-29"))))
   `(info-menu-star ((,class (:foreground "black"))))
   `(info-node ((,class (:foreground "color-39"))))
   `(info-title-1 ((,class (:foreground "color-100"))))
   `(info-title-2 ((,class (:foreground "color-106"))))
   `(info-xref ((,class (:inherit link :foreground "brightblue"))))
   `(isearch ((,class (:background "color-203" :foreground "color-231"))))
   `(isearch-fail ((,class (:background "red" :foreground "color-231"))))
   `(isearch-lazy-highlight-face ((,class (:background "color-94" :foreground "color-231"))))
   `(lazy-highlight ((,class (:background "color-73" :foreground "color-231"))))
   `(link ((,class (:foreground "color-20"))))
   `(link-visited ((,class (:inherit link :foreground "brightmagenta"))))
   `(magit-item-highlight ((,class (:background "color-231"))))
   `(magit-log-author ((,class (:foreground "color-63"))))
   `(magit-log-sha1 ((,class (:foreground "color-130"))))
   `(magit-section-title ((,class (:inherit font-lock-keyword-face))))
   `(match ((,class (:background "color-189"))))
   `(minibuffer-prompt ((,class (:foreground "color-24"))))
   `(mode-line ((,class (:background "white" :foreground "black"))))
   `(mode-line-buffer-id ((,class (:foreground "color-20"))))
   `(mode-line-inactive ((,class (:background "color-248" :foreground "color-238"))))
   `(my-debug-face ((,class (:background "color-208" :foreground "black"))))
   `(my-fixme-face ((,class (:background "red" :foreground "color-231"))))
   `(my-modified-face ((,class (:background "color-124" :foreground "color-254"))))
   `(my-narrow-face ((,class (:background "brightyellow" :foreground "black"))))
   `(my-read-only-face ((,class (:background "color-208" :foreground "black"))))
   `(my-tab-face ((,class (:background "color-224"))))
   `(my-todo-face ((,class (:background "brightyellow" :foreground "black"))))
   `(org-checkbox-statistics-done ((,class (:foreground "color-28"))))
   `(org-checkbox-statistics-todo ((,class (:foreground "color-95"))))
   `(org-date ((,class (:foreground "color-24"))))
   `(org-document-title ((,class (:foreground "color-100"))))
   `(org-special-keyword ((,class (:foreground "color-94"))))
   `(org-tag ((,class (:foreground "color-69"))))
   `(org-todo ((,class (:foreground "color-95"))))
   `(outline-1 ((,class (:inherit font-lock-function-name-face))))
   `(outline-2 ((,class (:foreground "color-33"))))
   `(outline-3 ((,class (:foreground "color-67"))))
   `(outline-4 ((,class (:foreground "color-62"))))
   `(outline-5 ((,class (:foreground "color-39"))))
   `(primary-selection ((,class (:background "color-20"))))
   `(region ((,class (:background "color-152"))))
   `(sh-heredoc ((,class (:foreground "color-136"))))
   `(sh-quoted-exec ((,class (:foreground "color-141"))))
   `(show-paren-match-face ((,class (:background "color-19" :foreground "color-231"))))
   `(show-paren-mismatch-face ((,class (:background "brightred" :foreground "color-231"))))
   `(sqlplus-table-head-face ((,class (:underline nil :foreground "color-254" :background "color-236"))))
   `(sqlplus-table-odd-rows-face ((,class (:background "color-252"))))
   `(trailing-whitespace ((,class (:background "color-195"))))
   `(vertical-border ((,class (:foreground "color-248"))))
   `(warning ((,class (:foreground "color-100"))))

   ))

(provide-theme 'my-terminal-light)
