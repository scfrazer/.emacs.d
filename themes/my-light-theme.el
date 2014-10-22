(deftheme my-light
  "Terminal with light background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-light

   `(ace-jump-face-foreground ((,class (:foreground "#FFFF00" :background "#000000"))))
   `(bm-face ((,class (:foreground nil :background "#FFFF5F"))))
   `(button ((,class (:foreground "#008787"))))
   `(compilation-column-number ((,class (:foreground "#5F00AF"))))
   `(compilation-error ((,class (:foreground "#CD0000"))))
   `(compilation-info ((,class (:foreground "#008700"))))
   `(compilation-line-number ((,class (:foreground "#000087"))))
   `(cperl-array-face ((,class (:background nil :foreground "#5F5FD7" :weight normal))))
   `(cperl-hash-face ((,class (:background nil :foreground "#00CDCD" :slant normal :weight normal))))
   `(cperl-nonoverridable-face ((,class (:foreground "#875F00"))))
   `(cursor ((,class (:background "#008700"))))
   `(custom-group-tag ((,class (:foreground "#5FAFFF"))))
   `(custom-variable-tag ((,class (:foreground "#5FAFFF"))))
   `(diff-added ((,class (:background nil :foreground "#00CD00"))))
   `(diff-removed ((,class (:background nil :foreground "#CD0000"))))
   `(dired-marked ((,class (:background "#0087D7" :foreground "#FFFFFF"))))
   `(ediff-current-diff-A ((,class (:background "#D7AF5F" :foreground "#000000"))))
   `(ediff-current-diff-B ((,class (:background "#AFD7D7" :foreground "#000000"))))
   `(ediff-even-diff-A ((,class (:background "#E4E4E4" :foreground "#000000"))))
   `(ediff-even-diff-B ((,class (:background "#E4E4E4" :foreground "#000000"))))
   `(ediff-fine-diff-A ((,class (:background "#FFD75F"))))
   `(ediff-fine-diff-B ((,class (:background "#5FD7FF"))))
   `(ediff-odd-diff-A ((,class (:background "#C6C6C6" :foreground "#000000"))))
   `(ediff-odd-diff-B ((,class (:background "#C6C6C6" :foreground "#000000"))))
   `(error ((,class (:foreground "#CD0000"))))
   `(etags-select-highlight-tag-face ((,class (:foreground "#E4E4E4" :background "#005FAF"))))
   `(flymake-errline ((,class (:inherit nil :background "#FFD7D7"))))
   `(flymake-warnline ((,class (:inherit nil :background "#FFFFD7"))))
   `(font-lock-builtin-face ((,class (:foreground "#875F00"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#949494"))))
   `(font-lock-comment-face ((,class (:foreground "#949494"))))
   `(font-lock-constant-face ((,class (:foreground "#005F00"))))
   `(font-lock-doc-face ((,class (:foreground "#808080"))))
   `(font-lock-doc-string-face ((,class (:foreground "#808080"))))
   `(font-lock-function-name-face ((,class (:foreground "#AF5F00"))))
   `(font-lock-keyword-face ((,class (:foreground "#0000AF"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#AF8700"))))
   `(font-lock-reference-face ((,class (:foreground "#FF875F"))))
   `(font-lock-string-face ((,class (:foreground "#870087"))))
   `(font-lock-type-face ((,class (:foreground "#005F87"))))
   `(font-lock-variable-name-face ((,class (:foreground "#00875F"))))
   `(font-lock-warning-face ((,class (:foreground "#CD0000"))))
   `(fringe ((,class (:background "#E4E4E4"))))
   `(header-line ((,class (:underline nil :foreground "#E4E4E4" :background "#303030"))))
   `(highlight ((,class (:background "#87D7FF" :foreground "#000000"))))
   `(hl-line ((,class (:inherit nil :background "#FFFFD7"))))
   `(ido-first-match ((,class (:weight normal :foreground "#000000" :background "#AFFFFF"))))
   `(ido-only-match ((,class (:weight normal :foreground "#000000" :background "#87FF87"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "#00AFFF"))))
   `(info-header-xref ((,class (:inherit info-xref :foreground "#00875F"))))
   `(info-menu-star ((,class (:foreground "#000000"))))
   `(info-node ((,class (:foreground "#00AFFF"))))
   `(info-title-1 ((,class (:foreground "#878700"))))
   `(info-title-2 ((,class (:foreground "#87AF00"))))
   `(info-xref ((,class (:inherit link :foreground "#5C5CFF"))))
   `(isearch ((,class (:background "#0000AF" :foreground "#FFFFFF"))))
   `(isearch-fail ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(isearch-lazy-highlight-face ((,class (:background "#5F87D7" :foreground "#FFFFFF"))))
   `(lazy-highlight ((,class (:background "#5F87D7" :foreground "#FFFFFF"))))
   `(link ((,class (:foreground "#0000D7"))))
   `(link-visited ((,class (:inherit link :foreground "#FF00FF"))))
   `(magit-item-highlight ((,class (:background "#FFFFFF"))))
   `(magit-log-author ((,class (:foreground "#5F5FFF"))))
   `(magit-log-sha1 ((,class (:foreground "#AF5F00"))))
   `(magit-section-title ((,class (:inherit font-lock-keyword-face))))
   `(match ((,class (:background "#AFD7FF"))))
   `(minibuffer-prompt ((,class (:foreground "#005F87"))))
   `(mode-line ((,class (:background "#BCBCBC" :foreground "#000000"))))
   `(mode-line-buffer-id ((,class (:foreground "#0000D7"))))
   `(mode-line-inactive ((,class (:background "#EEEEEE" :foreground "#BCBCBC"))))
   `(my-debug-face ((,class (:background "#FF8700" :foreground "#000000"))))
   `(my-fixme-face ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(my-modified-face ((,class (:background "#AF0000" :foreground "#E4E4E4"))))
   `(my-narrow-face ((,class (:background "#FFFF00" :foreground "#000000"))))
   `(my-read-only-face ((,class (:background "#FF8700" :foreground "#000000"))))
   `(my-tab-face ((,class (:background "#FFD7D7"))))
   `(my-todo-face ((,class (:background "#FFFF00" :foreground "#000000"))))
   `(org-checkbox-statistics-done ((,class (:foreground "#008700"))))
   `(org-checkbox-statistics-todo ((,class (:foreground "#875F5F"))))
   `(org-date ((,class (:foreground "#005F87"))))
   `(org-document-title ((,class (:foreground "#878700"))))
   `(org-special-keyword ((,class (:foreground "#875F00"))))
   `(org-tag ((,class (:foreground "#5F87FF"))))
   `(org-todo ((,class (:foreground "#875F5F"))))
   `(outline-1 ((,class (:inherit font-lock-function-name-face))))
   `(outline-2 ((,class (:foreground "#0087FF"))))
   `(outline-3 ((,class (:foreground "#5F87AF"))))
   `(outline-4 ((,class (:foreground "#5F5FD7"))))
   `(outline-5 ((,class (:foreground "#00AFFF"))))
   `(primary-selection ((,class (:background "#0000D7"))))
   `(region ((,class (:background "#AFD7FF"))))
   `(sh-heredoc ((,class (:foreground "#AF8700"))))
   `(sh-quoted-exec ((,class (:foreground "#AF87FF"))))
   `(show-paren-match-face ((,class (:background "#0000AF" :foreground "#FFFFFF"))))
   `(show-paren-mismatch-face ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(sqlplus-table-head-face ((,class (:underline nil :foreground "#E4E4E4" :background "#303030"))))
   `(sqlplus-table-odd-rows-face ((,class (:background "#D0D0D0"))))
   `(trailing-whitespace ((,class (:background "#D7FFFF"))))
   `(vertical-border ((,class (:foreground "#A8A8A8"))))
   `(warning ((,class (:foreground "#878700"))))

   ))

(provide-theme 'my-light)
