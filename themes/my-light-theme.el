(deftheme my-light
  "Terminal with light background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-light

   `(ac-completion-face ((,class (:foreground "#A8A8A8"))))
   `(avy-lead-face ((,class (:foreground "white" :background "#e52b50" :slant normal))))
   `(avy-lead-face-0 ((,class (:foreground "white" :background "#4f57f9" :slant normal))))
   `(avy-lead-face-1 ((,class (:foreground "white" :background "gray" :slant normal))))
   `(avy-lead-face-2 ((,class (:foreground "white" :background "#f86bf3" :slant normal))))
   `(bm-face ((,class (:foreground nil :background "#FFFF5F"))))
   `(button ((,class (:foreground "#008787"))))
   `(compilation-column-number ((,class (:foreground "#5F00AF"))))
   `(compilation-error ((,class (:foreground "#CD0000" :weight bold))))
   `(compilation-info ((,class (:foreground "#008700"))))
   `(compilation-line-number ((,class (:foreground "#000087"))))
   `(compilation-mode-line-exit ((,class (:background "#00CD00" :foreground "#FFFFFF"))))
   `(compilation-mode-line-fail ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(compilation-mode-line-run ((,class (:background "#AFD7D7" :foreground "#000000"))))
   `(cperl-array-face ((,class (:background nil :foreground "#5F5FD7"))))
   `(cperl-hash-face ((,class (:background nil :foreground "#00CDCD"))))
   `(cperl-nonoverridable-face ((,class (:foreground "#875F00"))))
   `(cursor ((,class (:background "#008700"))))
   `(custom-group-tag ((,class (:foreground "#5FAFFF"))))
   `(custom-variable-tag ((,class (:foreground "#5FAFFF"))))
   `(diff-added ((,class (:background nil :foreground "#00CD00"))))
   `(diff-changed ((,class (:background nil :foreground "#0000FF"))))
   `(diff-refine-added ((,class (:background "#D7FFD7" :foreground "#005F00"))))
   `(diff-refine-change ((,class (:background "#AFD7FF" :foreground "#000087"))))
   `(diff-refine-removed ((,class (:background "#FFD7D7" :foreground "#D70000"))))
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
   `(error ((,class (:foreground "#CD0000" :weight bold))))
   `(escape-glyph ((,class (:foreground "#AF0000" :background "#FFFFD7"))))
   `(etags-select-highlight-tag-face ((,class (:foreground "#E4E4E4" :background "#005FAF"))))
   `(flymake-errline ((,class (:inherit nil :background "#FFD7D7"))))
   `(flymake-warnline ((,class (:inherit nil :background "#FFFFD7"))))
   `(font-lock-builtin-face ((,class (:foreground "#875F00"))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-comment-face ((,class (:foreground "#5F5F87" :slant italic))))
   `(font-lock-constant-face ((,class (:foreground "#005F00"))))
   `(font-lock-doc-face ((,class (:foreground "#808080"))))
   `(font-lock-doc-string-face ((,class (:foreground "#808080"))))
   `(font-lock-function-name-face ((,class (:foreground "#AF5F00"))))
   `(font-lock-keyword-face ((,class (:foreground "#0000AF" :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#AF8700"))))
   `(font-lock-reference-face ((,class (:foreground "#FF875F"))))
   `(font-lock-string-face ((,class (:foreground "#870087"))))
   `(font-lock-type-face ((,class (:foreground "#005F87"))))
   `(font-lock-variable-name-face ((,class (:foreground "#00875F"))))
   `(font-lock-warning-face ((,class (:foreground "#CD0000" :weight bold))))
   `(header-line ((,class (:underline nil :foreground "#E4E4E4" :background "#303030"))))
   `(hi-blue ((,class (:background "#87D7FF" :foreground "#000000"))))
   `(hi-green ((,class (:background "#87FF87" :foreground "#000000"))))
   `(hi-pink ((,class (:background "#FFAFD7" :foreground "#000000"))))
   `(hi-yellow ((,class (:background "#FFD700" :foreground "#000000"))))
   `(highlight ((,class (:background "#87D7FF" :foreground "#000000"))))
   `(hl-line ((,class (:inherit nil :background "#FFFFD7"))))
   `(ido-first-match ((,class (:weight bold :foreground "#000000" :background "#FFFFD7"))))
   `(ido-only-match ((,class (:weight bold :foreground "#000000" :background "#D7FFD7"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "#00CD00"))))
   `(info-header-xref ((,class (:inherit info-xref :foreground "#00AFFF"))))
   `(info-menu-star ((,class (:foreground "#000000"))))
   `(info-node ((,class (:foreground "#00AFFF"))))
   `(info-title-1 ((,class (:foreground "#878700"))))
   `(info-title-2 ((,class (:foreground "#87AF00"))))
   `(info-xref ((,class (:inherit link :foreground "#5C5CFF"))))
   `(isearch ((,class (:background "#FF5F00" :foreground "#FFFFFF"))))
   `(isearch-fail ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(isearch-lazy-highlight-face ((,class (:background "#D7AFAF" :foreground "#000000"))))
   `(lazy-highlight-face ((,class (:background "#D7AFAF" :foreground "#000000"))))
   `(link ((,class (:foreground "#0000D7"))))
   `(link-visited ((,class (:inherit link :foreground "#FF00FF"))))
   `(magit-blame-heading ((,class (:background "#DADADA"))))
   `(magit-branch-local ((,class (:foreground "#5FAFD7"))))
   `(magit-branch-remote ((,class (:foreground "#5F875F"))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-added-highlight ((,class (:inherit diff-refine-added))))
   `(magit-diff-base ((,class (:inherit diff-changed))))
   `(magit-diff-base-highlight ((,class (:inherit diff-refine-change))))
   `(magit-diff-conflict-heading ((,class (:inherit error))))
   `(magit-diff-context ((,class :inherit font-lock-comment-face)))
   `(magit-diff-context-highlight ((,class :background "#DADADA" :foreground "#585858")))
   `(magit-diff-file-heading-highlight ((,class (:inherit diff-file-header))))
   `(magit-diff-file-heading-selection ((,class (:inherit diff-file-header :foreground "#FFAF87"))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((,class (:inherit diff-refine-removed))))
   `(magit-log-date ((,class (:inherit font-lock-comment-face))))
   `(magit-log-graph ((,class (:inherit font-lock-comment-face))))
   `(magit-refname ((,class (:inherit font-lock-comment-face))))
   `(magit-section-heading ((,class (:foreground "#D78700"))))
   `(magit-section-highlight ((,class (:background "#E4E4E4"))))
   `(magit-sequence-head ((,class (:foreground "#87AFD7"))))
   `(magit-sequence-part ((,class (:foreground "#D78700"))))
   `(magit-sequence-stop ((,class (:foreground "#5F875F"))))
   `(magit-tag ((,class (:foreground "#D78700"))))
   `(match ((,class (:background "#FFD7FF"))))
   `(minibuffer-prompt ((,class (:foreground "#005F87"))))
   `(mode-line ((,class (:background "#BCBCBC" :foreground "#000000"))))
   `(mode-line-buffer-id ((,class (:foreground "#0000D7"))))
   `(mode-line-inactive ((,class (:background "#EEEEEE" :foreground "#BCBCBC"))))
   `(my-debug-face ((,class (:background "#FF8700" :foreground "#000000"))))
   `(my-fixme-face ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(my-ibuffer-current-face ((,class (:background nil :foreground "#D700FF"))))
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
   `(show-mark-face ((,class (:background "#E4E4E4"))))
   `(show-paren-match-face ((,class (:background "#AFFFAF" :foreground nil))))
   `(show-paren-mismatch-face ((,class (:background "#CD0000" :foreground "#FFFFFF"))))
   `(sqlplus-table-head-face ((,class (:underline nil :foreground "#E4E4E4" :background "#303030"))))
   `(sqlplus-table-odd-rows-face ((,class (:background "#D0D0D0"))))
   `(trailing-whitespace ((,class (:background "#D7FFFF"))))
   `(vertical-border ((,class (:foreground "#A8A8A8"))))
   `(warning ((,class (:foreground "#878700" :weight bold))))
   `(web-mode-current-element-highlight-face ((,class (:background "#D7FFAF"))))
   `(web-mode-html-attr-name-face ((,class (:foreground "#87AFAF"))))

   ))

(provide-theme 'my-light)
