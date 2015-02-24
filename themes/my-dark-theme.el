(deftheme my-dark
  "Terminal with dark background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-dark

   `(ace-jump-face-foreground ((,class (:foreground "#FFFFFF" :background "#0087AF"))))
   `(bm-face ((,class (:foreground nil :background "#585858"))))
   `(font-lock-builtin-face ((,class (:foreground "#AF875F"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#808080"))))
   `(font-lock-comment-face ((,class (:foreground "#808080"))))
   `(font-lock-constant-face ((,class (:foreground "#AFD75F"))))
   `(font-lock-doc-face ((,class (:foreground "#FFD7AF"))))
   `(font-lock-doc-string-face ((,class (:foreground "#FFD7AF"))))
   `(font-lock-function-name-face ((,class (:foreground "#D7AF00"))))
   `(font-lock-keyword-face ((,class (:foreground "#00AFFF"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#FFD700"))))
   `(font-lock-reference-face ((,class (:foreground "#FF8787"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "#D75FAF"))))
   `(font-lock-type-face ((,class (:foreground "#87FFFF"))))
   `(font-lock-variable-name-face ((,class (:foreground "#5FFF87"))))
   `(font-lock-warning-face ((,class (:foreground "#CD0000"))))
   `(header-line ((,class (:underline nil :foreground "#E4E4E4" :background "#303030"))))
   `(ido-first-match ((,class (:weight normal :foreground "#CDCD00"))))
   `(ido-only-match ((,class (:weight normal :foreground "#00FF00"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(my-tab-face ((,class (:background "#D787AF"))))
   `(region ((,class (:background "#005FAF"))))
   `(show-mark-face ((,class (:background "#005F5F"))))
   `(show-mark-face-eol ((,class (:underline t :foreground "#005F5F"))))
   `(show-paren-match-face ((,class (:background "#005FFF" :foreground "#FFFFFF"))))
   `(show-paren-mismatch-face ((,class (:background "#AF0000" :foreground "#FFFFFF"))))
   `(trailing-whitespace ((,class (:background "#5F5F87"))))

   ))

(provide-theme 'my-dark)
