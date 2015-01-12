(deftheme my-dark
  "Terminal with dark background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-dark

   `(font-lock-comment-delimiter-face ((,class (:foreground "color-245"))))
   `(font-lock-comment-face ((,class (:foreground "color-245"))))
   ))

(provide-theme 'my-dark)
