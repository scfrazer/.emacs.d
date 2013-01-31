;;; my-theme.el

(setq custom-theme-directory "~/.emacs.d/themes")

(defun white ()
  (interactive)
  (load-theme 'whiteboard)
  (set-background-color "white")
  (my-unbold)
  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'whiteboard

     `(button ((,class (:foreground "darkcyan"))))
     `(compilation-error ((,class (:foreground "red"))))
     `(compilation-info ((,class (:foreground "green4"))))
     `(compilation-line-number ((,class (:foreground "blue4"))))
     `(cperl-array-face ((t (:foreground "SlateBlue3" :weight normal))))
     `(cperl-hash-face ((t (:foreground "turquoise3" :slant normal :weight normal))))
     `(default ((,class (:background "#ffffff" :foreground "black"))))
     `(flymake-errline ((,class (:underline "red" :weight normal))))
     `(flymake-warnline ((,class (:underline "magenta" :weight normal))))
     `(font-lock-comment-delimiter-face ((,class (:foreground "gray66"))))
     `(font-lock-comment-face ((,class (:foreground "gray66"))))
     `(hl-line ((,class (:background "#e0efe0"))))
     `(info-header-xref ((,class (:inherit info-xref :foreground "SeaGreen4"))))
     `(info-menu-star ((,class (:foreground "black"))))
     `(info-title-1 ((,class (:foreground "yellow4"))))
     `(info-xref ((,class (:inherit link :foreground "Blue"))))
     `(link ((,class (:foreground "blue3"))))
     `(link-visited ((,class (:inherit link :foreground "magenta3"))))
     `(magit-diff-add ((,class (:foreground "green3"))))
     `(magit-diff-del ((,class (:foreground "red"))))
     `(magit-item-highlight ((,class (:background "gray95"))))
     `(magit-section-title ((,class (:inherit font-lock-keyword-face))))
     `(mode-line-buffer-id ((,class (:foreground "blue3"))))
     `(org-checkbox-statistics-done ((,class (:foreground "Green4"))))
     `(org-checkbox-statistics-todo ((,class (:foreground "HotPink4"))))
     `(org-date ((,class (:foreground "DeepSkyBlue4"))))
     `(org-document-title ((,class (:foreground "gold4"))))
     `(org-special-keyword ((,class (:foreground "DarkOrange4"))))
     `(org-tag ((,class (:foreground "RoyalBlue1"))))
     `(org-todo ((,class (:foreground "Pink4"))))
     `(outline-1 ((t (:inherit font-lock-function-name-face))))
     `(region ((,class (:background "lightblue"))))
     `(task-bmk-face ((,class (:background "gray95"))))
     `(trailing-whitespace ((,class (:background "#f0f0ff"))))

     ))
  (unless window-system
    (dolist (face (list
                   'isearch
                   'isearch-fail
                   'isearch-lazy-highlight-face
                   'lazy-highlight
                   'my-fixme-face
                   'show-paren-match-face
                   'show-paren-mismatch-face))
      (set-face-foreground face "#ffffff"))
    (set-face-background 'flymake-errline "color-224")
    (set-face-background 'flymake-warnline "color-222")
    (set-face-background 'trailing-whitespace "color-195")
    (set-face-background 'hl-line "color-230")))

;; (defun blue ()
;;   (interactive)
;;   (load-theme 'deeper-blue)
;;   (let ((class '((class color) (min-colors 89))))
;;     (custom-theme-set-faces
;;      'deeper-blue
;;      `(cperl-array-face ((,class (:foreground "yellow2"))))
;;      `(cperl-hash-face ((,class (:foreground "coral1"))))
;;      `(cursor ((,class (:background "lawngreen" :foreground "black"))))
;;      `(custom-face-tag ((,class (:family "helv" :height 1.2))))
;;      `(ediff-current-diff-A ((,class (:background "darkslategray"))))
;;      `(ediff-current-diff-B ((,class (:background "darkslategray"))))
;;      `(ediff-even-diff-A ((,class (:background "Grey25"))))
;;      `(ediff-even-diff-B ((,class (:background "Grey25"))))
;;      `(ediff-fine-diff-A ((,class (:background "dodgerblue2" :foreground "white"))))
;;      `(ediff-fine-diff-B ((,class (:background "dodgerblue2" :foreground "white"))))
;;      `(ediff-odd-diff-A ((,class (:background "Grey25"))))
;;      `(ediff-odd-diff-B ((,class (:background "Grey25"))))
;;      `(header-line ((,class (:foreground "cyan2" :background "gray30"))))
;;      `(highlight-changes ((,class (:background "SteelBlue4" :foreground "white"))))
;;      `(highlight-changes-delete ((,class (:background "firebrick"))))
;;      `(hl-line ((,class (:background "#183245"))))
;;      `(magit-diff-add ((,class (:foreground "SeaGreen2"))))
;;      `(magit-diff-del ((,class (:foreground "red"))))
;;      `(magit-item-highlight ((,class (:background "gray25"))))
;;      `(magit-section-title ((,class (:inherit font-lock-keyword-face))))
;;      `(mode-line-buffer-id ((,class (:background nil :foreground "blue3" :bold nil))))
;;      `(my-tab-face ((,class (:background "pink4"))))
;;      `(org-document-title ((,class (:height 1.44 :foreground "cyan3"))))
;;      `(org-hide ((,class (:foreground "#181a26"))))
;;      `(org-table ((,class (:foreground "darkseagreen2"))))
;;      `(org-tag ((,class (:foreground "salmon"))))
;;      `(org-todo ((,class (:foreground "plum2"))))
;;      `(speedbar-button-face ((,class (:foreground nil :inherit font-lock-constant-face))))
;;      `(speedbar-directory-face ((,class (:foreground nil :inherit dired-directory))))
;;      `(speedbar-file-face ((,class (:foreground nil :inherit default))))
;;      `(speedbar-tag-face ((,class (:foreground nil :inherit font-lock-reference-face))))
;;      `(task-bmk-face ((,class (:background "#404040"))))
;;      `(trailing-whitespace ((,class (:background "steelblue4"))))
;;      `(widget-button-pressed-face ((,class (:foreground "red"))))
;;      `(widget-documentation-face ((,class (:foreground "lime green"))))
;;      `(widget-field-face ((,class (:background "dim gray"))))
;;      `(widget-inactive-face ((,class (:foreground "light gray"))))
;;      `(widget-single-line-field-face ((,class (:background "dim gray"))))))
;;   (unless window-system
;;     (set-face-background 'default "color-234")
;;     (set-face-foreground 'font-lock-comment-face "color-244")
;;     (set-face-foreground 'font-lock-comment-delimiter-face "color-244")
;;     (set-face-foreground 'isearch "color-253")
;;     (set-face-background 'isearch "color-166")
;;     (set-face-foreground 'isearch-lazy-highlight-face "color-253")
;;     (set-face-background 'isearch-lazy-highlight-face "color-23")
;;     (set-face-background 'hl-line "color-17")
;;     (dolist (face (list
;;                    'diff-indicator-added
;;                    'diff-indicator-changed
;;                    'diff-indicator-removed
;;                    'dired-marked
;;                    'ediff-fine-diff-A
;;                    'ediff-fine-diff-B
;;                    'highlight-changes
;;                    'isearch
;;                    'isearch-lazy-highlight-face
;;                    'lazy-highlight
;;                    'my-fixme-face
;;                    'show-paren-match-face
;;                    'show-paren-mismatch-face))
;;       (set-face-foreground face "color-231"))))
;;
;; (defalias 'red (lambda () (interactive) (blue) (set-background-color "#261a18")))
;; (defalias 'green (lambda () (interactive) (blue) (set-background-color "#18261a")))

(provide 'my-theme)
