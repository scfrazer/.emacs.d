(deftheme my-terminal-dark
  "Terminal with dark background.")

;; (defun blue ()
;;   (interactive)
;;   (load-theme 'deeper-blue t)
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

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-terminal-dark

;;   `(button ((,class (:foreground "color-30")))) ;; Light
;;   `(compilation-column-number ((,class (:foreground "color-22")))) ;; Light
   `(compilation-column-number ((,class (:foreground "color-120"))))
;;   `(compilation-error ((,class (:foreground "red")))) ;; Light
   `(compilation-error ((,class (:foreground "red"))))
;;   `(compilation-info ((,class (:foreground "color-28")))) ;; Light
   `(compilation-info ((,class (:foreground "color-117"))))
;;   `(compilation-line-number ((,class (:foreground "color-18")))) ;; Light
   `(compilation-line-number ((,class (:foreground "color-120"))))
   `(compilation-warning ((,class (:foreground "yellow"))))
;;   `(cperl-array-face ((t (:foreground "color-62" :weight normal)))) ;; Light
   `(cperl-array-face ((,class (:foreground "yellow"))))
;;   `(cperl-hash-face ((t (:foreground "cyan" :slant normal :weight normal)))) ;; Light
   `(cperl-hash-face ((,class (:foreground "color-203"))))
;;   `(cperl-nonoverridable-face ((,class (:foreground "color-170")))) ;; Light
;;   `(cursor ((,class (:background "color-28")))) ;; Light
   `(cursor ((,class (:background "green" :foreground "black"))))
;;   `(custom-group-tag ((,class (:foreground "color-75")))) ;; Light
;;   `(custom-variable-tag ((,class (:foreground "color-75")))) ;; Light
   `(diff-context ((,class (:foreground "color-102"))))
   `(diff-file-header ((,class (:background "color-246"))))
   `(diff-function ((,class (:inherit diff-header))))
   `(diff-header ((,class (:background "color-243"))))
   `(diff-hunk-header ((,class (:inherit diff-header))))
   `(diff-index ((,class (:inherit diff-file-header))))
   `(diff-indicator-added ((,class (:foreground "white" :background "color-58"))))
   `(diff-indicator-changed ((,class (:foreground "white" :background "color-24"))))
   `(diff-indicator-removed ((,class (:foreground "white" :background "color-95"))))
   `(diff-refine-change ((,class (:background "color-60"))))
   `(diff-removed ((,class (nil))))
;;   `(dired-marked ((,class (:background "color-32" :foreground "color-231")))) ;; Light
   `(dired-marked ((,class (:background "color-32" :foreground "white"))))
   `(ediff-current-diff-A ((,class (:background "color-23"))))
   `(ediff-current-diff-B ((,class (:background "color-23"))))
   `(ediff-even-diff-A ((,class (:background "color-238"))))
   `(ediff-even-diff-B ((,class (:background "color-238"))))
   `(ediff-fine-diff-A ((,class (:background "color-33" :foreground "white"))))
   `(ediff-fine-diff-B ((,class (:background "color-33" :foreground "white"))))
   `(ediff-odd-diff-A ((,class (:background "color-238"))))
   `(ediff-odd-diff-B ((,class (:background "color-238"))))
;;   `(flymake-errline ((,class (:background "color-224")))) ;; Light
;;   `(flymake-warnline ((,class (:background "color-230")))) ;; Light
;;   `(font-lock-builtin-face ((,class (:foreground "color-166")))) ;; Light
   `(font-lock-builtin-face ((,class (:foreground "color-210"))))
;;   `(font-lock-comment-delimiter-face ((,class (:foreground "color-248")))) ;; Light
   `(font-lock-comment-delimiter-face ((,class (:foreground "color-244"))))
;;   `(font-lock-comment-face ((,class (:foreground "color-248")))) ;; Light
   `(font-lock-comment-face ((,class (:foreground "color-244"))))
;;   `(font-lock-constant-face ((,class (:foreground "color-65")))) ;; Light
   `(font-lock-constant-face ((,class (:foreground "color-149"))))
;;   `(font-lock-doc-face ((,class (:foreground "color-173")))) ;; Light
   `(font-lock-doc-face ((,class (:foreground "color-223"))))
;;   `(font-lock-doc-string-face ((,class (:foreground "color-173")))) ;; Light
   `(font-lock-doc-string-face ((,class (:foreground "color-223"))))
;;   `(font-lock-function-name-face ((,class (:foreground "color-172")))) ;; Light
   `(font-lock-function-name-face ((,class (:foreground "color-178"))))
;;   `(font-lock-keyword-face ((,class (:foreground "color-33")))) ;; Light
   `(font-lock-keyword-face ((,class (:foreground "color-39"))))
;;   `(font-lock-preprocessor-face ((,class (:foreground "color-178")))) ;; Light
   `(font-lock-preprocessor-face ((,class (:foreground "color-220"))))
;;   `(font-lock-reference-face ((,class (:foreground "color-209")))) ;; Light
   `(font-lock-reference-face ((,class (:foreground "color-210"))))
;;   `(font-lock-string-face ((,class (:foreground "color-95")))) ;; Light
   `(font-lock-string-face ((,class (:foreground "color-180"))))
;;   `(font-lock-type-face ((,class (:foreground "color-24")))) ;; Light
   `(font-lock-type-face ((,class (:foreground "color-123"))))
;;   `(font-lock-variable-name-face ((,class (:foreground "color-29")))) ;; Light
   `(font-lock-variable-name-face ((,class (:foreground "color-84"))))
;;   `(font-lock-warning-face ((,class (:foreground "red")))) ;; Light
   `(font-lock-warning-face ((,class (:foreground "red"))))
;;   `(fringe ((,class (:background "color-254")))) ;; Light
   `(fringe ((,class (:background "black"))))
;;   `(highlight ((,class (:background "color-117" :foreground "black")))) ;; Light
;;   `(highlight ((,class (:background "color-24"))))
;;   `(hl-line ((,class (:background "color-230")))) ;; Light
;;   `(ido-first-match ((,class (:weight normal :foreground "color-166")))) ;; Light
   `(ido-first-match ((,class (:foreground "color-214"))))
;;   `(ido-only-match ((,class (:foreground "color-29")))) ;; Light
   `(ido-only-match ((,class (:foreground "green"))))
;;   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face)))) ;; Light
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
;;   `(info-header-node ((,class (:foreground "color-39")))) ;; Light
   `(info-header-node ((,class (:foreground "color-39"))))
;;   `(info-header-xref ((,class (:inherit info-xref :foreground "color-29")))) ;; Light
   `(info-header-xref ((,class (:foreground "color-84"))))
;;   `(info-menu-star ((,class (:foreground "black")))) ;; Light
;;   `(info-node ((,class (:foreground "color-39")))) ;; Light
   `(info-node ((,class (:foreground "color-39"))))
;;   `(info-title-1 ((,class (:foreground "color-100")))) ;; Light
;;   `(info-title-2 ((,class (:foreground "color-106")))) ;; Light
;;   `(info-xref ((,class (:inherit link :foreground "brightblue")))) ;; Light
   `(info-xref ((,class (:foreground "color-84"))))
;;   `(isearch ((,class (:background "color-203" :foreground "color-231")))) ;; Light
   `(isearch ((,class (:background "color-203" :foreground "white"))))
;;   `(isearch-fail ((,class (:background "red" :foreground "color-231")))) ;; Light
;;   `(isearch-lazy-highlight-face ((,class (:background "color-94" :foreground "color-231")))) ;; Light
;;   `(isearch-lazy-highlight-face ((,class (:background "color-94" :foreground "white"))))
;;   `(lazy-highlight ((,class (:background "color-73" :foreground "color-231")))) ;; Light
;;   `(lazy-highlight ((,class (:background "color-73" :foreground "white"))))
;;   `(link ((,class (:foreground "color-20")))) ;; Light
;;   `(link-visited ((,class (:inherit link :foreground "brightmagenta")))) ;; Light
;;   `(magit-diff-add ((,class (:foreground "green")))) ;; Light
;;   `(magit-diff-del ((,class (:foreground "red")))) ;; Light
;;   `(magit-item-highlight ((,class (:background "color-254")))) ;; Light
;;   `(magit-section-title ((,class (:inherit font-lock-keyword-face)))) ;; Light
;;   `(match ((,class (:background "color-217")))) ;; Light
   `(match ((,class (:background "color-89"))))
;;   `(minibuffer-prompt ((,class (:foreground "color-24")))) ;; Light
   `(minibuffer-prompt ((,class (:foreground "color-123"))))
;;   `(mode-line ((,class (:background "white" :foreground "black")))) ;; Light
   `(mode-line ((,class (:background "color-250" :foreground "black"))))
;;   `(mode-line-buffer-id ((,class (:foreground "color-20")))) ;; Light
   `(mode-line-buffer-id ((,class (:background nil :foreground "color-20"))))
;;   `(mode-line-inactive ((,class (:background "color-248" :foreground "color-238")))) ;; Light
   `(mode-line-inactive ((,class (:background "color-241" :foreground "black"))))
;;   `(my-debug-face ((,class (:background "color-208" :foreground "black")))) ;; Light
;;   `(my-fixme-face ((,class (:background "red" :foreground "color-231")))) ;; Light
;;   `(my-modified-face ((,class (:background "color-124" :foreground "color-255")))) ;; Light
;;   `(my-narrow-face ((,class (:background "brightyellow" :foreground "black")))) ;; Light
;;   `(my-read-only-face ((,class (:background "color-208" :foreground "black")))) ;; Light
;;   `(my-tab-face ((,class (:background "color-224")))) ;; Light
;;   `(my-todo-face ((,class (:background "brightyellow" :foreground "black")))) ;; Light
;;   `(org-checkbox-statistics-done ((,class (:foreground "color-28")))) ;; Light
;;   `(org-checkbox-statistics-todo ((,class (:foreground "color-95")))) ;; Light
;;   `(org-date ((,class (:foreground "color-24")))) ;; Light
;;   `(org-document-title ((,class (:foreground "color-100")))) ;; Light
;;   `(org-special-keyword ((,class (:foreground "color-94")))) ;; Light
;;   `(org-tag ((,class (:foreground "color-69")))) ;; Light
;;   `(org-todo ((,class (:foreground "color-95")))) ;; Light
;;   `(outline-1 ((t (:inherit font-lock-function-name-face)))) ;; Light
   `(outline-1 ((,class (:foreground "color-117"))))
;;   `(outline-2 ((,class (:foreground "color-33")))) ;; Light
   `(outline-2 ((,class (:foreground "color-123"))))
;;   `(outline-3 ((,class (:foreground "color-67")))) ;; Light
   `(outline-3 ((,class (:foreground "color-189"))))
;;   `(outline-4 ((,class (:foreground "color-62")))) ;; Light
   `(outline-4 ((,class (:foreground "color-45"))))
;;   `(outline-5 ((,class (:foreground "color-39")))) ;; Light
   `(outline-5 ((,class (:foreground "color-122"))))
;;   `(primary-selection ((,class (:background "color-20")))) ;; Light
   `(primary-selection ((,class (:background "color-20"))))
;;   `(region ((,class (:background "color-152")))) ;; Light
   `(region ((,class (:background "color-25"))))
;;   `(show-paren-match-face ((,class (:background "color-33" :foreground "color-231")))) ;; Light
   `(show-paren-match-face ((,class (:background "color-33" :foreground "white"))))
;;   `(show-paren-mismatch-face ((,class (:background "brightred" :foreground "color-231")))) ;; Light
   `(show-paren-mismatch-face ((,class (:background "red" :foreground "white"))))
;;   `(task-bmk-face ((,class (:background "color-254")))) ;; Light
;;   `(trailing-whitespace ((,class (:background "color-195")))) ;; Light
;;   `(warning ((,class (:foreground "color-100")))) ;; Light

   ))

(provide-theme 'my-terminal-dark)
