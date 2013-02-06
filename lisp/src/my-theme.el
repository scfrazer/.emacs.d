;;; my-theme.el

(setq custom-theme-directory "~/.emacs.d/themes")

(defun white ()
  (interactive)
  (if (display-graphic-p)
      (load-theme 'whiteboard t)
    (load-theme 'my-terminal-light t)))

(defun blue ()
  (interactive)
  (load-theme 'deeper-blue t)
  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'deeper-blue
     `(cperl-array-face ((,class (:foreground "yellow2"))))
     `(cperl-hash-face ((,class (:foreground "coral1"))))
     `(cursor ((,class (:background "lawngreen" :foreground "black"))))
     `(custom-face-tag ((,class (:family "helv" :height 1.2))))
     `(ediff-current-diff-A ((,class (:background "darkslategray"))))
     `(ediff-current-diff-B ((,class (:background "darkslategray"))))
     `(ediff-even-diff-A ((,class (:background "Grey25"))))
     `(ediff-even-diff-B ((,class (:background "Grey25"))))
     `(ediff-fine-diff-A ((,class (:background "dodgerblue2" :foreground "white"))))
     `(ediff-fine-diff-B ((,class (:background "dodgerblue2" :foreground "white"))))
     `(ediff-odd-diff-A ((,class (:background "Grey25"))))
     `(ediff-odd-diff-B ((,class (:background "Grey25"))))
     `(header-line ((,class (:foreground "cyan2" :background "gray30"))))
     `(highlight-changes ((,class (:background "SteelBlue4" :foreground "white"))))
     `(highlight-changes-delete ((,class (:background "firebrick"))))
     `(hl-line ((,class (:background "#183245"))))
     `(magit-diff-add ((,class (:foreground "SeaGreen2"))))
     `(magit-diff-del ((,class (:foreground "red"))))
     `(magit-item-highlight ((,class (:background "gray25"))))
     `(magit-section-title ((,class (:inherit font-lock-keyword-face))))
     `(mode-line-buffer-id ((,class (:background nil :foreground "blue3" :bold nil))))
     `(my-tab-face ((,class (:background "pink4"))))
     `(org-document-title ((,class (:height 1.44 :foreground "cyan3"))))
     `(org-hide ((,class (:foreground "#181a26"))))
     `(org-table ((,class (:foreground "darkseagreen2"))))
     `(org-tag ((,class (:foreground "salmon"))))
     `(org-todo ((,class (:foreground "plum2"))))
     `(speedbar-button-face ((,class (:foreground nil :inherit font-lock-constant-face))))
     `(speedbar-directory-face ((,class (:foreground nil :inherit dired-directory))))
     `(speedbar-file-face ((,class (:foreground nil :inherit default))))
     `(speedbar-tag-face ((,class (:foreground nil :inherit font-lock-reference-face))))
     `(task-bmk-face ((,class (:background "#404040"))))
     `(trailing-whitespace ((,class (:background "steelblue4"))))
     `(widget-button-pressed-face ((,class (:foreground "red"))))
     `(widget-documentation-face ((,class (:foreground "lime green"))))
     `(widget-field-face ((,class (:background "dim gray"))))
     `(widget-inactive-face ((,class (:foreground "light gray"))))
     `(widget-single-line-field-face ((,class (:background "dim gray"))))))
  (unless window-system
    (set-face-background 'default "color-234")
    (set-face-foreground 'font-lock-comment-face "color-244")
    (set-face-foreground 'font-lock-comment-delimiter-face "color-244")
    (set-face-foreground 'isearch "color-253")
    (set-face-background 'isearch "color-166")
    (set-face-foreground 'isearch-lazy-highlight-face "color-253")
    (set-face-background 'isearch-lazy-highlight-face "color-23")
    (set-face-background 'hl-line "color-17")
    (dolist (face (list
                   'diff-indicator-added
                   'diff-indicator-changed
                   'diff-indicator-removed
                   'dired-marked
                   'ediff-fine-diff-A
                   'ediff-fine-diff-B
                   'highlight-changes
                   'isearch
                   'isearch-lazy-highlight-face
                   'lazy-highlight
                   'my-fixme-face
                   'show-paren-match-face
                   'show-paren-mismatch-face))
      (set-face-foreground face "color-231"))))

(provide 'my-theme)
