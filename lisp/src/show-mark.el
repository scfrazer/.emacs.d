;;; show-mark.el

(defgroup show-mark nil
  "Show the the mark."
  :group 'convenience)

(defface show-mark-face
  '((((type tty))
     (:background "color-154" :foreground "black"))
    (t
     (:background "#afff00" :foreground "black")))
  "Face for showing the mark."
  :group 'show-mark)

(defface show-mark-face-eol
  '((((type tty))
     (:foreground "color-154" :underline t))
    (t
     (:box (:line-width 1 :color "#afff00"))))
  "Face for showing the mark at the end of a line."
  :group 'show-mark)

(defvar show-mark-overlay nil
  "Overlay for showing the mark.")
(make-variable-buffer-local 'show-mark-overlay)

(defun show-mark-update ()
  "Update the show-mark overlay."
  (let ((mark-pos (mark t)))
    (if (not mark-pos)
        (when show-mark-overlay
          (delete-overlay show-mark-overlay))
      (if show-mark-overlay
          (move-overlay show-mark-overlay mark-pos (1+ mark-pos))
        (setq show-mark-overlay (make-overlay mark-pos (1+ mark-pos) nil t)))
      (overlay-put show-mark-overlay 'face
                   (if (save-excursion (goto-char mark-pos) (eolp))
                       'show-mark-face-eol
                     'show-mark-face)))))

(define-minor-mode show-mark-mode
  "Minor mode to show the mark."
  t " mark" nil
  (if show-mark-mode
      (progn
        (show-mark-update)
        (add-hook 'post-command-hook 'show-mark-update nil t))
    (when show-mark-overlay
      (delete-overlay show-mark-overlay))
    (remove-hook 'post-command-hook 'show-mark-update t)))

(define-global-minor-mode global-show-mark-mode show-mark-mode
  (lambda () (unless show-mark-mode (show-mark-mode 1))))

(provide 'show-mark)
