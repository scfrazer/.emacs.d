;;; show-mark.el

(defgroup show-mark nil
  "Show the the mark."
  :group 'convenience)

(defface show-mark-face
  '((t :background "color-154" :foreground "black"))
  "Face for showing the mark."
  :group 'show-mark)

(defface show-mark-face-eol
  '((t :foreground "color-154" :underline t))
  "Face for showing the mark at the end of a line."
  :group 'show-mark)

(defvar show-mark-overlay nil
  "Overlay for showing the mark.")
(make-variable-buffer-local 'show-mark-overlay)

(defun show-mark-update ()
  "Update the show-mark overlay."
  (let ((mark-pos (mark t)))
    (when mark-pos
      (if show-mark-overlay
          (move-overlay show-mark-overlay mark-pos (1+ mark-pos))
        (setq show-mark-overlay (make-overlay mark-pos (1+ mark-pos))))
      (overlay-put show-mark-overlay 'face
                   (if (save-excursion (goto-char mark-pos) (eolp))
                       'show-mark-face-eol
                     'show-mark-face)))))

(define-minor-mode show-mark-mode
  "Minor mode to show the mark."
  t " Mark" nil
  (if show-mark-mode
      (progn
        (show-mark-update)
        (add-hook 'post-command-hook 'show-mark-update nil t))
    (when show-mark-overlay
      (delete-overlay show-mark-overlay)
      (setq show-mark-overlay nil
            show-mark-pos nil))
    (remove-hook 'post-command-hook 'show-mark-update t)))

(define-global-minor-mode global-show-mark-mode show-mark-mode
  (lambda () (unless show-mark-mode (show-mark-mode 1))))

(provide 'show-mark)
