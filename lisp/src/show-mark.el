;;; show-mark.el

(defgroup show-mark nil
  "Show the the mark."
  :group 'convenience)

(defface show-mark-face
  '((t :background "color-154" :foreground "black"))
  "Face for showing the mark."
  :group 'show-mark)

(defvar show-mark-overlay nil
  "Overlay for showing the mark.")
(make-variable-buffer-local 'show-mark-overlay)

(defvar show-mark-pos nil
  "Position of the show-mark overlay")
(make-variable-buffer-local 'show-mark-pos)

(defun show-mark-update ()
  "Update the show-mark overlay."
  (let ((mark-pos (mark t)))
    (when mark-pos
      (if show-mark-overlay
          (unless (= mark-pos show-mark-pos)
            (move-overlay show-mark-overlay mark-pos (1+ mark-pos)))
        (setq show-mark-overlay (make-overlay mark-pos (1+ mark-pos)))
        (overlay-put show-mark-overlay 'face 'show-mark-face))
      (setq show-mark-pos mark-pos))))

(define-minor-mode show-mark-mode
  "Minor mode to show the mark."
  t " sm" nil
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
