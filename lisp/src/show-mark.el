;;; show-mark.el

(defgroup show-mark nil
  "Show the the mark."
  :group 'convenience)

(defface show-mark-face
  '((t (:background "#E4E4E4" :foreground "black")))
  "Face for showing the mark."
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
      (if (save-excursion (goto-char mark-pos) (eolp))
          (progn
            (overlay-put show-mark-overlay 'face nil)
            (overlay-put show-mark-overlay 'display (concat (propertize " " 'face 'show-mark-face) "\n")))
        (overlay-put show-mark-overlay 'face 'show-mark-face)
        (overlay-put show-mark-overlay 'display nil)))))

(defun show-mark-after-revert-hook ()
  "Remove the overlay after reverting."
  (when show-mark-overlay
    (save-restriction
      (widen)
      (delete-overlay show-mark-overlay)
      (remove-overlays (point-min) (point-max) 'face 'show-mark-face))))

(add-hook 'after-revert-hook 'show-mark-after-revert-hook)

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
