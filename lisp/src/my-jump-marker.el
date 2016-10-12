;;; my-jump-marker.el

(defface my-jump-marker-face
  '((t (:background "color-197" :foreground "color-231")))
  "Face for showing the mark."
  :group 'faces)

(defvar my-jump-marker nil)
(defvar my-jump-marker-overlay nil)

(defun my-jump-marker (&optional arg)
  "Set or jump to a temporary marker."
  (interactive "P")
  (if (or arg
          (not my-jump-marker)
          (not (marker-buffer my-jump-marker))
          (not (marker-position my-jump-marker)))
      (progn
        (setq my-jump-marker (point-marker))
        (if my-jump-marker-overlay
            (move-overlay my-jump-marker-overlay (point) (1+ (point)) (current-buffer))
          (setq my-jump-marker-overlay (make-overlay (point) (1+ (point)) nil t)))
        (if (eolp)
            (progn
              (overlay-put my-jump-marker-overlay 'face nil)
              (overlay-put my-jump-marker-overlay 'display (concat (propertize " " 'face 'my-jump-marker-face) "\n")))
          (overlay-put my-jump-marker-overlay 'face 'my-jump-marker-face)
          (overlay-put my-jump-marker-overlay 'display nil))
        (message "Temporary marker set"))
    (switch-to-buffer (marker-buffer my-jump-marker))
    (goto-char (marker-position my-jump-marker))
    (setq my-jump-marker nil)
    (when my-jump-marker-overlay
      (delete-overlay my-jump-marker-overlay))))

(provide 'my-jump-marker)
