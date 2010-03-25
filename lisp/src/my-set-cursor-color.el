;;; my-set-cursor-color.el

(defvar my-set-cursor-color "")

(defun my-set-cursor-color-according-to-mode ()
  "Change cursor color according to buffer state."
  (let ((color (if buffer-read-only
                   "yellow2"
                 (if overwrite-mode
                     "red1"
                   "lawngreen"))))
    (unless (string= color my-set-cursor-color)
      (set-cursor-color (setq my-set-cursor-color color)))))

(add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode)

(provide 'my-set-cursor-color)
