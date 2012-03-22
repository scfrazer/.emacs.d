;;; my-set-cursor-color.el

(defvar my-set-cursor-color-read-only-color "yellow2"
  "*Cursor color when buffer is read-only.")
(defvar my-set-cursor-color-overwrite-color "red1"
  "*Cursor color when in overwrite mode.")
(defvar my-set-cursor-color-normal-color "lawngreen"
  "*Normal cursor color.")

(defvar my-set-cursor-color "")

(defun my-set-cursor-color-according-to-mode ()
  "Change cursor color according to buffer state."
  (let ((color (if buffer-read-only
                   my-set-cursor-color-read-only-color
                 (if overwrite-mode
                     my-set-cursor-color-overwrite-color
                   my-set-cursor-color-normal-color))))
    (unless (string= color my-set-cursor-color)
      (set-cursor-color (setq my-set-cursor-color color)))))

(add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode)
(add-hook 'find-file-hook 'my-set-cursor-color-according-to-mode)

(provide 'my-set-cursor-color)
