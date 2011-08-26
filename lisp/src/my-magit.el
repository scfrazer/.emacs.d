;;; my-magit.el

(require 'magit)

(defun my-magit-quit ()
  "Clean up magit buffers when quitting."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match "\\s-*\\*magit" (buffer-name buf))
      (kill-buffer buf))))

(defun my-magit-status ()
  "Don't split window."
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))

(define-key magit-log-edit-mode-map (kbd "C-x C-s") 'magit-log-edit-commit)
(define-key magit-mode-map "q" 'my-magit-quit)

(provide 'my-magit)
