;;; zsh-edit-mode.el

(defun zsh-edit-mode-done ()
  "Save and exit."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-emacs t)))

(define-derived-mode zsh-edit-mode sh-mode "zsh-edit"
  (sh-set-shell "zsh")
  (local-set-key (kbd "C-c C-c") 'zsh-edit-mode-done))

(provide 'zsh-edit-mode)
