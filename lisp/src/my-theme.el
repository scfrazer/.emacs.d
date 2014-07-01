;;; my-theme.el

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(defun my-theme-light ()
  (interactive)
  (load-theme 'my-light t))

(defun my-theme-dark ()
  (interactive)
  (load-theme 'my-dark t))

(provide 'my-theme)
