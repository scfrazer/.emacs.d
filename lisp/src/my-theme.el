;;; my-theme.el

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(defun my-theme-light ()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'my-light t))

(defun my-theme-dark ()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'my-dark t))

(provide 'my-theme)
