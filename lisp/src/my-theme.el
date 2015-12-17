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

(defun my-theme-misterioso ()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'my-misterioso t))

;; (setq-default frame-background-mode 'dark)
;; (mapc 'frame-set-background-mode (frame-list))

(provide 'my-theme)
