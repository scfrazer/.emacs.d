;;; my-theme.el

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(defun my-theme-light ()
  (interactive)
  (if (display-graphic-p)
      (load-theme 'whiteboard t)
    (load-theme 'my-terminal-light t)))

(defun my-theme-dark ()
  (interactive)
  (if (display-graphic-p)
      (load-theme 'deeper-blue t)
    (load-theme 'my-terminal-dark t)))

(provide 'my-theme)
