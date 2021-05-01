;;; my-fd.el

(require 'fd-dired)

(defun my-fd-current (&optional arg)
  "fd in current directory."
  (interactive "P")
  (let ((dir (if arg (read-directory-name "Run fd in directory: " nil "" t) default-directory))
        (args (read-string "Run fd (with args): " nil 'fd-dired-args-history)))
    (fd-dired dir args)))

(defun my-fd-project ()
  "fd in current project."
  (interactive)
  (when-let ((dir (car (project-roots (project-current)))))
    (let ((args (read-string "Run fd (with args): " nil 'fd-dired-args-history)))
      (fd-dired dir args))))

(provide 'my-fd)
