;;; my-fd.el

(require 'fd-dired)

(defun my-fd-current (&optional arg)
  "fd in current directory.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      ;; TODO
      t
    ;; TODO
    t))

(defun my-fd-project (&optional arg)
  "fd in current project.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      ;; TODO
      t
    ;; TODO
    t))

(provide 'my-fd)
