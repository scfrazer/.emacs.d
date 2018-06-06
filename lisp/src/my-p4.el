;;; my-p4.el

(require 'p4)

(defun my-p4-edit (&optional arg)
  "Like `p4-edit', but with C-u ask for a changelist to open in."
  (interactive "P")
  (let ((p4-open-in-changelist arg))
    (call-interactively 'p4-edit)))

(define-key p4-prefix-map "e" 'my-p4-edit)

(defun my-p4-opened ()
  "Improved p4 opened output."
  (interactive)
  ;; TODO
  )

(provide 'my-p4)
