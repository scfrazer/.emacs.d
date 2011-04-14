;;; my-narrow.el

(defun my-narrowed-p ()
  (not (and (= (point-min) 1) (= (1+ (buffer-size)) (point-max)))))

(defun my-narrow-advice ()
  )

(defun my-widen-advice ()
  )

(provide 'my-narrow)
