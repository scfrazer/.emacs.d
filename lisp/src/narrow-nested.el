;;; narrow-nested.el

(defvar narrow-nested-regions nil)
(make-variable-buffer-local 'narrow-nested-regions)

(defun narrow-nested-save-restriction ()
  "Save the current restriction."
  (when (and (not (equal this-command 'narrow-nested-widen-previous))
             (/= (buffer-size) (- (point-max) (point-min))))
    (push (cons (point-min-marker) (point-max-marker)) narrow-nested-regions)))

(defadvice narrow-to-region (before narrow-nested-region-advice activate)
  (narrow-nested-save-restriction))

(defadvice narrow-to-page (before narrow-nested-page-advice activate)
  (narrow-nested-save-restriction))

(defadvice narrow-to-defun (before narrow-nested-defun-advice activate)
  (narrow-nested-save-restriction))

(defadvice widen (after narrow-nested-widen-advice activate)
  (when (called-interactively-p)
    (setq narrow-nested-regions nil)))

(defun narrow-nested-widen-previous ()
  "Widen to the previous narrowing."
  (interactive)
  (if (not narrow-nested-regions)
      (widen)
    (let ((start (marker-position (caar narrow-nested-regions)))
          (end (marker-position (cdar narrow-nested-regions))))
      (setq narrow-nested-regions (cdr narrow-nested-regions))
      (narrow-to-region start end))))

(global-set-key (kbd "C-x n p") 'narrow-nested-widen-previous)

(provide 'narrow-nested)
