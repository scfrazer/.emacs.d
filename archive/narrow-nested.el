;;; narrow-nested.el

(defvar narrow-nested-deactivate-region t
  "*Deactive region when narrowing to region.")

(defvar narrow-nested-regions nil)
(make-variable-buffer-local 'narrow-nested-regions)

(defun narrow-nested-save-restriction ()
  "Save the current restriction."
  (when (and (not (equal this-command 'narrow-nested-widen-previous))
             (/= (buffer-size) (- (point-max) (point-min))))
    (push (cons (point-min-marker) (point-max-marker)) narrow-nested-regions)))

(defadvice narrow-to-region (before narrow-nested-region-before-advice activate)
  (narrow-nested-save-restriction))

(defadvice narrow-to-region (after narrow-nested-region-after-advice activate)
  (narrow-nested-turn-off-region))

(defun narrow-nested-turn-off-region ()
  (when (and narrow-nested-deactivate-region (region-active-p))
    (deactivate-mark)
    (goto-char (point-min))))

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

(defun narrow-nested-dwim (&optional arg)
  "narrow-to-region if active or with prefix arg, widen to
previous restriction if already narrowed, or narrow-to-defun."
  (interactive "P")
  (if (or arg (region-active-p))
      (progn
        (narrow-to-region (region-beginning) (region-end))
        (narrow-nested-turn-off-region))
    (if (/= (buffer-size) (- (point-max) (point-min)))
        (narrow-nested-widen-previous)
      (narrow-to-defun))))

(global-set-key (kbd "C-x n u") 'narrow-nested-widen-previous)

(provide 'narrow-nested)
