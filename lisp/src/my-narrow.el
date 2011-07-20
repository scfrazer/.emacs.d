;;; my-narrow.el

(require 'narrow-nested)

(defun my-narrow-or-org-edit ()
  "narrow-to-defun/widen or org-edit-special."
  (interactive)
  (if (equal 'major-mode 'org-mode)
      (org-edit-special)
    (if (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
      (if (/= (buffer-size) (- (point-max) (point-min)))
          (progn
            (narrow-nested-widen-previous)
            (recenter))
        (narrow-to-defun)))))

(defadvice narrow-to-region (after my-narrow-to-region activate)
  (when (region-active-p)
    (deactivate-mark)
    (goto-char (point-min))))

(provide 'my-narrow)
