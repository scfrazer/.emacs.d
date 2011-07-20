;;; my-narrow.el

(require 'narrow-nested)

(defun my-narrow-or-org-edit ()
  "narrow-to-defun/widen or org-edit-special."
  (interactive)
  (if (equal 'major-mode 'org-mode)
      (org-edit-special)
    (if (/= (buffer-size) (- (point-max) (point-min)))
        (progn
          (widen)
          (recenter))
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (narrow-to-defun)))))

(defadvice narrow-to-region (after my-narrow-to-region activate)
  (when (region-active-p)
    (deactivate-mark)
    (goto-char (point-min))))

(provide 'my-narrow)
