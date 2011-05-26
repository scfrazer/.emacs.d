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
      (narrow-to-defun))))

;; (defvar my-narrow-header-format "  Narrowed")
;;
;; (defadvice narrow-to-region (after my-narrow-to-region activate)
;;   (when (region-active-p)
;;     (deactivate-mark)
;;     (goto-char (point-min)))
;;   (unless header-line-format
;;     (setq header-line-format my-narrow-header-format)))
;;
;; (defadvice narrow-to-page (after my-narrow-to-page activate)
;;   (unless header-line-format
;;     (setq header-line-format my-narrow-header-format)))
;;
;; (defadvice narrow-to-defun (after my-narrow-to-defun activate)
;;   (unless header-line-format
;;     (setq header-line-format my-narrow-header-format)))
;;
;; (defadvice widen (after my-narrow-widen activate)
;;   (when (equal header-line-format my-narrow-header-format))
;;       (setq header-line-format nil))
;;
;; (ad-remove-advice 'narrow-to-region 'after 'my-narrow-to-region)
;; (ad-remove-advice 'narrow-to-page 'after 'my-narrow-to-page)
;; (ad-remove-advice 'narrow-to-defun 'after 'my-narrow-to-defun)
;; (ad-remove-advice 'widen 'after 'my-narrow-widen)

(provide 'my-narrow)
