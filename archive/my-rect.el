;;; my-rect.el

(setq-default cua-enable-cua-keys nil)

(defun my-rect-toggle ()
  "Toggle CUA rectangle mode"
  (interactive)
  (if cua-mode
      (progn
        (cua-clear-rectangle-mark)
        (setq mark-active nil
              deactivate-mark t)
        (cua-mode -1))
    (cua-mode 1)
    (cua-set-rectangle-mark)
    (setq mark-active t
          deactivate-mark nil)))

(defadvice cua--deactivate-rectangle (after my-rect-turn-off activate)
  "Turn off CUA mode when rectangle is deactivated"
  (when cua-mode
    (cua-mode -1)))

(defadvice cua--init-rectangles (after my-rect-change-keys activate)
  "Change some rectangle mode keys"
  (define-key cua--rectangle-keymap (kbd "<return>") 'cua-clear-rectangle-mark)
  (define-key cua--rectangle-keymap (kbd "<tab>") 'cua-rotate-rectangle))

(unless (assoc 'cua-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(cua-mode " CUA") minor-mode-alist)))

(provide 'my-rect)
