;;; my-edit.el

;;; Kill line

(defun my-edit-kill-line (&optional arg)
  "Like kill-line, but use `my-edit-join-line-with-next' when at
end-of-line (and it's not a empty line).  Kills region if active."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (or arg (not (eolp)) (bolp))
        (cond ((null arg)
               (kill-line))
              ((= arg 0)
               (kill-region (point) (progn (back-to-indentation) (point))))
              (t
               (kill-line arg)))
      (my-edit-join-line-with-next))))

;;; Join

(defun my-edit-join-line-with-next ()
  "Join current line with next."
  (interactive)
  (delete-indentation t)
  (just-one-space 1))

;;; Yank

(defun my-edit-yank ()
  "Yank and indent."
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))
    (rotate-yank-pointer 1))
  (if (and current-prefix-arg (integerp current-prefix-arg))
      (dotimes (x current-prefix-arg)
        (yank))
    (yank)
    (when (and (not current-prefix-arg)
               (not (member indent-line-function '(indent-relative sh-basic-indent-line)))
               (not (member major-mode '(makefile-mode))))
      (exchange-point-and-mark)
      (indent-region (point) (mark 't))
      (exchange-point-and-mark))))

;;; Scrolling/Paging

(defun my-edit-scroll-down (n)
  "Scroll down without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-max))
      (scroll-up n))
    (forward-line n)
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'next-line)))

(defun my-edit-scroll-up (n)
  "Scroll up without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-min))
      (scroll-down n))
    (forward-line (- 0 n))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'previous-line)))

(defun my-edit-page-down ()
  "Page down and keep column."
  (interactive)
  (let ((col (current-column))
        (lines (- (window-height) 2)))
    (if (pos-visible-in-window-p (point-max))
        (goto-char (point-max))
      (save-excursion
        (goto-char (window-start))
        (forward-line lines)
        (set-window-start (selected-window) (point)))
      (forward-line lines)
      (when (eobp)
        (recenter -1)))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'next-line)))

(defun my-edit-page-up ()
  "Page up and keep column."
  (interactive)
  (let ((col (current-column))
        (lines (- 2 (window-height))))
    (if (pos-visible-in-window-p (point-min))
        (goto-char (point-min))
      (save-excursion
        (goto-char (window-start))
        (forward-line lines)
        (set-window-start (selected-window) (point)))
      (forward-line lines))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'previous-line)))

(provide 'my-edit)
