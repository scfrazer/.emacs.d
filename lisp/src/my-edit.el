;;; my-edit.el

;;; Kill line

(defun my-edit-kill-line (&optional arg)
  "Like kill-line, but use `my-edit-join-line-with-next' when at
end-of-line (and it's not a empty line).  Kills region if active.
With C-u prefix arg, delete instead of kill."
  (interactive "*P")
  (if (region-active-p)
      (if (and arg (listp arg))
          (delete-region (region-beginning) (region-end))
        (kill-region (region-beginning) (region-end)))
    (if (or arg (not (eolp)) (bolp))
        (cond ((null arg)
               (kill-line))
              ((and (numberp arg) (= arg 0))
               (kill-region (point) (progn (back-to-indentation) (point))))
              ((and (listp arg))
               (delete-region (point-at-bol) (point-at-bol 2)))
              (t
               (kill-line arg)))
      (my-edit-join-line-with-next))))

;;; Insert lines

(defun my-edit-newline-and-indent (&optional arg)
  "Go to end-of-line first, and if repeated adds a blank line above.
With prefix arg, insert a blank line below if one doesn't exist."
  (interactive "*P")
  (if arg
      (save-excursion
        (forward-line 1)
        (unless (looking-at "\\s-*$")
          (insert "\n")))
    (end-of-line)
    (if (looking-back "^\\s-+" (point-at-bol))
        (save-excursion
          (beginning-of-line)
          (newline))
      (newline-and-indent))))

(defun my-edit-newline-and-indent-above (&optional arg)
  "Like `my-edit-newline-and-indent' but goes up instead of down.
With prefix arg, insert a blank line above if one doesn't exist."
  (interactive "*P")
  (if arg
      (save-excursion
        (forward-line -1)
        (unless (looking-at "\\s-*$")
          (end-of-line)
          (insert "\n")))
    (beginning-of-line)
    (if (looking-at "\\s-+$")
        (save-excursion
          (end-of-line)
          (newline))
      (beginning-of-line)
      (newline)
      (forward-line -1))
    (indent-according-to-mode)))

(defun my-edit-newline-and-indent-around (&optional arg)
  "Newline and indent, with blank lines above and below.
With prefix arg, insert blank lines above and below if they doesn't exist."
  (interactive "*P")
  (if arg
      (progn
        (save-excursion
          (forward-line -1)
          (unless (looking-at "\\s-*$")
            (end-of-line)
            (insert "\n")))
        (save-excursion
          (forward-line 1)
          (unless (looking-at "\\s-*$")
            (insert "\n"))))
    (beginning-of-line)
    (unless (looking-at "\\s-*$")
      (newline)
      (forward-line -1))
    (indent-according-to-mode)
    (save-excursion
      (forward-line)
      (unless (looking-at "\\s-*$")
        (newline)))
    (save-excursion
      (forward-line -1)
      (unless (looking-at "\\s-*$")
        (end-of-line)
        (newline)))))

;;; Join

(defun my-edit-join-line-with-next ()
  "Join current line with next."
  (interactive "*")
  (delete-indentation t)
  (just-one-space 1))

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

(provide 'my-edit)
