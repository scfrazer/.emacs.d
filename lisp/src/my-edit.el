;;; my-edit.el

;;; Kill line

(defun my-edit-kill-line (&optional arg)
  "Like kill-line, but use `my-edit-join-line-with-next' when at
end-of-line (and it's not a empty line).  Kills region if active."
  (interactive "P")
  (when (and arg (listp arg))
    (append-next-kill)
    (setq arg nil))
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (or arg (not (eolp)) (bolp))
        (cond ((null arg)
               (kill-line))
              ((and (numberp arg) (= arg 0))
               (kill-region (point) (progn (back-to-indentation) (point))))
              (t
               (kill-line arg)))
      (my-edit-join-line-with-next))))

;;; Jump-to-char

(defvar my-edit-jump-prev-str nil)
(defvar my-edit-jump-prev-arg nil)

(defun my-edit-jump-to-char (&optional arg)
  "Jump to the next entered char.  To start of word if [a-zA-Z]."
  (interactive "P")
  (let ((case-fold-search nil)
        (cmd-keys (this-command-keys))
        (str (char-to-string (read-char (if arg "Jump backward to char:" "Jump to char:")))))
    (if (string= str cmd-keys)
        (setq str my-edit-jump-prev-str
              arg my-edit-jump-prev-arg)
      (setq my-edit-jump-prev-str str
            my-edit-jump-prev-arg arg))
    (forward-char)
    (when (string-match "[a-zA-Z]" str)
      (setq str (concat "\\_<" str)))
    (if arg
        (re-search-backward str nil t)
      (when (re-search-forward str nil t)
        (backward-char)))))

;;; Join

(defun my-edit-join-line-with-next ()
  "Join current line with next."
  (interactive)
  (delete-indentation t)
  (just-one-space 1))

;;; Yank

(defun my-edit-yank ()
  "Like yank, but with prefix number yank that many times."
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))
    (rotate-yank-pointer 1))
  (if (and current-prefix-arg (integerp current-prefix-arg))
      (dotimes (x current-prefix-arg)
        (yank))
    (yank)))

(defun my-edit-yank-pop ()
  "Pops the last kill of the ring then does a yank."
  (interactive "*")
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))
  (when kill-ring-yank-pointer
    (setq kill-ring-yank-pointer kill-ring))
  (yank))

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
