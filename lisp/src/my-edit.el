;;; my-edit.el

;;; Kill line

(defun my-edit-kill-line (&optional arg)
  "Like kill-line, but use `my-edit-join-line-with-next' when at
end-of-line (and it's not a empty line).  Kills region if active."
  (interactive "*P")
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

;;; Jump-to-char

(defvar my-edit-jump-prev-str nil)
(defvar my-edit-jump-prev-arg nil)

(defun my-edit-jump-to-char (&optional arg)
  "Jump to the next entered char.  To start of word if [a-zA-Z].
Special behavior for ()[]{}<>\"'`."
  (interactive "P")
  (let* ((case-fold-search nil)
         (cmd-keys (this-command-keys))
         (char (read-char (if arg "Jump backward to char:" "Jump to char:")))
         (str (regexp-quote (char-to-string char))))
    (when arg
      (setq cmd-keys (substring cmd-keys 1)))
    (if (string= str cmd-keys)
        (setq str my-edit-jump-prev-str
              arg my-edit-jump-prev-arg)
      (setq my-edit-jump-prev-str str
            my-edit-jump-prev-arg arg))
    (push-mark (point) t)
    (cond ((and (not arg) (member char (list ?\) ?\] ?\} ?\>)))
           (my-edit-jump-forward-pair char))
          ((and arg (member char (list ?\( ?\[ ?\{ ?\<)))
           (my-edit-jump-backward-pair char))
          ((and (not arg) (member char (list ?\" ?\' ?\`)))
           (my-edit-jump-forward-quote char))
          ((and arg (member char (list ?\" ?\' ?\`)))
           (my-edit-jump-backward-quote char))
          (t
           (unless arg
             (forward-char))
           (when (string-match "[a-zA-Z]" str)
             (setq str (concat "\\_<" str)))
           (if arg
               (re-search-backward str (point-at-bol) t)
             (re-search-forward str (point-at-eol) t)
             (backward-char))))))

(defun my-edit-jump-forward-pair (char)
  "Jump forward inside pair."
  (let ((table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (catch 'done
        (condition-case nil
            (while t
              (up-list)
              (when (= (char-before) char)
                (backward-char)
                (throw 'done t)))
          (error nil))
        (goto-char (point-max))))))

(defun my-edit-jump-backward-pair (char)
  "Jump backward inside pair."
  (let ((table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (catch 'done
        (condition-case nil
            (while t
              (backward-up-list)
              (when (= (char-after) char)
                (forward-char)
                (throw 'done t)))
          (error nil))
        (goto-char (point-min))))))

(defun my-edit-jump-forward-quote (char)
  "Jump forward inside quote."
  (let ((regex (char-to-string char))
        done)
    (when (= (char-after) char)
      (forward-char))
    (setq done nil)
    (while (not (or done (eobp)))
      (when (re-search-forward regex nil 'go)
        (backward-char)
        (setq done (not (equal (char-before) ?\\)))
        (forward-char)))
    (unless (eobp)
      (backward-char))))

(defun my-edit-jump-backward-quote (char)
  "Jump backward inside quote."
  (let ((regex (char-to-string char))
        done)
    (when (= (char-before) char)
      (backward-char))
    (setq done nil)
    (while (not (or done (bobp)))
      (when (re-search-backward regex nil 'go)
        (setq done (not (equal (char-before) ?\\)))))
    (unless (bobp)
      (forward-char))))

;;; Join

(defun my-edit-join-line-with-next ()
  "Join current line with next."
  (interactive "*")
  (delete-indentation t)
  (just-one-space 1))

;;; Yank

(defun my-edit-yank ()
  "Like yank, but with prefix number yank that many times."
  (interactive "*")
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
