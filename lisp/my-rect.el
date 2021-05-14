;;; my-rect.el

(require 'rect)
(require 'multiple-cursors)

;; TODO -- M-# => number lines in multi-cursor mode

(defhydra my-rect
  (:foreign-keys run :hint nil)
  "
^Move^                         ^Action^
^^-----------------------------^^------
_n_ext Next-lines/this-char    _SPC_ Push mark
_p_rev Prev-lines/this-char    _e_ Multi-cursor edit
_N_ext Next-lines/prev-char    _#_ Insert numbers
_P_rev Prev-lines/prev-char    _k_ Kill rectangle
                             _t_ Insert text

"
  ("SPC" my-rect-push-mark)
  ("n" (lambda ()
         (interactive)
         (my-rect-push-mark)
         (my-rect-next-lines t)))
  ("N" (lambda ()
         (interactive)
         (my-rect-push-mark)
         (my-rect-next-lines)))
  ("p" (lambda ()
         (interactive)
         (my-rect-push-mark)
         (my-rect-prev-lines t)))
  ("P" (lambda ()
         (interactive)
         (my-rect-push-mark)
         (my-rect-prev-lines)))
  ("e" my-mc/edit-lines :exit t)
  ("#" my-rect-number-lines :exit t)
  ("k" kill-rectangle :exit t)
  ("t" string-rectangle :exit t)
  ("\C-g" nil :exit t)
  ("q" nil "Quit" :exit t))

(defun my-rect-push-mark ()
  "Wrap `push-mark' in an interactive form."
  (interactive)
  (push-mark))

(defun my-rect-next-lines(&optional look-fwd)
  "Move over next lines looking at backward or forward char."
  (interactive)
  (let* ((col (current-column))
         (look-fwd (or look-fwd (= col 0)))
         (char (if look-fwd (char-after) (char-before)))
         (again t))
    (while again
      (forward-line 1)
      (setq again (not (eobp)))
      (unless (and (= (move-to-column col) col)
                   (equal char (if look-fwd (char-after) (char-before))))
        (forward-line -1)
        (move-to-column col)
        (setq again nil)))))

(defun my-rect-prev-lines (&optional look-fwd)
  "Move over previous lines looking at backward or forward char."
  (interactive)
  (let* ((col (current-column))
         (look-fwd (or look-fwd (= col 0)))
         (char (if look-fwd (char-after) (char-before)))
         (again t))
    (while again
      (forward-line -1)
      (setq again (not (bobp)))
      (unless (and (= (move-to-column col) col)
                   (equal char (if look-fwd (char-after) (char-before))))
        (forward-line 1)
        (move-to-column col)
        (setq again nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number lines

(defvar rectangle-number-line-counter)
(defvar my-rect-number-line-format nil)

(defun my-rect-number-lines (&optional arg)
  "Like `rectangle-number-lines' but with better defaults.

When called with one prefix arg, prompt for starting point.  When
called with a different prefix arg, prompt for starting point and
format."
  (interactive "*P")
  (let ((start-at 0)
        (format "%d"))
    (when arg
      (setq start-at (read-string "Start at: "))
      (if (string-match "[a-zA-Z]" start-at)
          (setq start-at (string-to-char start-at)
                format "%c")
        (setq start-at (string-to-number start-at))
        (unless (equal current-prefix-arg '(4))
          (setq format (read-string "Format string: " "%d")))))
    (if (and (fboundp 'multiple-cursors-mode) multiple-cursors-mode)
        (progn
          (setq rectangle-number-line-counter start-at
                my-rect-number-line-format format)
          (mc/for-each-cursor-ordered
           (mc/execute-command-for-fake-cursor 'my-rect-mc-callback cursor)))
      (delete-extract-rectangle (region-beginning) (region-end))
      (let ((start (mark))
            (end (point)))
        (if (< end start)
            (progn
              (setq rectangle-number-line-counter (+ (count-lines end start) start-at -1))
              (apply-on-rectangle 'my-rect-reverse-number-line-callback end start format))
          (setq rectangle-number-line-counter start-at)
          (apply-on-rectangle 'rectangle-number-line-callback start end format))))))

(defun my-rect-reverse-number-line-callback (start _end format-string)
  (move-to-column start t)
  (insert (format format-string rectangle-number-line-counter))
  (setq rectangle-number-line-counter
        (1- rectangle-number-line-counter)))

(defun my-rect-mc-callback ()
  (interactive)
  (insert (format my-rect-number-line-format rectangle-number-line-counter))
  (setq rectangle-number-line-counter (1+ rectangle-number-line-counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors

(setq-default mc/always-run-for-all t)
(define-key mc/keymap (kbd "RET") 'mc/keyboard-quit)

(defun my-mc/edit-lines ()
  "Like `mc/edit-lines', but without needing mark active."
  (interactive)
  (when mark-active
    (deactivate-mark))
  (mc/remove-fake-cursors)
  (let* ((col (current-column))
         (beg (mark t))
         (end (point))
         (num-lines (- (line-number-at-pos end) (line-number-at-pos beg))))
    (unless (= num-lines 0)
      (save-excursion
        (goto-char beg)
        (mc/create-fake-cursor-at-point)
        (while (not (= num-lines 0))
          (if (< num-lines 0)
              (progn
                (previous-logical-line 1 nil)
                (cl-incf num-lines))
            (next-logical-line 1 nil)
            (cl-decf num-lines))
          (move-to-column col)
          (when (and (not (= num-lines 0))
                     (equal col (current-column)))
            (mc/create-fake-cursor-at-point))))
      (multiple-cursors-mode 1))))

(defun my-mc/mark-all-in-region (beg end &optional search)
  "Find and mark all the parts in the region matching the given search"
  (interactive "r")
  (let ((search (or search (read-from-minibuffer "Mark all in region: ")))
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (while (search-forward search end t)
          (goto-char (match-beginning 0))
          (push-mark)
          (mc/create-fake-cursor-at-point)
          (goto-char (min end (point-at-eol))))
        (let ((first (mc/furthest-cursor-before-point)))
          (if (not first)
              (error "Search failed for %S" search)
            (mc/pop-state-from-overlay first)))
        (if (> (mc/num-cursors) 1)
            (multiple-cursors-mode 1)
          (multiple-cursors-mode 0))))))

(provide 'my-rect)
