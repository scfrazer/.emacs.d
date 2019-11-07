;;; my-rect.el

(require 'rect)
(require 'multiple-cursors)

;; TODO -- M-# => number lines in multi-cursor mode

;; n -- next lines matching prev-char
;; N -- next lines matching next-char
;; p -- prev lines matching prev-char
;; P -- prev lines matching next-char
;; e -- multi-cursor edit lines
;; # -- insert number (C-u prefix does magic)
;; k -- kill-rectangle
;; t -- insert text
;; q -- quit

(defhydra my-rect
  (:exit nil :foreign-keys run :hint nil)
  "
^Move^                                ^Action^
^^------------------------------------^^------
_n_ext Next lines matching prev-char  _e_ Multi-cursor edit
_p_rev Prev lines matching prev-char  _SPC_ Push mark

"
  ("SPC" my-push-mark)
  ("n" next-line)
  ("p" previous-line)
  ("e" mc/edit-lines)
  ("q" nil "Quit" :color blue))

(defun my-push-mark ()
  "Wrap `push-mark' in an interactive form."
  (interactive)
  (push-mark))

(defvar rectangle-number-line-counter nil)
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

(defun my-rect-mc-callback ()
  (interactive)
  (insert (format my-rect-number-line-format rectangle-number-line-counter))
  (setq rectangle-number-line-counter (1+ rectangle-number-line-counter)))

(defun my-rect-reverse-number-line-callback (start _end format-string)
  (move-to-column start t)
  (insert (format format-string rectangle-number-line-counter))
  (setq rectangle-number-line-counter
        (1- rectangle-number-line-counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors

(defvar my-mc-mark nil)
(make-variable-buffer-local 'my-mc-mark)

(defvar my-mc-point nil)
(make-variable-buffer-local 'my-mc-point)

(setq-default mc/always-run-for-all t)
(define-key mc/keymap (kbd "RET") 'mc/keyboard-quit)

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

(defun my-mc-save (&optional arg)
  (unless (region-active-p)
    (activate-mark))
  (setq my-mc-mark (mark t))
  (setq my-mc-point (point-marker)))
(advice-add #'mc/edit-lines :before #'my-mc-save)
(advice-add #'my-mc/mark-all-in-region :before #'my-mc-save)

(defun my-mc-restore ()
  (when (and my-mc-point (marker-position my-mc-point))
    (goto-char my-mc-point))
  (when my-mc-mark
    (set-mark my-mc-mark))
  (deactivate-mark))
(add-hook 'multiple-cursors-mode-disabled-hook 'my-mc-restore)

(provide 'my-rect)
