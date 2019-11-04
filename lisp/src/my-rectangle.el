;;; my-rectangle.el

(require 'rect)
(require 'multiple-cursors)

;; (defhydra smerge-hydra
;;   (:color pink :hint nil :post (smerge-auto-leave))
;;   "
;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ^^-----------^^-------------------^^---------------------^^-------
;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ^^           _a_ll                _R_efine
;; ^^           _RET_: current       _E_diff
;; "
;;   ("n" smerge-next)
;;   ("p" smerge-prev)
;;   ("b" smerge-keep-base)
;;   ("u" smerge-keep-upper)
;;   ("l" smerge-keep-lower)
;;   ("a" smerge-keep-all)
;;   ("RET" smerge-keep-current)
;;   ("\C-m" smerge-keep-current)
;;   ("<" smerge-diff-base-upper)
;;   ("=" smerge-diff-upper-lower)
;;   (">" smerge-diff-base-lower)
;;   ("R" smerge-refine)
;;   ("E" smerge-ediff)
;;   ("C" smerge-combine-with-next)
;;   ("r" smerge-resolve)
;;   ("k" smerge-kill-current)
;;   ("q" nil "cancel" :color blue))

(defun my-push-mark ()
  "Wrap `push-mark' in an interactive form."
  (interactive)
  (push-mark))

(defvar rectangle-number-line-counter nil)
(defvar my-rectangle-number-line-format nil)

(defun my-rectangle-number-lines (&optional arg)
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
                my-rectangle-number-line-format format)
          (mc/for-each-cursor-ordered
           (mc/execute-command-for-fake-cursor 'my-rectangle-mc-callback cursor)))
      (delete-extract-rectangle (region-beginning) (region-end))
      (let ((start (mark))
            (end (point)))
        (if (< end start)
            (progn
              (setq rectangle-number-line-counter (+ (count-lines end start) start-at -1))
              (apply-on-rectangle 'my-rectangle-reverse-number-line-callback end start format))
          (setq rectangle-number-line-counter start-at)
          (apply-on-rectangle 'rectangle-number-line-callback start end format))))))

(defun my-rectangle-mc-callback ()
  (interactive)
  (insert (format my-rectangle-number-line-format rectangle-number-line-counter))
  (setq rectangle-number-line-counter (1+ rectangle-number-line-counter)))

(defun my-rectangle-reverse-number-line-callback (start _end format-string)
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

(provide 'my-rectangle)
