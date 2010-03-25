;;; edit-list.el

(defvar edit-list-mode-hook nil
  "*Hooks to run when entering edit-list-mode")

(defvar edit-list-mode-item-regexp "^\\(D\\| \\) \\(.+\\)$"
  "Item regexp.")

(defun edit-list-mode-next ()
  "Go to next list item."
  (interactive)
  (beginning-of-line)
  (unless (eobp)
    (when (looking-at edit-list-mode-item-regexp)
      (forward-char))
    (re-search-forward edit-list-mode-item-regexp nil t)
    (beginning-of-line)))

(defun edit-list-mode-prev ()
  "Go to previous list item."
  (interactive)
  (beginning-of-line)
  (re-search-backward edit-list-mode-item-regexp nil t)
  (beginning-of-line))

(defun edit-list-mode-toggle-sort ()
  "Toggle sorting of items."
  (interactive)
  (setq edit-list-mode-sorted (not edit-list-mode-sorted))
  (edit-list-mode-populate))

(defun edit-list-mode-mark-current ()
  "Mark current item for deletion."
  (interactive)
  (edit-list-mode-mark-item)
  (edit-list-mode-next))

(defun edit-list-mode-mark-item ()
  "Mark current item."
  (beginning-of-line)
  (when (looking-at edit-list-mode-item-regexp)
    (setcdr (assoc (match-string-no-properties 2) edit-list-mode-items) t)
    (setq buffer-read-only nil)
    (delete-char 1)
    (insert "D")
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun edit-list-mode-mark-regexp (regexp)
  "Mark items for deletion using a regexp"
  (interactive "sMark to delete by regexp: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward edit-list-mode-item-regexp nil t)
      (let ((item (match-string-no-properties 2)))
        (when (string-match regexp item)
          (edit-list-mode-mark-item)))
      (end-of-line))))

(defun edit-list-mode-unmark-current ()
  "Unmark current item for deletion."
  (interactive)
  (beginning-of-line)
  (when (looking-at edit-list-mode-item-regexp)
    (setcdr (assoc (match-string-no-properties 2) edit-list-mode-items) nil)
    (setq buffer-read-only nil)
    (delete-char 1)
    (insert " ")
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (beginning-of-line)))

(defun edit-list-mode-unmark-all ()
  "Unmark all items for deletion"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (while (re-search-forward edit-list-mode-item-regexp nil t)
      (edit-list-mode-unmark-current)
      (end-of-line))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun edit-list-mode-execute ()
  "Send a list of remaining items and deleted items to the callback."
  (interactive)
  (let ((callback edit-list-mode-callback)
        remaining-items deleted-items)
    (dolist (item edit-list-mode-items)
      (add-to-list (if (cdr item) 'deleted-items 'remaining-items) (car item) t))
    (edit-list-mode-quit)
    (funcall callback remaining-items deleted-items)))

(defun edit-list-mode-quit ()
  "Quit and kill the current edit-list buffer."
  (interactive)
  (kill-buffer))

(defun edit-list-mode-populate ()
  "Populate the edit-list-mode buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "% " edit-list-mode-description "\n")
  (insert "- ")
  (insert-char ?- (length edit-list-mode-description))
  (insert "\n")
  (dolist (item edit-list-mode-items)
    (insert (if (cdr item) "D" " ") " " (car item) "\n"))
  (goto-char (point-min))
  (edit-list-mode-next)
  (when edit-list-mode-sorted
    (sort-regexp-fields nil edit-list-mode-item-regexp "\\2" (point) (point-max)))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map

(defvar edit-list-mode-map nil
  "`edit-list-mode' keymap.")

(if (not edit-list-mode-map)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>") 'edit-list-mode-next)
      (define-key map (kbd "<up>") 'edit-list-mode-prev)
      (define-key map "n" 'edit-list-mode-next)
      (define-key map "p" 'edit-list-mode-prev)
      (define-key map "s" 'edit-list-mode-toggle-sort)
      (define-key map "d" 'edit-list-mode-mark-current)
      (define-key map "D" 'edit-list-mode-mark-regexp)
      (define-key map "u" 'edit-list-mode-unmark-current)
      (define-key map "U" 'edit-list-mode-unmark-all)
      (define-key map "x" 'edit-list-mode-execute)
      (define-key map "q" 'edit-list-mode-quit)
      (setq edit-list-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

(defvar edit-list-mode-font-lock-keywords
  '(
    ("^% .+$"
     0 font-lock-keyword-face)
    ("^- -+$"
     0 font-lock-keyword-face)
    ("^D.+"
     0 font-lock-warning-face)
    )
  "Keyword highlighting specification for `edit-list-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar edit-list-mode-description nil)
(make-variable-buffer-local 'edit-list-mode-description)

(defvar edit-list-mode-items nil)
(make-variable-buffer-local 'edit-list-mode-items)

(defvar edit-list-mode-callback nil)
(make-variable-buffer-local 'edit-list-mode-callback)

(defvar edit-list-mode-sorted nil)
(make-variable-buffer-local 'edit-list-mode-sorted)

(defun edit-list-of-strings (description items callback)
  "Major mode for editing a generic list of strings."
  ;; Setup
  (switch-to-buffer (concat "*" description "*"))
  (kill-all-local-variables)
  (setq edit-list-mode-description description)
  (dolist (item items)
    (add-to-list 'edit-list-mode-items (cons item nil) t))
  (setq edit-list-mode-callback callback)
  (setq edit-list-mode-sorted nil)
  (setq truncate-lines t)
  (edit-list-mode-populate)
  ;; Mode
  (setq major-mode 'edit-list-mode)
  (setq mode-name "Edit List")
  (use-local-map edit-list-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(edit-list-mode-font-lock-keywords))
  (turn-on-font-lock)
  ;; Done
  (run-hooks 'edit-list-mode-hook))

(provide 'edit-list)
