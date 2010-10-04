;;; cc-status.el

(require 'clearcase)

;; Variables

(defvar cc-status-item-regexp "^[IUXDA ] \\((\\(unreserved\\|reserved  \\))\\|?           \\) \\([-a-zA-Z0-9_./]+\\)$"
  "Item regexp.")

(defvar cc-status-tree-dir-name nil
  "ClearCase status tree directory name.")
(make-variable-buffer-local 'cc-status-tree-dir-name)

;; Functions

(defun cc-status-tree ()
  "Create ClearCase status tree."
  (interactive)
  (let ((buf (get-buffer-create "*cc-status-tree*")))
    (set-buffer buf)
    (unless cc-status-tree-dir-name
      (cc-status-mode-change-dir default-directory)
      (cc-status-tree-mode))
    (switch-to-buffer buf)))

(defun cc-status-tree-refresh ()
  "Refresh the cc-status buffer."
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert cc-status-tree-dir-name ":\n\n")
  (call-process-shell-command (concat
                               "(cleartool lscheckout -me -r -fmt '(%Rf) %n\\n' "
                               cc-status-tree-dir-name
                               " ; cleartool lspri -other "
                               cc-status-tree-dir-name ")") nil t)
  (goto-char (point-min))
  (forward-line 2)
  (while (not (eobp))
    (insert "  ")
    (cond ((looking-at "(reserved)")
           (forward-sexp)
           (insert "  "))
          ((not (looking-at "(unreserved)"))
           (insert          "?            ")))
    (forward-line 1))
  (goto-char (point-min))
  (forward-line 2)
  (while (re-search-forward (concat cc-status-tree-dir-name "/") nil t)
    (replace-match ""))
  (goto-char (point-min))
  (cc-status-mode-next)
  (sort-regexp-fields nil cc-status-item-regexp "\\3" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun cc-status-mode-open-file ()
  "Open file."
  (interactive)
  (beginning-of-line)
  (when (looking-at cc-status-item-regexp)
    (find-file (concat
                cc-status-tree-dir-name "/"
                (match-string-no-properties 3)))))

(defun cc-status-mode-change-dir (&optional dir)
  "Change directories."
  (interactive)
  (setq dir (or dir cc-status-tree-dir-name))
  (setq cc-status-tree-dir-name
        (replace-regexp-in-string
         "/$" ""
         (read-directory-name "ClearCase status for dir? " dir nil t)))
  (cc-status-tree-refresh))

(defun cc-status-mode-execute ()
  "Execute commands on marked files."
  (interactive)
  (when (y-or-n-p "Operate on marked files? ")
    (goto-char (point-max))
    (let (filename)
      (while (re-search-backward cc-status-item-regexp nil t)
        (setq filename (concat cc-status-tree-dir-name "/"
                               (match-string-no-properties 3)))
        (beginning-of-line)
        (cond ((= (char-after) ?I)
               (clearcase-commented-checkin filename))
              ((= (char-after) ?U)
               (clearcase-uncheckout filename))
              ((= (char-after) ?X)
               (let ((clearcase-keep-uncheckouts nil))
                 (clearcase-uncheckout filename)))
              ((= (char-after) ?A)
               (clearcase-commented-mkelem filename))
              ((= (char-after) ?D)
               (delete-file filename)))))
    (cc-status-tree-refresh)))

(defun cc-status-mode-next ()
  "Go to next file."
  (interactive)
  (end-of-line)
  (re-search-forward cc-status-item-regexp nil t)
  (beginning-of-line))

(defun cc-status-mode-prev ()
  "Go to previous file."
  (interactive)
  (re-search-backward cc-status-item-regexp nil t)
  (beginning-of-line))

(defun cc-status-mode-ediff ()
  "Ediff the current file."
  (interactive)
  (cc-status-mode-open-file)
  (clearcase-ediff-pred-current-buffer))

(defun cc-status-mode-unmark ()
  "Unmark file."
  (interactive)
  (beginning-of-line)
  (unless (looking-at cc-status-item-regexp)
    (error "Must be on a line with a file"))
  (setq buffer-read-only nil)
  (delete-char 1)
  (insert " ")
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (beginning-of-line))

(defun cc-status-mode-mark-for-checkin ()
  "Mark file for checkin."
  (interactive)
  (cc-status-mode-mark t "I"))

(defun cc-status-mode-mark-for-checkout-keep ()
  "Mark file for checkout with keep."
  (interactive)
  (cc-status-mode-mark t "U"))

(defun cc-status-mode-mark-for-checkout-rm ()
  "Mark file for checkout with rm."
  (interactive)
  (cc-status-mode-mark t "X"))

(defun cc-status-mode-mark-for-mkelem ()
  "Mark file for mkelem."
  (interactive)
  (cc-status-mode-mark nil "A"))

(defun cc-status-mode-mark-for-delete ()
  "Mark file for deletion."
  (interactive)
  (cc-status-mode-mark nil "D"))

(defun cc-status-mode-mark (must-be-co mark-letter)
  "Mark a file."
  (beginning-of-line)
  (if must-be-co
      (unless (looking-at ". (")
        (error "Must be on a line with a checked-out file"))
    (when (looking-at ". (")
      (error "Must be on a line with a uncatalogued file")))
  (setq buffer-read-only nil)
  (delete-char 1)
  (insert mark-letter)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (beginning-of-line))

;; Map

(defvar cc-status-tree-mode-map nil
  "`cc-status-mode' keymap.")

(unless cc-status-tree-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" 'bury-buffer)
    (define-key map "g" 'cc-status-tree-refresh)

    (define-key map (kbd "RET") 'cc-status-mode-open-file)
    (define-key map "j" 'cc-status-mode-change-dir)
    (define-key map "x" 'cc-status-mode-execute)

    (define-key map (kbd "<down>") 'cc-status-mode-next)
    (define-key map (kbd "<up>") 'cc-status-mode-prev)
    (define-key map (kbd "C-n") 'cc-status-mode-next)
    (define-key map (kbd "C-p") 'cc-status-mode-prev)
    (define-key map "n" 'cc-status-mode-next)
    (define-key map "p" 'cc-status-mode-prev)

    (define-key map "=" 'cc-status-mode-ediff)

    (define-key map (kbd "SPC") 'cc-status-mode-unmark)

    (define-key map "i" 'cc-status-mode-mark-for-checkin)
    (define-key map "u" 'cc-status-mode-mark-for-checkout-keep)
    (define-key map "U" 'cc-status-mode-mark-for-checkout-rm)

    (define-key map "a" 'cc-status-mode-mark-for-mkelem)
    (define-key map "d" 'cc-status-mode-mark-for-delete)

    (setq cc-status-tree-mode-map map)))

;; Font-lock

(defvar cc-status-mode-font-lock-keywords
  '(
    ("^\\(.+\\):$"
     (1 font-lock-function-name-face))
    ("^[IA].+"
     (0 font-lock-variable-name-face))
    ("^[UXD].+"
     (0 font-lock-warning-face))
    ("(\\(un\\)reserved)"
     (0 font-lock-keyword-face))
    )
  "Keyword highlighting specification for cc-status.")

;; Mode

(defun cc-status-tree-mode ()
  "Major mode for working with ClearCase tree status.

Key Bindings:

\\{cc-status-tree-mode-map}"
  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'cc-status-tree-mode)
  (setq mode-name "cc-status-tree")
  (use-local-map cc-status-tree-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(cc-status-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'cc-status-tree-mode-hook))

(provide 'cc-status)
