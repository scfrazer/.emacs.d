;;; cc-status.el

(require 'clearcase)

;; Status tree

(defvar cc-status-tree-ignore-regexps (list "\\.cmake\\.state"
                                            "rtl/Makefile\\(\\..+\\)?"
                                            "rtl/.+?\\.\\(vlist\\|xpdb\\|args\\|makerule\\)"
                                            "rtl/dump.rdl")
  "*Regexps to ignore in cc-status-tree.")

(defvar cc-status-tree-filter t
  "*Use `cc-status-tree-ignore-regexps' to filter files/dirs.")

(defvar cc-status-tree-dir-name nil
  "ClearCase status tree directory name.")
(make-variable-buffer-local 'cc-status-tree-dir-name)

(defun cc-status-tree ()
  "Create ClearCase status tree."
  (interactive)
  (let ((buf (get-buffer-create "*cc-status-tree*")))
    (set-buffer buf)
    (unless cc-status-tree-dir-name
      (cc-status-tree-change-dir default-directory)
      (cc-status-tree-mode))
    (switch-to-buffer buf)))

(defun cc-status-tree-change-dir (&optional dir)
  "Change directories."
  (interactive)
  (setq dir (or dir cc-status-tree-dir-name))
  (setq cc-status-tree-dir-name
        (replace-regexp-in-string
         "/$" ""
         (read-directory-name "ClearCase status for dir? " dir nil t)))
  (cc-status-tree-refresh))

(defun cc-status-tree-refresh ()
  "Refresh the cc-status tree buffer."
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert cc-status-tree-dir-name ":"
          (if cc-status-tree-filter " (filtered)" "")
          "\n\n")
  (call-process-shell-command (concat
                               "(cleartool lscheckout -me -r -cview -fmt '(%Rf) %n\\n' "
                               cc-status-tree-dir-name
                               " ; cleartool lspri -other "
                               cc-status-tree-dir-name ")") nil t)
  (cc-status-goto-first-file-line)
  (while (not (eobp))
    (if (cc-status-tree-ignore-entry)
        (delete-region (line-beginning-position) (1+ (line-end-position)))
      (insert "  ")
      (cond ((looking-at "(reserved)")
             (forward-sexp)
             (insert "  "))
            ((not (looking-at "(unreserved)"))
             (insert          "?            ")))
      (forward-line 1)))
  (cc-status-goto-first-file-line)
  (while (re-search-forward (concat cc-status-tree-dir-name "/") nil t)
    (replace-match ""))
  (goto-char (point-min))
  (cc-status-mode-next)
  (sort-regexp-fields nil cc-status-item-regexp "\\2" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun cc-status-tree-ignore-entry ()
  "Return t if current line should be ignored."
  (and cc-status-tree-filter
       (catch 'ignore
         (dolist (regexp cc-status-tree-ignore-regexps)
           (when (re-search-forward regexp (line-end-position) t)
             (throw 'ignore t))))))

(defun cc-status-tree-toggle-filter()
  "Toggle showing all entries."
  (interactive)
  (setq cc-status-tree-filter (not cc-status-tree-filter))
  (cc-status-tree-refresh))

;; Status checkouts

(defun cc-status-checkouts ()
  "Create ClearCase status for checkouts."
  (interactive)
  (let ((buf (get-buffer-create "*cc-status-checkouts*")))
    (set-buffer buf)
    (cc-status-checkouts-refresh)
    (cc-status-checkouts-mode)
    (switch-to-buffer buf)))

(defun cc-status-checkouts-refresh ()
  "Refresh the cc-status checkouts buffer."
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Checkouts:\n\n")
  (call-process-shell-command "cleartool lspri -co -short | xargs cleartool describe -fmt '(%Rf) %n\\n'" nil t)
  (cc-status-goto-first-file-line)
  (while (not (eobp))
    (insert "  ")
    (when (looking-at "(reserved)")
      (forward-sexp)
      (insert "  "))
    (forward-line 1))
  (cc-status-goto-first-file-line)
  (goto-char (point-min))
  (cc-status-mode-next)
  (sort-regexp-fields nil cc-status-item-regexp "\\2" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

;; Common

(defvar cc-status-item-regexp "^[IUXDA ] \\((unreserved)\\|(reserved)  \\|?           \\) \\(.+\\)$"
  "Item regexp.")

(defun cc-status-goto-first-file-line ()
  "Go to the first line where a file would be."
  (goto-char (point-min))
  (forward-line 2))

(defun cc-status-mode-open-file ()
  "Open file."
  (interactive)
  (beginning-of-line)
  (when (looking-at cc-status-item-regexp)
    (find-file (concat
                cc-status-tree-dir-name "/" ; TODO
                (match-string-no-properties 2)))))

(defun cc-status-mode-execute ()
  "Execute commands on marked files."
  (interactive)
  (when (y-or-n-p "Operate on marked files? ")
    (cc-status-goto-first-file-line)
    (if (not (save-excursion (re-search-forward "^[IA]" nil t)))
        (cc-status-do-operations)
      (setq cc-status-prev-window-config (current-window-configuration))
      (pop-to-buffer cc-status-comment-buffer-name)
      (cc-status-comment-mode))))

(defun cc-status-do-operations (&optional comment)
  "Do marked operations."
  (setq comment (or comment ""))
  (let (filename)
    (goto-char (point-max))
    (while (re-search-backward cc-status-item-regexp nil t)
      (setq filename (concat cc-status-tree-dir-name "/" ; TODO
                             (match-string-no-properties 2)))
      (beginning-of-line)
      (cond ((= (char-after) ?I)
             (clearcase-commented-checkin filename comment))
            ((= (char-after) ?U)
             (clearcase-uncheckout filename))
            ((= (char-after) ?X)
             (let ((clearcase-keep-uncheckouts nil))
               (clearcase-uncheckout filename)))
            ((= (char-after) ?A)
             (clearcase-commented-mkelem filename t comment))
            ((= (char-after) ?D)
             (delete-file filename)))))
  (cc-status-tree-refresh))

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
  (let ((truename (clearcase-fprop-truename (dired-get-filename)))) ; TODO
    (clearcase-ediff-file-with-version truename (clearcase-fprop-predecessor-version truename)))

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
  (cc-status-mode-next))

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
  (cc-status-mode-next))

;; Comment operations

(defconst cc-status-comment-buffer-name "*cc-status-comment*")

(defvar cc-status-prev-window-config nil)

(define-derived-mode cc-status-comment-mode text-mode "cc-status-comment-mode"
  "Add comments for checkin or mkelem."
  (local-set-key (kbd "C-x C-s") 'cc-status-comment-mode-finish))

(defun cc-status-comment-mode-finish ()
  "Finish entering comment and do the operations."
  (interactive)
  (let ((comment (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer cc-status-comment-buffer-name)
    (when cc-status-prev-window-config
      (set-window-configuration cc-status-prev-window-config))
    (cc-status-do-operations comment)))

;; Map

(defvar cc-status-tree-mode-map nil
  "`cc-status-mode' keymap.")

(unless cc-status-tree-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" 'bury-buffer)
    (define-key map "g" 'cc-status-tree-refresh)
    (define-key map "f" 'cc-status-tree-toggle-filter)

    (define-key map (kbd "RET") 'cc-status-mode-open-file)
    (define-key map "j" 'cc-status-tree-change-dir)
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
    ("^\\(.+\\):"
     (1 font-lock-function-name-face))
    ("^[IA].+"
     (0 font-lock-variable-name-face))
    ("^[UXD].+"
     (0 font-lock-warning-face))
    ("(\\(un\\)?reserved)"
     (0 font-lock-keyword-face))
    )
  "Keyword highlighting specification for cc-status.")

;; Modes

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

(defun cc-status-checkouts-mode ()
  "Major mode for working with ClearCase checkouts status.

Key Bindings:

\\{cc-status-tree-mode-map}"
  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'cc-status-checkouts-mode)
  (setq mode-name "cc-status-checkouts")
  (use-local-map cc-status-checkout-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(cc-status-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'cc-status-checkout-mode-hook))

(provide 'cc-status)
