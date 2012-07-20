;;; my-clearcase.el

(require 'clearcase)
(require 'cc-status)

(defun my-clearcase-setcs-current ()
  (interactive)
  (clearcase-ct-cleartool-cmd "setcs" "-current")
  (clearcase-ct-cleartool-cmd "setcs" "-current")
  (clearcase-ct-cleartool-cmd "setcs" "-current")
  (message "ct setcs -curr complete"))

(defun my-clearcase-setview (view)
  (interactive "sView: ")
  (clearcase-ct-cleartool-cmd "setview" view)
  (message "ct setview complete"))

(defun clearcase-fprop-viewtag (file)
  "For FILE, return its \"viewtag\" ClearCase property."
  (or (aref (clearcase-fprop-get-properties file) 10)
      clearcase-setview-viewtag))

(defun my-clearcase-list-hist-or-co (&optional arg)
  "List the history or (with prefix arg) checkouts of the current file."
  (interactive "P")
  (if arg
      (my-clearcase-list-checkouts)
    (if (equal major-mode 'dired-mode)
        (clearcase-list-history-dired-file)
      (clearcase-list-history-current-buffer))))

(defun my-clearcase-list-checkouts ()
  "List the checkouts of FILE.
FILE can be a file or a directory. If it is a directory, only the information
on the directory element itself is listed, not on its contents."
  (interactive)
  (let* ((file (if (equal major-mode 'dired-mode) (dired-get-filename) (buffer-file-name)))
         (mtype (clearcase-fprop-mtype file)))
    (if (not (or (eq mtype 'version)
                 (eq mtype 'directory-version)))
        (error "%s is not a ClearCase element" file)
      (message "Listing checkouts...")
      (clearcase-utl-populate-and-view-buffer
       "*clearcase*"
       (list file)
       (function
        (lambda (file)
          (clearcase-ct-do-cleartool-command "lsco" file 'unused (list "-long" (if (eq mtype 'directory-version) "-d" ""))))))
      (message "Listing checkouts...done"))))

(defun my-clearcase-lsprivate ()
  "Find private-view files in current view."
  (interactive)
  (let ((viewtag (clearcase-fprop-viewtag default-directory)))
    (when viewtag
      (let ((ignore (message "Finding view-private files..."))
            (text (clearcase-ct-blocking-call "lsprivate" "-tag" viewtag)))
        (if (zerop (length text))
            (message "No private files found")
          (message "Finding view-private files...done")
          (clearcase-utl-populate-and-view-buffer
           "*clearcase*"
           (list text)
           (function (lambda (s) (insert s))))
          (setq buffer-read-only nil)
          (goto-char (point-min))
          (while (re-search-forward (concat "^/view/" viewtag) nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (point-min))
          (while (re-search-forward "\\[checkedout\\]" nil t)
            (set-text-properties (match-beginning 0) (match-end 0) '(face font-lock-constant-face)))
          (sort-lines nil (point-min) (point-max))
          (fit-window-to-buffer nil (/ (frame-height) 2))
          (goto-char (point-min))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil))))))

(defun my-clearcase-unreserve ()
  "Unreserve current buffer/dired-file."
  (interactive)
  (let ((file (if (equal major-mode 'dired-mode) (dired-get-filename) (buffer-file-name))) view)
    (when (clearcase-fprop-viewtag file)
      (message "Unreserving file...")
      (with-temp-buffer
        (insert (clearcase-ct-blocking-call "lsco" "-l" file))
        (goto-char (point-min))
        (when (re-search-forward "(reserved)" nil t)
          (when (re-search-forward "by view:.+?:\\(.+?\\)\"" nil t)
            (setq view (match-string 1)))))
      (if (not view)
          (message "File is not reserved by anyone")
        (clearcase-ct-blocking-call "unres" "-view" view file)
        (let ((buf (get-file-buffer file)))
          (when buf
            (with-current-buffer buf
              (revert-buffer nil t))))
        (dired-relist-file file)))))

(defun my-clearcase-reserve ()
  "Reserve current buffer/dired-file."
  (interactive)
  (let ((file (if (equal major-mode 'dired-mode) (dired-get-filename) (buffer-file-name))))
    (when (clearcase-fprop-viewtag file)
      (message "Reserving file...")
      (clearcase-ct-blocking-call "reserve" file)
      (let ((buf (get-file-buffer file)))
        (when buf
          (with-current-buffer buf
            (revert-buffer nil t))))
      (dired-relist-file file))))

(defun my-clearcase-backup-set-mode ()
  "Set the mode of backup ClearCase files to the mode of the original."
  (interactive)
  (let ((backup-regexp "\\.keep\\(\\.[0-9]+\\)?$") mode)
    (when (and (buffer-file-name) (string-match backup-regexp (buffer-file-name)))
      (let ((name (replace-regexp-in-string backup-regexp "" (buffer-file-name))))
        (setq mode (let ((case-fold-search t))
                     (assoc-default name auto-mode-alist 'string-match)))
        (when mode
          (set-auto-mode-0 mode))))))

(defun my-clearcase-ediff-current (&optional arg)
  "Do ediff of current buffer/dired-file against latest.
With prefix arg ask for version."
  (interactive "P")
  (call-interactively
   (if (eq major-mode 'dired-mode)
       (if arg
           'clearcase-ediff-named-version-dired-file
         'clearcase-ediff-pred-dired-file)
     (if arg
         'clearcase-ediff-named-version-current-buffer
       'clearcase-ediff-pred-current-buffer))))

(defun my-clearcase-gui-diff-current (&optional arg)
  "Do GUI diff of current buffer/dired-file against latest.
With prefix arg ask for version."
  (interactive "P")
  (call-interactively
   (if (eq major-mode 'dired-mode)
       (if arg
           'clearcase-gui-diff-named-version-dired-file
         'clearcase-gui-diff-pred-dired-file)
     (if arg
         'clearcase-gui-diff-named-version-current-buffer
       'clearcase-gui-diff-pred-current-buffer))))

(add-hook 'find-file-hook 'my-clearcase-backup-set-mode)

(setq clearcase-suppress-checkout-comments t)
(setq clearcase-diff-gui-tool "tkdiff")
(setq clearcase-use-normal-diff t)

(define-key clearcase-mode-map (kbd "C-v") clearcase-prefix-map)
(define-key clearcase-dired-mode-map (kbd "C-v") clearcase-dired-prefix-map)

(define-key clearcase-prefix-map "a" 'clearcase-mkelem-current-buffer)
(define-key clearcase-prefix-map "i" 'clearcase-checkin-current-buffer)
(define-key clearcase-prefix-map "o" 'clearcase-checkout-unreserved-current-buffer)
(define-key clearcase-prefix-map "O" 'clearcase-checkout-current-buffer)
(define-key clearcase-prefix-map "l" 'my-clearcase-list-hist-or-co)
(define-key clearcase-prefix-map "n" 'my-clearcase-unreserve)
(define-key clearcase-prefix-map "p" 'my-clearcase-lsprivate)
(define-key clearcase-prefix-map "r" 'my-clearcase-reserve)
(define-key clearcase-prefix-map "s" 'my-clearcase-setcs-current)
(define-key clearcase-prefix-map "t" 'cc-status-tree)
(define-key clearcase-prefix-map "u" 'clearcase-uncheckout-current-buffer)
(define-key clearcase-prefix-map "U" (lambda() (interactive) (clearcase-uncheckout-current-buffer 'discard)))
(define-key clearcase-prefix-map "v" 'my-clearcase-setview)
(define-key clearcase-prefix-map "=" 'my-clearcase-ediff-current)
(define-key clearcase-prefix-map (kbd "C-=") 'my-clearcase-gui-diff-current)

(define-key clearcase-dired-prefix-map "a" 'clearcase-mkelem-dired-files)
(define-key clearcase-dired-prefix-map "i" 'clearcase-checkin-dired-files)
(define-key clearcase-dired-prefix-map "o" 'clearcase-checkout-unreserved-dired-files)
(define-key clearcase-dired-prefix-map "O" 'clearcase-checkout-dired-files)
(define-key clearcase-dired-prefix-map "l" 'my-clearcase-list-hist-or-co)
(define-key clearcase-dired-prefix-map "n" 'my-clearcase-unreserve)
(define-key clearcase-dired-prefix-map "p" 'my-clearcase-lsprivate)
(define-key clearcase-dired-prefix-map "r" 'my-clearcase-reserve)
(define-key clearcase-dired-prefix-map "s" 'my-clearcase-setcs-current)
(define-key clearcase-dired-prefix-map "t" 'cc-status-tree)
(define-key clearcase-dired-prefix-map "u" 'clearcase-uncheckout-dired-files)
(define-key clearcase-dired-prefix-map "U" (lambda() (interactive) (clearcase-uncheckout-dired-files 'discard)))
(define-key clearcase-dired-prefix-map "v" 'my-clearcase-setview)
(define-key clearcase-dired-prefix-map "=" 'my-clearcase-ediff-current)
(define-key clearcase-dired-prefix-map (kbd "C-=") 'my-clearcase-gui-diff-current)

(define-key clearcase-comment-mode-map (kbd "C-x C-s") 'clearcase-comment-finish)
(define-key clearcase-comment-mode-map (kbd "C-x C-w") 'clearcase-comment-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edcs mode

(defvar clearcase-edcs-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))

    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\* "." table)
    (modify-syntax-entry ?\/ "." table)
    (modify-syntax-entry ?\. "." table)

    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\'  "\"" table)
    (modify-syntax-entry ?\_  "w" table)

    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    table)
  "Syntax table for `clearcase-edcs-mode'.")

; (concat "\\<" (regexp-opt '("include" "element") t) "\\>")
; (concat "\\<" (regexp-opt '("CHECKEDOUT" "LATEST" "INLINE" "DISCARD") t) "\\>")

(defvar clearcase-edcs-mode-font-lock-keywords
  '(
    ("^\\s-*\\<\\(element\\|include\\)\\>"
     (1 font-lock-keyword-face))
    ("\\<\\(CHECKEDOUT\\|DISCARD\\|INLINE\\|LATEST\\)\\>"
     (1 font-lock-builtin-face))
    )
  "Keyword highlighting specification for `clearcase-edcs-mode'.")

(make-variable-buffer-local 'clearcase-parent-buffer)

(define-key clearcase-edcs-mode-map (kbd "C-x C-s") 'clearcase-edcs-finish)
(define-key clearcase-edcs-mode-map (kbd "C-x C-w") 'clearcase-edcs-save)
(define-key clearcase-edcs-mode-map (kbd "C-c C-l") 'my-clearcase-cs-set-latest)
(define-key clearcase-edcs-mode-map (kbd "C-c C-r") 'my-clearcase-cs-set-latest-release)

(defun my-clearcase-cs-set-latest ()
  "Set current line to /main/LATEST."
  (interactive)
  (my-clearcase-cs-fixup-line)
  (if (looking-at "\\s-*element\\s-+\\([^ \t\n]+\\)\\s-+\\([^ \t\n]+\\)")
      (replace-match "/main/LATEST" t t nil 2)
    (when (looking-at "\\s-*include\\s-+\\([^@]+\\)@@\\([^ \t\n]+\\)")
      (replace-match "/main/LATEST" t t nil 2)))
  (beginning-of-line))

(defun my-clearcase-cs-set-latest-release ()
  "Set current line to latest release label."
  (interactive)
  (my-clearcase-cs-fixup-line)
  (let (filename beg end output label)
    (cond ((looking-at "\\s-*element\\s-+\\([^ \t\n]+\\)/\.\.\.\\s-+\\([^ \t\n]+\\)")
           (setq filename (match-string-no-properties 1)
                 beg (match-beginning 2)
                 end (match-end 2)
                 output (shell-command-to-string
                         (concat "cleartool lsh -d -last -fmt '%Nl' " filename))))
          ((looking-at "\\s-*element\\s-+\\([^ \t\n]+\\)\s-+\\([^ \t\n]+\\)")
           (setq filename (match-string-no-properties 1)
                 beg (match-beginning 2)
                 end (match-end 2)
                 output (shell-command-to-string
                         (concat "cleartool lsh -d -last -fmt '%Nl'" filename))))
          ((looking-at "\\s-*include\\s-+\\(\\([^@]+\\)@@\\([^ \t\n]+\\)\\)")
           (setq filename (match-string-no-properties 2)
                 beg (match-beginning 1)
                 end (match-end 1)
                 output (shell-command-to-string
                         (concat "cleartool ls -short " filename))))
          (t
           (error "Couldn't parse current line")))
    (unless (string-match "^\\([^ \t\n]+\\)" output)
      (error "Couldn't find/parse release label"))
    (setq label (match-string-no-properties 1 output))
    (delete-region beg end)
    (goto-char beg)
    (insert label)
    (beginning-of-line)))

(defun my-clearcase-cs-fixup-line ()
  "Turn line into a proper config spec line."
  (interactive)
  (beginning-of-line)
  (when (and (looking-at "^\\s-*$") (fboundp 'my-ido-insert-bookmark-dir))
    (my-ido-insert-bookmark-dir)
    (beginning-of-line))
  (unless (looking-at "\\s-*\\(include\\|element\\|#\\)")
    (delete-region (point) (progn (skip-chars-forward " \t\n") (point)))
    (cond
     ;; Dir/file
     ((looking-at "/vob")
      (end-of-line)
      (delete-region (point) (progn (skip-chars-backward " \t\n") (point)))
      (when (file-directory-p (buffer-substring (point-at-bol) (point-at-eol)))
        (unless (equal (char-before) ?/)
          (insert "/"))
        (insert "..."))
      (insert " /main/LATEST")
      (beginning-of-line)
      (insert "element "))
     ;; ur release label
     ((looking-at "\\([A-Z_]+\\)-\\([A-Z_]+?\\)__")
      (let* ((type (downcase (match-string-no-properties 1)))
             (name (downcase (match-string-no-properties 2)))
             (output (shell-command-to-string
                      (concat "grep '^pkg_dir:' /vob/sse/lib/release/" type "/" name ".urctl")))
             dir)
        (unless (string-match "pkg_dir:\\s-*\\([^ \t\n]+\\)" output)
          (error "Can't figure out package dir"))
        (setq dir (match-string-no-properties 1 output))
        (insert "element " dir "/... ")))
     ;; Checkin comment
     ((looking-at "\\s-*Checked in \"\\(.+?\\)\" version \"\\(.+?\\)\"")
      (let ((filename (match-string-no-properties 1))
            (version (match-string-no-properties 2)))
        (delete-region (point-at-bol) (point-at-eol))
        (insert "element")
        (when (file-directory-p filename)
          (insert " -directory"))
        (insert " " filename " " version)))
     ;; None of the above
     (t
      (error "Can't parse this line")))
    (beginning-of-line)))

(defvar clearcase-cs-mode-abbrev-table nil
  "*Abbrev table in use in ClearCase config spec buffers.")

(define-abbrev-table 'clearcase-cs-mode-abbrev-table ())

(define-abbrev clearcase-cs-mode-abbrev-table
  "lat"
  "/main/LATEST")

(defun clearcase-edcs-mode ()
  (interactive)

  (use-local-map clearcase-edcs-mode-map)
  (setq major-mode 'clearcase-edcs-mode)
  (setq mode-name "ClearCase/edcs")

  (set-syntax-table clearcase-edcs-mode-syntax-table)
  (setq local-abbrev-table clearcase-cs-mode-abbrev-table)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#[ \t]*")

  (set (make-local-variable 'font-lock-defaults) '(clearcase-edcs-mode-font-lock-keywords))
  (turn-on-font-lock)

  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)

  (run-hooks 'text-mode-hook 'clearcase-edcs-mode-hook))

(defun clearcase-cs-mode ()
  (interactive)

  (setq major-mode 'clearcase-cs-mode)
  (setq mode-name "ClearCase/cs")

  (set-syntax-table clearcase-edcs-mode-syntax-table)
  (setq local-abbrev-table clearcase-cs-mode-abbrev-table)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#[ \t]*")

  (local-set-key (kbd "C-c C-r") 'my-clearcase-cs-element-set-latest-rev)

  (set (make-local-variable 'font-lock-defaults) '(clearcase-edcs-mode-font-lock-keywords))
  (turn-on-font-lock)

  (run-hooks 'text-mode-hook))

(add-to-list 'auto-mode-alist '("\\.cs$" . clearcase-cs-mode))
(add-to-list 'auto-mode-alist '("\\.template$" . clearcase-cs-mode))

(provide 'my-clearcase)
