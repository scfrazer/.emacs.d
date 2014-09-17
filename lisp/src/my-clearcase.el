;;; my-clearcase.el

(require 'clearcase)
(require 'cc-status)

(setq clearcase-annotate-fmt-string "| %Sd  %-8.8u  %-10.10Vn | "
      clearcase-diff-gui-tool "tkdiff"
      clearcase-suppress-checkout-comments t
      clearcase-use-normal-diff t)

(defun clearcase-fprop-viewtag (file)
  "For FILE, return its \"viewtag\" ClearCase property."
  (or (aref (clearcase-fprop-get-properties file) 10)
      clearcase-setview-viewtag))

(defvar my-clearcase-update-cs-after-checkin t
  "When non-nil, update config spec after a checkin.")
(defadvice clearcase-commented-checkin (after my-clearcase-commented-checkin activate)
  "Update config spec after checking a file in."
  (when my-clearcase-update-cs-after-checkin
    (let ((file (ad-get-arg 0)))
      (with-current-buffer "*clearcase*"
        (goto-char (point-min))
        (when (re-search-forward (concat "Checked in \"" file "\" version \"\\(.+?\\)\"") nil t)
          (let ((version (match-string-no-properties 1)))
            (message "Updating config spec ...")
            (let ((temp-file (make-temp-file "cc-status-config-spec-")))
              (with-temp-file temp-file
                (call-process-shell-command (concat "cleartool catcs -tag " clearcase-setview-viewtag) nil t)
                (goto-char (point-min))
                (when (re-search-forward "^\\s-*element\\s-+[*]\\s-+CHECKEDOUT.*$" nil t)
                  (forward-line 1)
                  (insert "\nelement " file " " version "\n"))
                (unless (looking-at "^\\s-*$")
                  (insert "\n")))
              (shell-command-to-string (concat "cleartool setcs -tag " clearcase-setview-viewtag " " temp-file))
              (clearcase-fprop-clear-all-properties)
              (delete-file temp-file)
              (message ""))))))))

(defun my-clearcase-list-history (&optional arg)
  "List the history of the current file."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (clearcase-list-history-dired-file)
    (clearcase-list-history-current-buffer)))

(defun my-clearcase-list-history-get-filename ()
  "Get a filename from the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (cond ((looking-at ".+checkout version.+\"\\(.+?\\)\".+?  \\(.+\\)  ")
           (concat "/view/" (match-string-no-properties 2) (match-string-no-properties 1)))
          ((looking-at ".+create version.+\"\\(.+?\\)\"")
           (match-string-no-properties 1))
          (t nil))))

(defun my-clearcase-list-history-get-file ()
  "Get file on current line."
  (interactive)
  (let ((filename (my-clearcase-list-history-get-filename)))
    (unless (file-exists-p filename)
      (error (concat "Couldn't find " filename)))
    (View-quit)
    (find-file filename)))

(defun my-clearcase-list-history-diff ()
  "Diff against file on current line."
  (interactive)
  (let ((filename (my-clearcase-list-history-get-filename)))
    (unless (file-exists-p filename)
      (error (concat "Couldn't find " filename)))
    (View-quit)
    (ediff-files filename (buffer-file-name))))

(defun my-clearcase-list-history-next ()
  "Go to next file."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (when (re-search-forward "^[0-9]+-[0-9]+-[0-9]+" nil t)
      (setq pos (match-beginning 0)))
    (goto-char pos)))

(defun my-clearcase-list-history-previous ()
  "Go to previous file."
  (interactive)
  (let ((pos (point)))
    (when (re-search-backward "^[0-9]+-[0-9]+-[0-9]+" nil t)
      (setq pos (match-beginning 0)))
    (goto-char pos)))

(defadvice clearcase-list-history (after my-clearcase-list-history activate)
  "Colorize and add some extra functions."
  (with-current-buffer "*clearcase*"
    (setq show-trailing-whitespace nil)
    (local-set-key (kbd "C-c C-e") 'my-clearcase-list-history-get-file)
    (local-set-key (kbd "C-c =") 'my-clearcase-list-history-diff)
    (local-set-key (kbd "C-n") 'my-clearcase-list-history-next)
    (local-set-key (kbd "C-p") 'my-clearcase-list-history-previous)
    (setq truncate-lines t)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "^.+checkout version.+$")
          (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
        (unless (looking-at "^[0-9]+-[0-9]+-[0-9]+.+$")
          (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-comment-face)))
      (forward-line 1))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

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
          (clearcase-ct-do-cleartool-command "lsco" file 'unused (list (if (eq mtype 'directory-version) "-d" "")
                                                                       "-areplicas"
                                                                       "-fmt"
                                                                       "\"%Sd %u %e %f (%Rf %Mf) %Tf %[checkout_replica]p\\n\"")))))
      (message "Listing checkouts...done"))))

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

(defun my-clearcase-backup-set-mode ()
  "Set the mode of backup ClearCase files to the mode of the original."
  (interactive)
  (let ((backup-regexp "\\.\\(keep\\|contrib\\)\\(\\.[0-9]+\\)?$") mode)
    (when (and (buffer-file-name) (string-match backup-regexp (buffer-file-name)))
      (let ((name (replace-regexp-in-string backup-regexp "" (buffer-file-name))))
        (setq mode (let ((case-fold-search t))
                     (assoc-default name auto-mode-alist 'string-match)))
        (when mode
          (set-auto-mode-0 mode))))))

(add-hook 'find-file-hook 'my-clearcase-backup-set-mode)

(define-key clearcase-prefix-map "=" 'my-clearcase-ediff-current)
(define-key clearcase-prefix-map "?" nil)
(define-key clearcase-prefix-map "O" 'clearcase-checkout-current-buffer)
(define-key clearcase-prefix-map "R" 'my-clearcase-unreserve)
(define-key clearcase-prefix-map "U" (lambda() "Uncheckout/remove current file." (interactive) (clearcase-uncheckout-current-buffer 'discard)))
(define-key clearcase-prefix-map "a" 'clearcase-annotate-current-buffer)
(define-key clearcase-prefix-map "b" nil)
(define-key clearcase-prefix-map "c" 'my-clearcase-list-checkouts)
(define-key clearcase-prefix-map "g" 'my-clearcase-gui-diff-current)
(define-key clearcase-prefix-map "i" 'clearcase-checkin-current-buffer)
(define-key clearcase-prefix-map "l" 'my-clearcase-list-history)
(define-key clearcase-prefix-map "o" 'clearcase-checkout-unreserved-current-buffer)
(define-key clearcase-prefix-map "r" 'my-clearcase-reserve)
(define-key clearcase-prefix-map "u" 'clearcase-uncheckout-current-buffer)
(define-key clearcase-prefix-map "v" nil)
(define-key clearcase-prefix-map "w" 'clearcase-what-rule-current-buffer)
(define-key clearcase-prefix-map "~" nil)
(define-key clearcase-prefix-map (kbd "RET") 'cc-status)

(define-key clearcase-dired-prefix-map "=" 'my-clearcase-ediff-current)
(define-key clearcase-dired-prefix-map "?" nil)
(define-key clearcase-dired-prefix-map "O" 'clearcase-checkout-dired-files)
(define-key clearcase-dired-prefix-map "R" 'my-clearcase-unreserve)
(define-key clearcase-dired-prefix-map "U" (lambda() (interactive) (clearcase-uncheckout-dired-files 'discard)))
(define-key clearcase-dired-prefix-map "a" 'clearcase-annotate-dired-file)
(define-key clearcase-dired-prefix-map "b" nil)
(define-key clearcase-dired-prefix-map "c" 'my-clearcase-list-checkouts)
(define-key clearcase-dired-prefix-map "g" 'my-clearcase-gui-diff-current)
(define-key clearcase-dired-prefix-map "i" 'clearcase-checkin-dired-files)
(define-key clearcase-dired-prefix-map "l" 'my-clearcase-list-history)
(define-key clearcase-dired-prefix-map "o" 'clearcase-checkout-unreserved-dired-files)
(define-key clearcase-dired-prefix-map "r" 'my-clearcase-reserve)
(define-key clearcase-dired-prefix-map "u" 'clearcase-uncheckout-dired-files)
(define-key clearcase-dired-prefix-map "v" nil)
(define-key clearcase-dired-prefix-map "w" 'clearcase-what-rule-dired-file)
(define-key clearcase-dired-prefix-map "~" nil)
(define-key clearcase-dired-prefix-map (kbd "RET") 'cc-status)

(define-key clearcase-comment-mode-map (kbd "C-x C-s") 'clearcase-comment-finish)
(define-key clearcase-comment-mode-map (kbd "C-x C-w") 'clearcase-comment-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edcs mode

(defun my-clearcase-cs-set-latest-dwim ()
  "Set latest for current line, trying to do-the-right-thing."
  (interactive)
  (beginning-of-line)
  (cond
   ;; Package name
   ((looking-at "\\s-*\\([a-z0-9_]+\\)-\\([a-z0-9_]+\\)\\(:\\([0-9]+\\)\\)?")
    (let* ((dir (downcase (match-string-no-properties 1)))
           (pkg (downcase (match-string-no-properties 2)))
           (release (match-string-no-properties 4))
           (urq-rules (my-clearcase-urq-rules dir pkg release)))
      (when urq-rules
        (delete-region (point-at-bol) (point-at-eol))
        (insert urq-rules)
        (forward-char 1)
        (unless (looking-at "^\\s-*$")
          (insert "\n")
          (backward-char 1)))))
   ;; ur label
   ((looking-at ".+?\\([A-Z0-9_]+\\)-\\([A-Z0-9_]+\\)__.+__\\([0-9]+\\)")
    (let* ((dir (downcase (match-string-no-properties 1)))
           (pkg (downcase (match-string-no-properties 2)))
           (urq-rules (my-clearcase-urq-rules dir pkg)))
      (when urq-rules
        (delete-region (point-at-bol) (save-excursion (re-search-forward "^\\s-*$\\|^\\s-*element.+[.][*]\\s-+/main/LATEST" nil t) (point)))
        (insert urq-rules)
        (forward-char 1)
        (unless (looking-at "^\\s-*$")
          (insert "\n")
          (backward-char 1)))))
   ;; Include
   ((looking-at "\\s-*include\\s-+\\([^@]+\\)@@[^ \t\n]+")
    (let ((output (shell-command-to-string (concat "cleartool ls -short " (match-string-no-properties 1)))))
      (delete-region (point-at-bol) (point-at-eol))
      (insert "include " output)
      (delete-char -1)))
   ;; File/directory
   ((looking-at "\\s-*\\([a-zA-Z0-9_./]+\\)")
    (let ((elm (match-string-no-properties 1)))
      (delete-region (point-at-bol) (point-at-eol))
      (insert "element " elm)
      (if (eq (char-before) ?/)
          (insert "...")
        (when (file-directory-p elm)
          (insert "/...")))
      (insert " /main/LATEST")))
   ;; Unknown
   (t
    (error "Couldn't parse current line"))))

(defun my-clearcase-urq-rules (dir pkg &optional release)
  "Get urq rules."
  (with-temp-buffer
    (let ((status (call-process "urq" nil t nil "rules" (concat dir "-" pkg (if release (concat ":" release))))))
      (if (not (equal status 0))
          (error "Couldn't get urq rules for current line")
        (goto-char (point-min))
        (keep-lines "^element.+")
        (buffer-substring-no-properties (point-min) (1- (point-max)))))))

;; Syntax table

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

;; Font lock

(defvar clearcase-edcs-mode-font-lock-keywords
  '(
    ("^\\s-*\\<\\(element\\|include\\)\\>"
     (1 font-lock-keyword-face))
    ("@@"
     (0 font-lock-reference-face))
    ("\\<\\(CHECKEDOUT\\|LATEST\\)\\>"
     (1 font-lock-builtin-face))
    )
  "Keyword highlighting specification for `clearcase-edcs-mode'.")

(make-variable-buffer-local 'clearcase-parent-buffer)

(define-key clearcase-edcs-mode-map (kbd "C-x C-s") 'clearcase-edcs-finish)
(define-key clearcase-edcs-mode-map (kbd "C-x C-w") 'clearcase-edcs-save)
(define-key clearcase-edcs-mode-map (kbd "C-c C-l") 'my-clearcase-cs-set-latest-dwim)

;; Abbrevs

(defvar clearcase-cs-mode-abbrev-table nil
  "*Abbrev table in use in ClearCase config spec buffers.")

(define-abbrev-table 'clearcase-cs-mode-abbrev-table ())

(define-abbrev clearcase-cs-mode-abbrev-table
  "lat"
  "/main/LATEST")

(define-abbrev clearcase-cs-mode-abbrev-table
  "el"
  "element")

(define-abbrev clearcase-cs-mode-abbrev-table
  "eld"
  "element -directory")

;; Modes

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leave this in here to not break existing users
(define-key clearcase-mode-map (kbd "C-v") clearcase-prefix-map)
(define-key clearcase-dired-mode-map (kbd "C-v") clearcase-dired-prefix-map)

(provide 'my-clearcase)
