;;; p4o.el

(require 'p4)
(require 'diff)

(defgroup p4o nil
  "P4 opened mode."
  :group 'tools)

(defface p4o-title-face
  '((t :inherit font-lock-function-name-face))
  "Font Lock mode face for changelist numbers."
  :group 'p4o)

(defface p4o-changelist-num-face
  '((t :inherit font-lock-keyword-face))
  "Font Lock mode face for changelist numbers."
  :group 'p4o)

(defface p4o-changelist-desc-face
  '((t :inherit font-lock-doc-face))
  "Font Lock mode face for changelist descriptions."
  :group 'p4o)

(defface p4o-edit-face
  '((t :inherit font-lock-builtin-face))
  "Font Lock mode face for edit indicator."
  :group 'p4o)

(defface p4o-add-face
  '((t :inherit success))
  "Font Lock mode face for add indicator."
  :group 'p4o)

(defface p4o-delete-face
  '((t :inherit error))
  "Font Lock mode face for error indicator."
  :group 'p4o)

(defface p4o-other-face
  '((t :inherit font-lock-constant-face))
  "Font Lock mode face for other indicators."
  :group 'p4o)

(defface p4o-merge-face
  '((t :inherit warning))
  "Font Lock mode face for merge indicator."
  :group 'p4o)

(defface p4o-marked-face
  '((t :bold t))
  "Font Lock mode face for marked files."
  :group 'p4o)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar p4o-client nil)
(defvar p4o-client-root nil)

(defun p4o (&optional arg)
  "Improved p4 opened output."
  (interactive "P")
  (let ((client (p4-current-setting "P4CLIENT")) buf)
    (when arg
      (setq client (read-string "Client? " client)))
    (setq buf (get-buffer-create (concat "*p4o " client "*")))
    (with-current-buffer buf
      (set (make-local-variable 'p4o-client) client)
      (set (make-local-variable 'p4o-client-root) (string-trim (shell-command-to-string (concat "p4 -c " client " -F %clientRoot% -ztag info"))))
      (p4o-refresh)
      (p4o-mode))
    (switch-to-buffer buf)))

(defun p4o-refresh ()
  "Refresh p4o status."
  (interactive)
  (let ((client p4o-client)
        (current-file (p4o-get-current-file))
        (current-changelist (p4o-get-current-changelist))
        (marked-files (p4o-get-marked-files))
        (changelist-alist (list (list "default" "Default change list"))))
    ;; Refresh
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Client: " client "\n")
    (insert "Root:   " p4o-client-root "\n")
    ;; Pending changelists
    (with-temp-buffer
      (let (change-num desc)
        (call-process "p4" nil t nil "changes" "-L" "-s" "pending" "-u" user-login-name "-c" client)
        (goto-char (point-min))
        (while (re-search-forward "^Change \\([0-9]+\\) .+ [*]pending[*]" nil t)
          (setq change-num (match-string-no-properties 1))
          (re-search-forward "^\t\\(.+\\)" nil t)
          (setq desc (match-string-no-properties 1))
          (add-to-list 'changelist-alist (list change-num desc)))))
    ;; Opened files
    (with-temp-buffer
      (let (filename rev action list-name change-num changelist items)
        (call-process "p4" nil t nil "-c" client "opened")
        (goto-char (point-min))
        (while (re-search-forward "^\\([^#]+\\)\\(#[0-9]+\\) - \\([^ ]+\\) \\(default\\|\\(change \\([0-9]+\\)\\)\\)" nil t)
          (setq filename (match-string-no-properties 1)
                rev (match-string-no-properties 2)
                action (match-string-no-properties 3)
                list-name (match-string-no-properties 4)
                change-num (match-string-no-properties 6))
          (when (string= list-name "default")
            (setq change-num list-name))
          (setq changelist (assoc change-num changelist-alist))
          (setq items (cdr changelist))
          (add-to-list 'items (format "%-11s  %s%s" action filename rev) t)
          (setcdr changelist items))))
    ;; Initial population of changelists and files
    (dolist (changelist changelist-alist)
      (insert "\n")
      (insert "[" (car changelist) "] -- " (cadr changelist) "\n")
      (when (> (length changelist) 2)
        (dolist (file (cddr changelist))
          (insert "   " file "\n"))))
    ;; See what needs to be resolved and update buffer
    (let ((buf (current-buffer)))
      (with-temp-buffer
        (let (filename to-revs from-rev)
          (call-process "p4" nil t nil "-c" client "resolve" "-n" "-o")
          (goto-char (point-min))
          (while (re-search-forward "^.+? - merging \\([^#]+?\\)\\([#,0-9]+\\) using base [^#]+\\(#[0-9]+\\)" nil t)
            (setq filename (match-string-no-properties 1)
                  to-revs (match-string-no-properties 2)
                  from-rev (match-string-no-properties 3))
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward (concat filename "\\>") nil t)
              (end-of-line)
              (insert "  <Needs merge: " from-rev " -> " to-revs ">"))))))
    (goto-char (point-min))
    ;; Try to restore previous state
    (let (depot-file)
      (when marked-files
        (dolist (file marked-files)
          (goto-char (point-min))
          (setq depot-file (p4o-file-to-depot-file file))
          (when (re-search-forward (concat "^.\\{16\\}" depot-file "#") nil t)
            (beginning-of-line)
            (delete-char 1)
            (insert "*"))))
      (if current-file
          (progn
            (goto-char (point-min))
            (setq depot-file (p4o-file-to-depot-file current-file))
            (if (re-search-forward (concat "^.\\{16\\}" depot-file "#") nil t)
                (beginning-of-line)
              (p4o-goto-first-item)))
        (if current-changelist
            (progn
              (goto-char (point-min))
              (if (re-search-forward (concat "^[[]" current-changelist "[]]") nil t)
                  (beginning-of-line)
                (p4o-goto-first-item)))
          (p4o-goto-first-item))))
    ;; Done
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p4o-file-to-depot-file (file)
  "Convert absolute file path to depot file path."
  (when (string-match (concat (regexp-quote p4o-client-root) "\\(.+\\)") file)
    (concat "/" (match-string-no-properties 1 file))))

(defun p4o-get-current-file ()
  "Get the file on the current line, if any."
  (beginning-of-line)
  (when (looking-at ".\\{16\\}/\\(/[^#]+\\)#")
    (concat p4o-client-root (match-string-no-properties 1))))

(defun p4o-get-current-changelist ()
  "Get the changelist on the current line, if any."
  (beginning-of-line)
  (when (looking-at "[[]\\(default\\|[0-9]+\\)[]]")
    (match-string-no-properties 1)))

(defun p4o-get-marked-files ()
  "Get a list of marked files."
  (save-excursion
    (goto-char (point-min))
    (let (files)
      (while (re-search-forward "^[*].\\{15\\}/\\(/[^#]+\\)#" nil t)
        (push (concat p4o-client-root (match-string-no-properties 1)) files))
      (nreverse files))))

(defun p4o-get-marked-depot-files ()
  "Get a list of marked files in depot format."
  (save-excursion
    (goto-char (point-min))
    (let (files)
      (while (re-search-forward "^[*].\\{15\\}\\(//[^#]+\\)#" nil t)
        (push (match-string-no-properties 1) files))
      (nreverse files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p4o-edit ()
  "Edit the current file."
  (interactive)
  (when-let ((file (p4o-get-current-file)))
    (find-file file)))

(defun p4o-diff ()
  "Diff the changelist, marked files, or current file."
  (interactive)
  (if-let ((change-num (p4o-get-current-changelist)))
      (p4o-diff-1 (concat "p4 opened -c " change-num " | sed -e 's/#.*//' | p4 -x - diff -du"))
    (if-let ((files (p4o-get-marked-files)))
        (p4o-diff-1 (concat "p4 diff -du " (mapconcat 'identity files " ")))
      (when-let ((file (p4o-get-current-file)))
        (p4o-diff-1 (concat "p4 diff -du " file))))))

(defun p4o-diff-1 (diff-cmd)
  "Common diff wrapper."
  (let ((buf (get-buffer-create (concat " *p4o diff*"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "Diffing ...")
      (shell-command diff-cmd t)
      (goto-char (point-min))
      (diff-mode)
      (font-lock-ensure)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (set-window-buffer nil buf)
      (message ""))))

(defun p4o-ediff ()
  "EDiff the current file."
  (interactive)
  (when-let ((file (p4o-get-current-file)))
    (setq p4-ediff-window-config (current-window-configuration))
    (find-file file)
    (p4-call-command "print" (list (concat file "#have"))
                     :after-show (p4-activate-ediff-callback))))

(defun p4o-revert ()
  "Revert marked files."
  (interactive)
  (when-let ((files (p4o-get-marked-depot-files)))
    (when (yes-or-no-p (format "Revert %d file(s)? " (length files)))
      (shell-command (format "p4 revert %s" (mapconcat 'identity files " ")))
      (p4o-refresh)
      (p4-refresh-buffers))))

(defun p4o-change-success (cmd buffer)
  "Callback after changelist creation/modification succeeds."
  (let (client)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Client:\t\\(.+\\)" nil t)
        (setq client (match-string-no-properties 1))))
    (p4-change-update-form buffer "pending" "^Change \\([0-9]+\\) created")
    (dolist (buf (buffer-list))
      (when (string-match "[*]P4 change" (buffer-name buf))
        (kill-buffer buf)))
    (when client
      (let* ((buf (get-buffer (concat "*p4o " client "*")))
             (win (get-buffer-window buf)))
        (when win
          (select-window win))
        (with-current-buffer buf
          (p4o-refresh))))))

(defp4cmd p4o-new-changelist ()
  "change"
  "Create a new changelist."
  (interactive)
  (p4-form-command "change" nil :move-to "Description:\n\t"
                   :mode 'p4-change-form-mode
                   :head-text p4-change-head-text
                   :success-callback 'p4o-change-success))

(defp4cmd p4o-edit-changelist ()
  "change"
  "Edit changelist on current line."
  (interactive)
  (when-let ((changelist (p4o-get-current-changelist)))
    (p4-form-command "change" (list changelist) :move-to "Description:\n\t"
                     :mode 'p4-change-form-mode
                     :head-text p4-change-head-text
                     :success-callback 'p4o-change-success)))

(defun p4o-reopen ()
  "Reopen marked files into a different changelist."
  (interactive)
  (when-let ((files (p4o-get-marked-files)))
    (let (changelists changelist)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[[]\\(default\\|[0-9]+\\)[]]" nil t)
          (push (match-string-no-properties 1) changelists)))
      (setq changelist
            (completing-read (format "Reopen %d file(s) in changelist: " (length files))
                             changelists nil t))
      (shell-command (format "p4 reopen -c %s %s" changelist (mapconcat 'identity files " ")))
      (p4o-unmark-all)
      (p4o-refresh))))

(defun p4o-resolve ()
  "Resolve marked files."
  (interactive)
  (when-let ((files (p4o-get-marked-depot-files)))
    (when (yes-or-no-p (format "Resolve %d file(s)? " (length files)))
      (shell-command (format "p4 resolve -am %s" (mapconcat 'identity files " ")))
      (p4o-unmark-all)
      (p4o-refresh)
      (p4-refresh-buffers))))

(defun p4o-submit-success (cmd buffer)
  (let (client)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Client:\t\\(.+\\)" nil t)
        (setq client (match-string-no-properties 1))))
    (p4-change-update-form buffer "submitted" "^Change \\(?:[0-9]+ renamed change \\)?\\([0-9]+\\)\\(?: and\\)? submitted\\.$")
    (p4-refresh-buffers)
    (dolist (buf (buffer-list))
      (when (string-match "[*]P4 \\(submit\\|change\\)" (buffer-name buf))
        (kill-buffer buf)))
    (when client
      (let* ((buf (get-buffer (concat "*p4o " client "*")))
             (win (get-buffer-window buf)))
        (when win
          (select-window win))
        (with-current-buffer buf
          (p4o-refresh))))))

(defp4cmd p4o-submit (&optional args)
  "submit"
  "Submit changelist on current line."
  (interactive)
  (when-let ((changelist (p4o-get-current-changelist)))
    (setq changelist (if (string= changelist "default") nil (list changelist)))
    (save-some-buffers nil (lambda () (or (not p4-do-find-file) p4-vc-status)))
    (p4-form-command "change" changelist :move-to "Description:\n\t"
                     :commit-cmd "submit"
                     :mode 'p4-change-form-mode
                     :head-text p4-submit-head-text
                     :success-callback 'p4o-submit-success
                     :failure-callback 'p4-submit-failure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p4o-goto-next-item ()
  "Goto next file or changelist."
  (interactive)
  (end-of-line)
  (re-search-forward "^\\([[]\\|.\\{16\\}//\\)" nil t)
  (beginning-of-line))

(defun p4o-goto-prev-item ()
  "Goto previous file or changelist."
  (interactive)
  (re-search-backward "^\\([[]\\|.\\{16\\}//\\)" nil t))

(defun p4o-goto-first-item ()
  "Goto first file or changelist."
  (interactive)
  (goto-char (point-min))
  (p4o-goto-next-item))

(defun p4o-goto-last-item ()
  "Goto last file or changelist."
  (interactive)
  (goto-char (point-max))
  (p4o-goto-prev-item))

(defun p4o-goto-next-changelist ()
  "Goto next changelist."
  (interactive)
  (end-of-line)
  (re-search-forward "^[[]" nil t)
  (beginning-of-line))

(defun p4o-goto-prev-changelist ()
  "Goto previous changelist."
  (interactive)
  (re-search-backward "^[[]" nil t))

(defun p4o-mark ()
  "Toggle mark of current file."
  (interactive)
  (setq buffer-read-only nil)
  (beginning-of-line)
  (cond ((looking-at "[*]")
         (delete-char 1)
         (insert " "))
        ((looking-at " ")
         (delete-char 1)
         (insert "*")))
  (beginning-of-line)
  (p4o-goto-next-item)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun p4o-unmark-all ()
  "Unmark all files."
  (interactive)
  (save-excursion
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward "^[*]" nil t)
      (backward-delete-char 1)
      (insert " "))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar p4o-mode-map nil
  "`p4o-mode' keymap.")

(unless p4o-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") 'p4o-ediff)
    (define-key map (kbd "=") 'p4o-diff)
    (define-key map (kbd "C") 'p4o-new-changelist)
    (define-key map (kbd "E") 'p4o-edit-changelist)
    (define-key map (kbd "C-n") 'p4o-goto-next-item)
    (define-key map (kbd "C-p") 'p4o-goto-prev-item)
    (define-key map (kbd "K") 'p4o-revert)
    (define-key map (kbd "M-<") 'p4o-goto-first-item)
    (define-key map (kbd "M->") 'p4o-goto-last-item)
    (define-key map (kbd "N") 'p4o-goto-next-changelist)
    (define-key map (kbd "O") 'p4o-reopen)
    (define-key map (kbd "P") 'p4o-goto-prev-changelist)
    (define-key map (kbd "R") 'p4o-resolve)
    (define-key map (kbd "RET") 'p4o-edit)
    (define-key map (kbd "S") 'p4o-submit)
    (define-key map (kbd "SPC") 'p4o-mark)
    (define-key map (kbd "U") 'p4o-unmark-all)
    (define-key map (kbd "g") 'p4o-refresh)
    (define-key map (kbd "n") 'p4o-goto-next-item)
    (define-key map (kbd "p") 'p4o-goto-prev-item)
    (define-key map (kbd "q") 'bury-buffer)
    (setq p4o-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar p4o-mode-font-lock-keywords
  '(
    ("^\\([a-zA-Z]+\\):"
     (1 'p4o-title-face))
    ("^\\[\\([0-9]+\\|default\\)\\] -- \\(.+\\)"
     (1 'p4o-changelist-num-face)
     (2 'p4o-changelist-desc-face))
    ("^.  \\(edit\\)"
     (1 'p4o-edit-face))
    ("^.  \\(\\(move/\\)?add\\)"
     (1 'p4o-add-face))
    ("^.  \\(\\(move/\\)?delete\\)"
     (1 'p4o-delete-face))
    ("^.  \\(branch\\|integrate\\|import\\|purge\\|archive\\)"
     (1 'p4o-other-face))
    ("<Needs merge.+"
     (0 'p4o-merge-face))
    ("^[*].+$"
     (0 'p4o-marked-face keep))
    )
  "Keyword highlighting specification for p4o-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p4o-mode ()
  "P4 opened mode.

Key Bindings:

\\{p4o-mode-map}"

  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'p4o-mode)
  (setq mode-name "p4o")
  (use-local-map p4o-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(p4o-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'p4o-mode-hook))

(define-key p4-prefix-map "o" 'p4o)

(provide 'p4o)
