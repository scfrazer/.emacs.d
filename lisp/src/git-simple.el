;;; git-simple.el

(require 'ansi-color)
(require 'diff)
(require 'ediff)
(require 'git-timemachine)
(require 'log-edit)
(require 'smerge-mode)

;; TODO Use space to mark files to operate on

(defgroup git-simple nil
  "Simple git."
  :group 'tools)

(defcustom git-simple-executable "git"
  "*Git executable."
  :type 'string
  :group 'git-simple)

(defconst git-simple-buf-prefix "*git-simple: ")

;;;###autoload
(defun git-simple (dir)
  "Simple Git mode."
  (interactive "DSelect directory: ")
  (unless (string-match ".+/$" dir)
    (setq dir (concat dir "/")))
  (setq dir (git-simple-find-root dir))
  (let* ((buf-name (concat git-simple-buf-prefix dir "*"))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (set-buffer buf)
      (setq default-directory dir)
      (git-simple-init)
      (git-simple-refresh)
      (git-simple-mode)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    (switch-to-buffer buf)))

;;;###autoload
(defun git-simple-switch-next ()
  "Switch to the next git-simple buffer."
  (interactive)
  (let ((bufs (cdr (buffer-list))) found)
    (while (and (not found) bufs)
      (if (string-match (concat git-simple-buf-prefix "/.+") (buffer-name (car bufs)))
          (setq found (car bufs))
        (setq bufs (cdr bufs))))
    (if found
        (progn (switch-to-buffer found)
               (git-simple-refresh))
      (call-interactively 'git-simple))))

(defun git-simple-find-root (dir)
  "Find root directory."
  (with-temp-buffer
    (let* ((default-directory dir))
      (unless (= (call-process git-simple-executable nil t nil "rev-parse" "--show-toplevel") 0)
        (error (concat "Couldn't find Git root for directory '" dir "'")))
      (goto-char (point-min))
      (file-name-as-directory (buffer-substring-no-properties (point) (point-at-eol))))))

(defun git-simple-init ()
  "Initialize the status buffer."
  (insert "Root:      " default-directory "\n"))

(defun git-simple-get-url (remote)
  "Get remote URL."
  (with-temp-buffer
    (unless (= (call-process git-simple-executable nil t nil "config" "--get" (concat "remote." remote ".url")) 0)
      (error (concat "Couldn't get Git URL for remote '" remote "'")))
    (goto-char (point-min))
    (buffer-substring-no-properties (point) (point-at-eol))))

(defun git-simple-refresh ()
  "Refresh status."
  (interactive)
  (message "Refreshing ...")
  (let ((buf (current-buffer))
        (file (git-simple-get-current-file)))
    (goto-char (point-min))
    (forward-line 1)
    (setq buffer-read-only nil)
    (delete-region (point) (point-max))
    (with-temp-buffer
      (unless (= (call-process git-simple-executable nil t nil "status" "-b" "--porcelain") 0)
        (error (concat "Couldn't get status for directory '" default-directory "'")))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "## \\([a-zA-Z0-9_-]+\\)\\([.][.][.]\\(\\([a-zA-Z0-9_-]+\\)/[a-zA-Z0-9_-]+\\)\\)?\\( .+\\)?")
               (let ((branch (match-string-no-properties 1))
                     (remote-branch (match-string-no-properties 3))
                     (remote (match-string-no-properties 4))
                     (state (match-string-no-properties 5)))
                 (with-current-buffer buf
                   (insert "Branch:    " branch (or state "") "\n")
                   (insert "Tracking:  " (or remote-branch "NONE") "\n")
                   (insert "URL:       " (if remote (git-simple-get-url remote) "NONE") "\n")
                   (insert "\n"))))
              ((looking-at "\\([ MADRCU?!]\\)\\([ MADU?!]\\) \\(.+\\)")
               (let ((index (match-string-no-properties 1))
                     (work-tree (match-string-no-properties 2))
                     (filename (match-string-no-properties 3)))
                 (with-current-buffer buf
                   (insert " " index work-tree " ~ " filename "\n")))))
        (forward-line 1)))
    (goto-char (point-min))
    (if (and file (search-forward file nil t))
        (progn
          (beginning-of-line)
          (git-simple-goto-next-file))
      (git-simple-goto-first-file))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (message "")))

(defun git-simple-goto-first-file ()
  "Goto first file."
  (interactive)
  (goto-char (point-min))
  (forward-line 4)
  (git-simple-goto-next-file))

(defun git-simple-goto-last-file ()
  "Goto last file."
  (interactive)
  (goto-char (point-max))
  (git-simple-goto-prev-file))

(defun git-simple-goto-next-file ()
  "Goto next file."
  (interactive)
  (search-forward "~ " nil t))

(defun git-simple-goto-prev-file ()
  "Goto previous file."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (when (search-backward "~ " nil t)
        (setq pos (+ (point) 2))))
    (goto-char pos)))

(defun git-simple-get-current-file ()
  "Get the current file."
  (if (string-match (regexp-quote git-simple-buf-prefix) (buffer-name))
      (save-excursion
        (beginning-of-line)
        (when (or (re-search-forward "-> \\(.+\\)" (point-at-eol) t)
                  (re-search-forward "~ \\(.+\\)" (point-at-eol) t))
          (match-string-no-properties 1)))
    (buffer-file-name)))

;;;###autoload
(defun git-simple-add-current-file ()
  "Add the current file."
  (interactive)
  (let ((file (git-simple-get-current-file)))
    (when file
      (message "Adding file ...")
      (unless (= (call-process git-simple-executable nil nil nil "add" file) 0)
        (error (concat "Couldn't add file '" file "'")))
      (git-simple-goto-next-file)
      (git-simple-refresh))))

(defun git-simple-add-tracked ()
  "Add files that are already tracked."
  (interactive)
  (message "Adding tracked files ...")
  (unless (= (call-process git-simple-executable nil nil nil "add" "-u") 0)
    (error "Couldn't add tracked files"))
  (git-simple-refresh))

;;;###autoload
(defun git-simple-diff-file ()
  "Diff file."
  (interactive)
  (let ((file (git-simple-get-current-file))
        (buf (get-buffer-create (concat " " git-simple-buf-prefix "Diff*"))))
    (when file
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (message "Diffing ...")
        (unless (= (call-process git-simple-executable nil t nil "diff" file) 0)
          (error (concat "Couldn't diff file '" file "'")))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (diff-mode)
        (font-lock-ensure))
      (set-window-buffer nil buf)
      (message ""))))

;;;###autoload
(defun git-simple-history ()
  "Go through git history using git-timemachine."
  (interactive)
  (git-simple-edit-file)
  (call-interactively 'git-timemachine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar git-simple-ediff-head-rev-buf nil)

;;;###autoload
(defun git-simple-ediff-file ()
  "ediff file."
  (interactive)
  (let ((file (git-simple-get-current-file)) bufB mode)
    (when file
      (setq bufB (get-buffer-create (find-file file)))
      (with-current-buffer bufB
        (setq mode major-mode))
      (setq git-simple-ediff-head-rev-buf (get-buffer-create (concat "HEAD:" file)))
      (with-current-buffer git-simple-ediff-head-rev-buf
        (erase-buffer)
        (message "Diffing ...")
        (unless (= (call-process git-simple-executable nil t nil "show" (concat "HEAD:" file)) 0)
          (error (concat "Couldn't get HEAD revision for file '" file "'")))
        (goto-char (point-min))
        (set-auto-mode-0 mode)
        (set-buffer-modified-p nil))
      (ediff-buffers git-simple-ediff-head-rev-buf bufB))))

(defun git-simple-ediff-quit-hook ()
  (when git-simple-ediff-head-rev-buf
    (kill-buffer git-simple-ediff-head-rev-buf)
    (setq git-simple-ediff-head-rev-buf nil)))

(add-hook 'ediff-quit-hook 'git-simple-ediff-quit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun git-simple-discard ()
  "Discard changes."
  (interactive)
  (let ((file (git-simple-get-current-file)))
    (if (save-excursion (beginning-of-line) (looking-at " [?][?]"))
        (when (and file (y-or-n-p (concat "Delete " file "? ")))
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file)))
      (when (and file (y-or-n-p (concat "Discard changes to " file "? ")))
        (message "Discarding file ...")
        (unless (= (call-process git-simple-executable nil nil nil "checkout" "--" file) 0)
          (error (concat "Couldn't discard changes to file '" file "'")))
        (git-simple-refresh)))))

;;;###autoload
(defun git-simple-unstage ()
  "Unstage file"
  (interactive)
  (let ((file (git-simple-get-current-file)))
    (when file
      (message "Unstaging file ...")
      (unless (= (call-process git-simple-executable nil nil nil "reset" "HEAD" "--" file) 0)
        (error (concat "Couldn't unstage file '" file "'")))
      (git-simple-refresh))))

(defun git-simple-edit-file ()
  "Edit file."
  (interactive)
  (let ((file (git-simple-get-current-file)))
    (when file
      (find-file file))))

(defun git-simple-grep ()
  "Run git grep from root."
  (interactive)
  ;; TODO
  )

;;;###autoload
(defun git-simple-resolve-file ()
  "Resolve merge conflicts."
  (interactive)
  (let ((file (git-simple-get-current-file)))
    (when file
      (find-file file)
      (call-interactively 'smerge-ediff))))

(defvar git-simple-commit-window-configuration nil)
(defvar git-simple-commit-buffer nil)

(defun git-simple-commit ()
  "Commit."
  (interactive)
  (setq git-simple-commit-window-configuration (current-window-configuration))
  (setq git-simple-commit-buffer (get-buffer-create (concat " " git-simple-buf-prefix "Commit*")))
  (log-edit 'git-simple-commit-finish t nil git-simple-commit-buffer))

(defun git-simple-commit-finish ()
  "Commit callback."
  (interactive)
  (when git-simple-commit-buffer
    (set-window-configuration git-simple-commit-window-configuration)
    (setq git-simple-commit-window-configuration nil)
    (kill-buffer git-simple-commit-buffer)
    (setq git-simple-commit-buffer nil)
    (message "Committing ...")
    (unless (= (call-process git-simple-executable nil nil nil "commit" "-m" (ring-ref log-edit-comment-ring 0)) 0)
      (error "Couldn't do commit"))
    (git-simple-refresh)))

(defun git-simple-push ()
  "Push."
  (interactive)
  (message "Pushing ...")
  (unless (= (call-process git-simple-executable nil nil nil "push") 0)
    (error "Couldn't push"))
  (git-simple-refresh))

;;;###autoload
(defun git-simple-exec (cmd)
  "Execute arbitrary command.
Substitute '%' in command with current file name."
  (interactive "sGit command? ")
  (let ((buf (get-buffer-create (concat git-simple-buf-prefix "Exec*")))
        (expanded-cmd (replace-regexp-in-string "%" (git-simple-get-current-file) cmd)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert git-simple-executable " " expanded-cmd "\n")
      (unless (= (apply #'call-process git-simple-executable nil t nil (split-string-and-unquote expanded-cmd)) 0)
        (error (concat "Error executing " cmd)))
      (ansi-color-apply-on-region (point-min) (point-max))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (view-mode)
      (goto-char (point-min))
      (display-buffer buf)))
  (git-simple-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar git-simple-mode-map nil
  "`git-simple-mode' keymap.")

;; TODO
;; ? -> show table translation
;; -------------------------------------------------
;; X          Y     Meaning
;; -------------------------------------------------
;;           [MD]   not updated
;; M        [ MD]   updated in index
;; A        [ MD]   added to index
;; D         [ M]   deleted from index
;; R        [ MD]   renamed in index
;; C        [ MD]   copied in index
;; [MARC]           index and work tree matches
;; [ MARC]     M    work tree changed since index
;; [ MARC]     D    deleted in work tree
;; -------------------------------------------------
;; D           D    unmerged, both deleted
;; A           U    unmerged, added by us
;; U           D    unmerged, deleted by them
;; U           A    unmerged, added by them
;; D           U    unmerged, deleted by us
;; A           A    unmerged, both added
;; U           U    unmerged, both modified
;; -------------------------------------------------
;; ?           ?    untracked
;; !           !    ignored
;; -------------------------------------------------
;;

(unless git-simple-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'git-simple-exec)
    (define-key map (kbd "=") 'git-simple-diff-file)
    (define-key map (kbd "A") 'git-simple-add-tracked)
    (define-key map (kbd "C") 'git-simple-commit)
    (define-key map (kbd "C-n") 'git-simple-goto-next-file)
    (define-key map (kbd "C-p") 'git-simple-goto-prev-file)
    (define-key map (kbd "G") 'git-simple-grep)
    (define-key map (kbd "M-<") 'git-simple-goto-first-file)
    (define-key map (kbd "M->") 'git-simple-goto-last-file)
    (define-key map (kbd "P") 'git-simple-push)
    (define-key map (kbd "RET") 'git-simple-edit-file)
    (define-key map (kbd "TAB") 'git-simple-diff-file)
    (define-key map (kbd "a") 'git-simple-add-current-file)
    (define-key map (kbd "e") 'git-simple-ediff-file)
    (define-key map (kbd "g") 'git-simple-refresh)
    (define-key map (kbd "h") 'git-simple-history)
    (define-key map (kbd "d") 'git-simple-discard)
    (define-key map (kbd "n") 'git-simple-goto-next-file)
    (define-key map (kbd "p") 'git-simple-goto-prev-file)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "r") 'git-simple-resolve-file)
    (define-key map (kbd "u") 'git-simple-unstage)
    (setq git-simple-mode-map map)))

(define-prefix-command 'git-simple-global-map)
(define-key git-simple-global-map (kbd "!") 'git-simple-exec)
(define-key git-simple-global-map (kbd "=") 'git-simple-diff-file)
(define-key git-simple-global-map (kbd "G") 'git-simple-grep)
(define-key git-simple-global-map (kbd "RET") 'git-simple-switch-next)
(define-key git-simple-global-map (kbd "a") 'git-simple-add-current-file)
(define-key git-simple-global-map (kbd "e") 'git-simple-ediff-file)
(define-key git-simple-global-map (kbd "h") 'git-simple-history)
(define-key git-simple-global-map (kbd "d") 'git-simple-discard)
(define-key git-simple-global-map (kbd "n") 'git-simple)
(define-key git-simple-global-map (kbd "r") 'git-simple-resolve-file)
(define-key git-simple-global-map (kbd "u") 'git-simple-unstage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar git-simple-mode-font-lock-keywords
  '(
    ("^\\([^:]+\\):"
     (1 'font-lock-keyword-face))
    ("\\[\\(ahead\\|behind\\) [0-9]+\\]"
     (0 'font-lock-warning-face))
    ;; Files with conflicts: (regexp-opt (list "DD" "AU" "UD" "UA" "DU" "AA" "UU"))
    ("^.\\(?:A[AU]\\|D[DU]\\|U[ADU]\\) ~ \\(.+\\)"
     (1 'error))
    ("^.\\([MADRCU]\\)"
     (1 'success))
    ("^..\\([MADU]\\)"
     (1 'error))
    ("^.\\([?]+\\|[!]+\\)"
     (1 'warning))
    )
  "Keyword highlighting specification for git-simple-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-simple-mode ()
  "Simple Git mode.

Key Bindings:

\\{git-simple-mode-map}"

  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'git-simple-mode)
  (setq mode-name "git-simple")
  (use-local-map git-simple-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(git-simple-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'git-simple-mode-hook))

(provide 'git-simple)
