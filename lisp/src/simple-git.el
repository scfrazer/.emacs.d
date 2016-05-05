;;; simple-git.el

(require 'diff)
(require 'ediff)
(require 'log-edit)

(defgroup simple-git nil
  "Simple git."
  :group 'tools)

(defcustom simple-git-executable "git"
  "*Git executable."
  :type 'string
  :group 'simple-git)

(defconst simple-git-buf-prefix "*simple-git: ")

(defun simple-git (dir)
  "Simple Git mode."
  (interactive "DSelect directory: ")
  (setq dir (simple-git-find-root dir))
  (let* ((buf-name (concat simple-git-buf-prefix dir "*"))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (set-buffer buf)
      (setq default-directory dir)
      (simple-git-init)
      (simple-git-refresh)
      (simple-git-mode)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    (switch-to-buffer buf)))

(defun simple-git-find-root (dir)
  "Find root directory."
  (with-temp-buffer
    (let* ((default-directory dir))
      (unless (= (call-process simple-git-executable nil t nil "rev-parse" "--show-toplevel") 0)
        (error (concat "Couldn't find Git root for directory '" dir "'")))
      (goto-char (point-min))
      (file-name-as-directory (buffer-substring-no-properties (point) (point-at-eol))))))

(defun simple-git-init ()
  "Initialize the status buffer."
  (insert "Root:      " default-directory "\n"))

(defun simple-git-get-url (remote)
  "Get remote URL."
  (with-temp-buffer
    (unless (= (call-process simple-git-executable nil t nil "config" "--get" (concat "remote." remote ".url")) 0)
      (error (concat "Couldn't get Git URL for remote '" remote "'")))
    (goto-char (point-min))
    (buffer-substring-no-properties (point) (point-at-eol))))

(defun simple-git-refresh ()
  "Refresh status."
  (interactive)
  (message "Refreshing ...")
  (let ((buf (current-buffer))
        (file (simple-git-get-current-file)))
    (goto-char (point-min))
    (forward-line 1)
    (setq buffer-read-only nil)
    (delete-region (point) (point-max))
    (with-temp-buffer
      (unless (= (call-process simple-git-executable nil t nil "status" "-b" "--porcelain") 0)
        (error (concat "Couldn't get status for directory '" default-directory "'")))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "## \\([a-zA-Z0-9_]+\\)\\([.][.][.]\\(\\([a-zA-Z0-9_]+\\)/[a-zA-Z0-9_]+\\)\\)?\\( .+\\)?")
               (let ((branch (match-string-no-properties 1))
                     (remote-branch (match-string-no-properties 3))
                     (remote (match-string-no-properties 4))
                     (state (match-string-no-properties 5)))
                 (with-current-buffer buf
                   (insert "Branch:    " branch (or state "") "\n")
                   (insert "Tracking:  " (or remote-branch "NONE") "\n")
                   (insert "URL:       " (if remote (simple-git-get-url remote) "NONE") "\n")
                   (insert "\n"))))
              ((looking-at "\\([ MADRCU?!]\\)\\([ MADU?!]\\) \\(.+\\)")
               (let ((index (match-string-no-properties 1))
                     (work-tree (match-string-no-properties 2))
                     (filename (match-string-no-properties 3)))
                 (with-current-buffer buf
                   (insert " " index work-tree " -> " filename "\n")))))
        (forward-line 1)))
    (goto-char (point-min))
    (if (and file (search-forward file nil t))
        (progn
          (beginning-of-line)
          (simple-git-goto-next-file))
      (simple-git-goto-first-file))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (message "")))

(defun simple-git-goto-first-file ()
  "Goto first file."
  (interactive)
  (goto-char (point-min))
  (forward-line 4)
  (simple-git-goto-next-file))

(defun simple-git-goto-last-file ()
  "Goto last file."
  (interactive)
  (goto-char (point-max))
  (simple-git-goto-prev-file))

(defun simple-git-goto-next-file ()
  "Goto next file."
  (interactive)
  (search-forward "-> " nil t))

(defun simple-git-goto-prev-file ()
  "Goto previous file."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (when (search-backward "-> " nil t)
        (setq pos (+ (point) 3))))
    (goto-char pos)))

(defun simple-git-get-current-file ()
  "Get the current file."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "-> \\(.+\\)" (point-at-eol) t)
      (match-string-no-properties 1))))

(defun simple-git-add-current-file ()
  "Add the current file."
  (interactive)
  (let ((file (simple-git-get-current-file)))
    (when file
      (message "Adding file ...")
      (unless (= (call-process simple-git-executable nil nil nil "add" file) 0)
        (error (concat "Couldn't add file '" file "'")))
      (simple-git-refresh))))

(defun simple-git-add-tracked ()
  "Add files that are already tracked."
  (interactive)
  (message "Adding tracked files ...")
  (unless (= (call-process simple-git-executable nil nil nil "add" "-u") 0)
    (error "Couldn't add tracked files"))
  (simple-git-refresh))

(defun simple-git-diff-file ()
  "Diff file."
  (interactive)
  (let ((file (simple-git-get-current-file))
        (buf (get-buffer-create (concat " " simple-git-buf-prefix "Diff*"))))
    (when file
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (message "Diffing ...")
        (unless (= (call-process simple-git-executable nil t nil "diff" file) 0)
          (error (concat "Couldn't diff file '" file "'")))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (diff-mode)
        (font-lock-fontify-buffer))
      (set-window-buffer nil buf)
      (message ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar simple-git-ediff-head-rev-buf)

(defun simple-git-ediff-file ()
  "ediff file."
  (interactive)
  (let ((file (simple-git-get-current-file)) bufB mode)
    (when file
      (setq bufB (get-buffer-create (find-file file)))
      (with-current-buffer bufB
        (setq mode major-mode))
      (setq simple-git-ediff-head-rev-buf (get-buffer-create (concat "HEAD:" file)))
      (with-current-buffer simple-git-ediff-head-rev-buf
        (erase-buffer)
        (message "Diffing ...")
        (unless (= (call-process simple-git-executable nil t nil "show" (concat "HEAD:" file)) 0)
          (error (concat "Couldn't get HEAD revision for file '" file "'")))
        (goto-char (point-min))
        (set-auto-mode-0 mode)
        (set-buffer-modified-p nil))
      (ediff-buffers simple-git-ediff-head-rev-buf bufB))))

(defun simple-git-ediff-quit-hook ()
  (when simple-git-ediff-head-rev-buf
    (kill-buffer simple-git-ediff-head-rev-buf)
    (setq simple-git-ediff-head-rev-buf nil)))

(add-hook 'ediff-quit-hook 'simple-git-ediff-quit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-git-discard ()
  "Discard changes."
  (interactive)
  (let ((file (simple-git-get-current-file)))
    (when (and file (y-or-n-p (concat "Discard changes to " file "? ")))
      (message "Discarding file ...")
      (unless (= (call-process simple-git-executable nil nil nil "checkout" "--" file) 0)
        (error (concat "Couldn't discard changes to file '" file "'")))
      (simple-git-refresh))))

(defun simple-git-edit-file ()
  "Edit file."
  (interactive)
  (let ((file (simple-git-get-current-file)))
    (when file
      (find-file file))))

(defvar simple-git-commit-window-configuration)
(defvar simple-git-commit-buffer)

(defun simple-git-commit ()
  "Commit."
  (interactive)
  (setq simple-git-commit-window-configuration (current-window-configuration))
  (setq simple-git-commit-buffer (get-buffer-create (concat " " simple-git-buf-prefix "Commit*")))
  (log-edit 'simple-git-commit-finish t nil simple-git-commit-buffer))

(defun simple-git-commit-finish ()
  "Commit callback."
  (interactive)
  (when simple-git-commit-buffer
    (set-window-configuration simple-git-commit-window-configuration)
    (setq simple-git-commit-window-configuration nil)
    (kill-buffer simple-git-commit-buffer)
    (setq simple-git-commit-buffer nil)
    (message "Committing ...")
    (unless (= (call-process simple-git-executable nil nil nil "commit" "-m" (ring-ref log-edit-comment-ring 0)) 0)
      (error "Couldn't do commit"))
    (simple-git-refresh)))

(defun simple-git-push ()
  "Push."
  (interactive)
  (message "Pushing ...")
  (unless (= (call-process simple-git-executable nil nil nil "push") 0)
    (error "Couldn't push"))
  (simple-git-refresh))

(defun simple-git-exec (cmd)
  "Execute arbitrary command."
  (interactive "sGit command? ")
  ;; TODO Get current file and sub wherever % is
  (with-temp-buffer
    (unless (= (apply #'call-process simple-git-executable nil t nil (split-string-and-unquote cmd)) 0)
      (error (concat "Error executing " cmd)))
    (message (buffer-substring-no-properties (point-min) (point-max))))
  (simple-git-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar simple-git-mode-map nil
  "`simple-git-mode' keymap.")

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

(unless simple-git-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'simple-git-exec)
    (define-key map (kbd "=") 'simple-git-diff-file)
    (define-key map (kbd "A") 'simple-git-add-tracked)
    (define-key map (kbd "C") 'simple-git-commit)
    (define-key map (kbd "C-n") 'simple-git-goto-next-file)
    (define-key map (kbd "C-p") 'simple-git-goto-prev-file)
    (define-key map (kbd "M-<") 'simple-git-goto-first-file)
    (define-key map (kbd "M->") 'simple-git-goto-last-file)
    (define-key map (kbd "P") 'simple-git-push)
    (define-key map (kbd "RET") 'simple-git-edit-file)
    (define-key map (kbd "a") 'simple-git-add-current-file)
    (define-key map (kbd "e") 'simple-git-ediff-file)
    (define-key map (kbd "g") 'simple-git-refresh)
    (define-key map (kbd "k") 'simple-git-discard)
    (define-key map (kbd "n") 'simple-git-goto-next-file)
    (define-key map (kbd "p") 'simple-git-goto-prev-file)
    (define-key map (kbd "q") 'bury-buffer)
    (setq simple-git-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar simple-git-mode-font-lock-keywords
  '(
    ("^\\([^:]+\\):"
     (1 'font-lock-keyword-face))
    ("\\[\\(ahead\\|behind\\) [0-9]+\\]"
     (0 'font-lock-warning-face))
    ("^.\\([MADRCU]\\)"
     (1 'success))
    ("^..\\([MADU]\\)"
     (1 'error))
    ("^.\\([?]+\\|[!]+\\)"
     (1 'warning))
    )
  "Keyword highlighting specification for simple-git-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-git-mode ()
  "Simple Git mode.

Key Bindings:

\\{simple-git-mode-map}"

  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'simple-git-mode)
  (setq mode-name "simple-git")
  (use-local-map simple-git-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(simple-git-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'simple-git-mode-hook))

(provide 'simple-git)
