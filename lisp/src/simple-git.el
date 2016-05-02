;;; simple-git.el

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
    (let* ((default-directory dir)
           (result (call-process simple-git-executable nil t nil "rev-parse" "--show-toplevel")))
      (unless (= result 0)
        (error (concat "Couldn't find Git root for directory '" dir "'")))
      (goto-char (point-min))
      (file-name-as-directory (buffer-substring-no-properties (point) (point-at-eol))))))

(defun simple-git-init ()
  "Initialize the status buffer."
  (insert "Root:    " default-directory "\n")
  (insert "Origin:  " (simple-git-get-origin default-directory) "\n"))

(defun simple-git-get-origin (dir)
  "Get origin."
  (with-temp-buffer
    (let* ((default-directory dir)
           (result (call-process simple-git-executable nil t nil "config" "--get" "remote.origin.url")))
      (unless (= result 0)
        (error (concat "Couldn't get Git origin for directory '" dir "'")))
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (point-at-eol)))))

(defun simple-git-refresh ()
  "Refresh status."
  (interactive)
  (let ((buf (current-buffer)))
    (goto-char (point-min))
    (forward-line 2)
    (setq buffer-read-only nil)
    (delete-region (point) (point-max))
    (with-temp-buffer
      (let ((result (call-process simple-git-executable nil t nil "status" "-b" "--porcelain")))
        (unless (= result 0)
          (error (concat "Couldn't get status for directory '" default-directory "'")))
        (goto-char (point-min))
        (while (not (eobp))
          (cond ((looking-at "## \\([a-zA-Z0-9_]+\\)\\([.][.][.]\\([a-zA-Z0-9_]+/[a-zA-Z0-9_]+\\)\\)?")
                 (let ((branch (match-string-no-properties 1))
                       (remote (match-string-no-properties 3)))
                   (with-current-buffer buf
                     (insert "Branch:  " branch "\n")
                     (insert "Remote:  " (or remote "NONE") "\n\n"))))
                ((looking-at "\\([ MADRCU?!]\\)\\([ MADU?!]\\) \\(.+\\)")
                 (let ((index (match-string-no-properties 1))
                       (work-tree (match-string-no-properties 2))
                       (filename (match-string-no-properties 3)))
                   (with-current-buffer buf
                     (insert " " index work-tree " -> " filename "\n")))))
          (forward-line 1))))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

;; TODO
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar simple-git-mode-map nil
  "`simple-git-mode' keymap.")

(unless simple-git-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'simple-git-refresh)
    (define-key map "q" 'bury-buffer)
    (setq simple-git-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar simple-git-mode-font-lock-keywords
  '(
    ("^[^:]+:"
     (0 'font-lock-keyword-face))
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
