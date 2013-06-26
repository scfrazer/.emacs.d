;;; cc-status.el

(require 'clearcase)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cc-status-ignore-regexps (list "@@"
                                       "ver/build"
                                       "/obj\\(64\\)?"
                                       "\\.cmake\\.state"
                                       "rtl/Makefile\\(\\..+\\)?"
                                       "rtl/.+?\\.\\(vlist\\|xpdb\\|args\\|makerule\\)"
                                       "rtl/dump.rdl")
  "*Regexps of view-private elements to ignore.")

(defvar cc-status-filter t
  "*Use `cc-status-ignore-regexps' to filter view-private elements.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct cc-status-elm filename ok private reserved mastered pull merge)

(defun cc-status ()
  "ClearCase status."
  (interactive)
  (let ((buf (get-buffer "*cc-status*")))
    (unless buf
      (setq buf (get-buffer-create "*cc-status*"))
      (set-buffer buf)
      (cc-status-refresh)
      (cc-status-mode))
    (switch-to-buffer buf)))

(defvar cc-status-elms nil)

(defun cc-status-refresh ()
  "Refresh the cc-status buffer."
  (interactive)
  (setq buffer-read-only nil)
  (message "Refreshing ClearCase status ...")
  (setq cc-status-elms nil)
  (let (filename)
    ;; Private files
    (with-temp-buffer
      (call-process-shell-command "cleartool lspri -other" nil t)
      (message "Checking view-private files ...")
      (goto-char (point-min))
      (while (not (eobp))
        (setq filename (buffer-substring-no-properties (point) (point-at-eol)))
        (unless (and cc-status-filter
                     (catch 'ignore
                       (dolist (regexp cc-status-ignore-regexps)
                         (when (string-match regexp filename)
                           (throw 'ignore t)))))
          (push (make-cc-status-elm :filename filename :private t) cc-status-elms))
        (forward-line 1)))
    ;; Checked out files
    (let (elm user version reserved mastered view location master-replica latest-version)
      (with-temp-buffer
        (call-process-shell-command "cleartool lspri -co -short" nil t)
        (goto-char (point-min))
        (while (not (eobp))
          (setq filename (buffer-substring-no-properties (point) (point-at-eol)))
          (setq elm (make-cc-status-elm :filename filename))
          (with-temp-buffer
            (message (concat "Updating status for " filename " ..."))
            (call-process-shell-command
             (concat "cleartool lsco -areplicas -fmt \"%u %f %Rf %Mf %Tf %[checkout_replica]p\\n\" " filename) nil t)
            (goto-char (point-min))
            (while (not (eobp))
              (looking-at "\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.+\\)")
              (setq user (match-string-no-properties 1)
                    version (match-string-no-properties 2)
                    reserved (match-string-no-properties 3)
                    mastered (match-string-no-properties 4)
                    view (match-string-no-properties 5)
                    location (match-string-no-properties 6))
              (if (string= view clearcase-setview-viewtag)
                  (progn
                    (setq master-replica (shell-command-to-string (concat "cleartool describe -fmt \"%[master]p\" " filename "@@/main")))
                    (setq latest-version (shell-command-to-string (concat "cleartool describe -fmt \"%Vn\" " filename "@@/main/LATEST")))
                    (unless (string= location master-replica)
                      (setf (cc-status-elm-pull elm) (concat "[Needs pull: " master-replica "]")))
                    (unless (string= version latest-version)
                      (with-temp-buffer
                        (call-process-shell-command
                         (concat "cleartool desc -fmt \"%[hlink:Merge]p\\n\" " filename) nil t)
                        (goto-char (point-min))
                        (while (not (eobp))
                          (when (looking-at (concat ".+Merge.+\"" filename "@@" latest-version "\""))
                            (setf (cc-status-elm-ok elm) (concat "{Merged to: " latest-version "}")))
                          (forward-line 1)))
                      (unless (cc-status-elm-ok elm)
                        (setf (cc-status-elm-merge elm) (concat "[Checked out: " version ", /main/LATEST: " latest-version "]")))))
                (when (string= reserved "reserved")
                  (setf (cc-status-elm-reserved elm) (concat "[Reserved: " user ":" view "]")))
                (when (string= mastered "mastered")
                  (setf (cc-status-elm-mastered elm) (concat "[Mastered: " user ":" view "]"))))
              (forward-line 1)))
          (unless (or (cc-status-elm-ok elm)
                      (cc-status-elm-private elm)
                      (cc-status-elm-reserved elm)
                      (cc-status-elm-mastered elm)
                      (cc-status-elm-pull elm)
                      (cc-status-elm-merge elm))
            (setf (cc-status-elm-ok elm) "{OK}"))
          (push elm cc-status-elms)
          (forward-line 1))))
    ;; Update buffer
    (erase-buffer)
    (insert "ClearCase Status:\n\n")
    (setq cc-status-elms (sort cc-status-elms #'(lambda (x y) (string< (cc-status-elm-filename x) (cc-status-elm-filename y)))))
    (dolist (elm cc-status-elms)
      (insert "  " (cc-status-elm-filename elm) " "
              (if (cc-status-elm-ok elm) (concat " " (cc-status-elm-ok elm)) "")
              (if (cc-status-elm-private elm) " (Private)" "")
              (if (cc-status-elm-reserved elm) (concat " " (cc-status-elm-reserved elm)) "")
              (if (cc-status-elm-mastered elm) (concat " " (cc-status-elm-mastered elm)) "")
              (if (cc-status-elm-pull elm) (concat " " (cc-status-elm-pull elm)) "")
              (if (cc-status-elm-merge elm) (concat " " (cc-status-elm-merge elm)) "")
              "\n")))
  (cc-status-goto-first-file)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (message "Done"))

(defun cc-status-goto-first-file ()
  "Go to the first file."
  (interactive)
  (goto-char (point-min))
  (forward-line 2))

(defun cc-status-goto-last-file ()
  "Go to the last file."
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun cc-status-next-file ()
  "Go to next file."
  (interactive)
  (forward-line 1)
  (when (looking-at "^$")
    (forward-line -1)))

(defun cc-status-prev-file ()
  "Go to previous file."
  (interactive)
  (forward-line -1)
  (when (looking-at "^$")
    (forward-line 1)))

(defun cc-status-open-file ()
  "Open file."
  (interactive)
  (beginning-of-line)
  (when (looking-at ". \\(/[^ ]+\\) .+$")
    (find-file (match-string-no-properties 1))))

(defun cc-status-ediff ()
  "Ediff the current file."
  (interactive)
  (beginning-of-line)
  (if (looking-at ". \\(/[^ ]+\\)  [[{].+$")
      (let ((filename (match-string-no-properties 1)))
        (clearcase-ediff-file-with-version filename (clearcase-fprop-predecessor-version filename)))
    (error "Can't diff a view-private file.")))

(defun cc-status-mark (op)
  "Mark a file."
  (beginning-of-line)
  (let* ((elm-num (- (line-number-at-pos) 3))
         (elm (elt cc-status-elms elm-num)))
    (cond ((= op ?p)
           (unless (cc-status-elm-pull elm)
             (error "File doesn't need to be pulled")))
          ((= op ?m)
           (unless (cc-status-elm-merge elm)
             (error "File doesn't need to be merged")))
          ((= op ?i)
           (when (cc-status-elm-private elm)
             (error "File is view-private")))
          ((= op ?u)
           (when (cc-status-elm-private elm)
             (error "File is view-private")))
          ((= op ?r)
           (when (cc-status-elm-private elm)
             (error "File is view-private")))
          ((= op ?d)
           (unless (cc-status-elm-private elm)
             (error "You can only delete private files")))
          (t
           nil))
    (setq buffer-read-only nil)
    (delete-char 1)
    (insert (upcase op))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (cc-status-next-file)))

(defun cc-status-pull (files)
  "Pull files."
  (when files
    (message "Pulling files ...")
    (shell-command-to-string (concat "cpull " (mapconcat 'identity files " ")))))

(defun cc-status-merge (files)
  "Merge files."
  (when files
    (message "Merging files ...")
    (dolist (filename files)
      (shell-command-to-string (concat "cleartool merge -abort -to " filename " " filename "@@/main/LATEST")))))

(defun cc-status-uncheckout-keep (files)
  "Uncheckout keeping files."
  (when files
    (message "Uncheckout keeping files ...")
    (clearcase-ct-blocking-call "unco" "-keep" (mapconcat 'identity files " "))
    (dolist (filename files)
      (when (find-buffer-visiting filename)
        (clearcase-sync-from-disk filename t)))))

(defun cc-status-uncheckout-rm (files)
  "Uncheckout removing files."
  (when files
    (message "Uncheckout removing files ...")
    (clearcase-ct-blocking-call "unco" "-rm" (mapconcat 'identity files " "))
    (dolist (filename files)
      (when (find-buffer-visiting filename)
        (clearcase-sync-from-disk filename t)))))

(defun cc-status-delete (files)
  "Delete files."
  (when files
    (message "Deleting files ...")
    (dolist (filename files)
      (delete-file filename))))

(defconst cc-status-comment-buffer-name "*cc-status-comment*")
(defvar cc-status-prev-window-config nil)
(defvar cc-status-files-to-checkin)

(defun cc-status-execute ()
  "Execute commands on marked files."
  (interactive)
  (when (y-or-n-p "Operate on marked files? ")
    (cc-status-goto-first-file)
    (let (op files-to-pull files-to-merge files-to-uncheckout-keep files-to-uncheckout-rm files-to-delete)
      (setq cc-status-files-to-checkin nil)
      (dolist (elm cc-status-elms)
        (setq op (char-after))
        (cond ((= op ?p)
               (push (cc-status-elm-filename elm) files-to-pull))
              ((= op ?m)
               (push (cc-status-elm-filename elm) files-to-merge))
              ((= op ?i)
               (push (cc-status-elm-filename elm) cc-status-files-to-checkin))
              ((= op ?u)
               (push (cc-status-elm-filename elm) files-to-uncheckout-keep))
              ((= op ?r)
               (push (cc-status-elm-filename elm) files-to-uncheckout-rm))
              ((= op ?d)
               (push (cc-status-elm-filename elm) files-to-delete))
              (t
               nil))
        (forward-line 1))
      (cc-status-pull files-to-pull)
      (cc-status-merge files-to-merge)
      (cc-status-uncheckout-keep files-to-uncheckout-keep)
      (cc-status-uncheckout-rm files-to-uncheckout-rm)
      (cc-status-delete files-to-delete))
    (if (not cc-status-files-to-checkin)
        (cc-status-refresh)
      (setq cc-status-prev-window-config (current-window-configuration))
      (pop-to-buffer cc-status-comment-buffer-name)
      (cc-status-comment-mode))))

(define-derived-mode cc-status-comment-mode text-mode "cc-status-comment-mode"
  "Add comments for checkin or mkelem."
  (local-set-key (kbd "C-x C-s") 'cc-status-comment-mode-finish))

(defun cc-status-comment-mode-finish ()
  "Finish entering comment and do the operations."
  (interactive)
  (let ((comment (buffer-substring-no-properties (point-min) (point-max))))
    (when (string= comment "")
      (setq comment nil))
    (kill-buffer cc-status-comment-buffer-name)
    (when cc-status-prev-window-config
      (set-window-configuration cc-status-prev-window-config)
      (setq cc-status-prev-window-config nil))
    (cc-status-update-config-spec (cc-status-checkin cc-status-files-to-checkin comment)))
  (cc-status-refresh))

(defun cc-status-checkin (files comment)
  "Check in files and return config spec changes."
  (when files
    (message "Checking in files ...")
    (let (cs-changes)
      (with-temp-buffer
        (if (not comment)
            (insert (clearcase-ct-blocking-call "ci" "-nc" (mapconcat 'identity files " ")))
          (let ((temp-file (make-temp-file "cc-status-comment-")))
            (with-temp-file temp-file
              (insert comment))
            (insert (clearcase-ct-blocking-call "ci" "-cfile" temp-file (mapconcat 'identity files " ")))
            (delete-file temp-file)))
        (goto-char (point-min))
        (when (re-search-forward "Checked in \"\\(.+?\\)\" version \"\\(.+?\\)\"" nil t)
          (setq cs-changes
                (concat cs-changes "element " (match-string-no-properties 1) " " (match-string-no-properties 2) "\n"))))
      (dolist (filename files)
        (when (find-buffer-visiting filename)
          (clearcase-sync-from-disk filename t)))
      (message "")
      cs-changes)))

(defun cc-status-update-config-spec (cs-changes)
  "Update the current config spec with the supplied changes."
  (message "Updating config spec ...")
  (let ((temp-file (make-temp-file "cc-status-config-spec-")))
    (with-temp-file temp-file
      (insert (clearcase-ct-blocking-call "catcs" "-tag" clearcase-setview-viewtag))
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*element\\s-+[*]\\s-+CHECKEDOUT.*$" nil t)
        (forward-line 1)
        (insert "\n" cs-changes "\n")))
    (clearcase-ct-blocking-call "setcs" "-tag" clearcase-setview-viewtag temp-file)
    (clearcase-fprop-clear-all-properties)
    (delete-file temp-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keymap

(defvar cc-status-mode-map nil
  "`cc-status-mode' keymap.")

(unless cc-status-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" 'bury-buffer)
    (define-key map "g" 'cc-status-refresh)

    (define-key map (kbd "C-n") 'cc-status-next-file)
    (define-key map (kbd "C-p") 'cc-status-prev-file)
    (define-key map (kbd "<down>") 'cc-status-next-file)
    (define-key map (kbd "<up>") 'cc-status-prev-file)

    (define-key map (kbd "RET") 'cc-status-open-file)
    (define-key map "=" 'cc-status-ediff)

    (define-key map (kbd "p") (lambda () (interactive) (cc-status-mark ?p)))
    (define-key map (kbd "m") (lambda () (interactive) (cc-status-mark ?m)))
    (define-key map (kbd "i") (lambda () (interactive) (cc-status-mark ?i)))
    (define-key map (kbd "u") (lambda () (interactive) (cc-status-mark ?u)))
    (define-key map (kbd "r") (lambda () (interactive) (cc-status-mark ?r)))
    (define-key map (kbd "d") (lambda () (interactive) (cc-status-mark ?d)))
    (define-key map (kbd "SPC") (lambda () (interactive) (cc-status-mark nil)))
    (define-key map (kbd "x") 'cc-status-execute)

    (define-key map (kbd "M-<") 'cc-status-goto-first-file)
    (define-key map (kbd "M->") 'cc-status-goto-last-file)

    (setq cc-status-mode-map map)))

;; Font-lock

(defvar cc-status-mode-font-lock-keywords
  '(
    ("^. "
     (0 'font-lock-keyword-face))
    ("^\\(.\\) \\(.+ (.+)\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-comment-face))
    ("{.+}"
     (0 'font-lock-variable-name-face))
    ("\\[.+\\]"
     (0 'font-lock-warning-face))
    )
  "Keyword highlighting specification for cc-status.")

;; Mode

(defun cc-status-mode ()
  "Major mode for working with ClearCase status.

Key Bindings:

\\{cc-status-mode-map}"

  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'cc-status-mode)
  (setq mode-name "cc-status")
  (use-local-map cc-status-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(cc-status-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'cc-status-mode-hook))

(provide 'cc-status)
