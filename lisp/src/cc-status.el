;;; cc-status.el

;; TODO Eclipsed files?

(require 'clearcase)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup cc-status nil
  "ClearCase status."
  :group 'tools)

(defcustom cc-status-filter-private t
  "*Use `cc-status-ignore-regexps' to filter view-private elements."
  :group 'cc-status)

(defcustom cc-status-no-private nil
  "*Don't show any private files."
  :group 'cc-status)

(defcustom cc-status-ignore-regexps (list "#"
                                          "~"
                                          "<.+>"
                                          "\\.swp"
                                          "@@"
                                          "ver/build"
                                          "/obj\\(64\\)?"
                                          "\\.cmake\\.state"
                                          "rtl/Makefile\\(\\..+\\)?"
                                          "rtl/.+?\\.\\(vlist\\|xpdb\\|args\\|makerule\\)"
                                          "rtl/dump.rdl"
                                          "Makefile.chip"
                                          "chipdv.targets.macro"
                                          "bus_struct"
                                          "\\.pyc"
                                          "simv"
                                          "rtl/.*spyglass.*")
  "*Regexps of view-private elements to ignore."
  :type '(repeat (string :tag "Regexp"))
  :group 'cc-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst cc-status-buffer-name "*cc-status*")
(defconst cc-status-output-buffer-name " *cc-status-output*")
(defconst cc-status-comment-buffer-name "*cc-status-comment*")

(defstruct cc-status-elm filename ok private reserved mastered pull merge)

(defun cc-status ()
  "ClearCase status."
  (interactive)
  (let ((buf (get-buffer cc-status-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create cc-status-buffer-name))
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
    (unless cc-status-no-private
      (with-temp-buffer
        (call-process-shell-command "cleartool lspri -other" nil t)
        (message "Checking view-private files ...")
        (goto-char (point-min))
        (while (not (eobp))
          (setq filename (buffer-substring-no-properties (point) (point-at-eol)))
          (unless (and cc-status-filter-private
                       (catch 'ignore
                         (dolist (regexp cc-status-ignore-regexps)
                           (when (string-match regexp filename)
                             (throw 'ignore t)))))
            (push (make-cc-status-elm :filename filename :private t) cc-status-elms))
          (forward-line 1))))
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
    (insert "ClearCase Status" (if cc-status-no-private " (No private)" "") ":\n\n")
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
  (cc-status-sync)
  (message "Done"))

(defun cc-status-prep-output-buffer ()
  "Prepare the output buffer."
  (with-current-buffer (get-buffer-create cc-status-output-buffer-name)
    (widen)
    (goto-char (point-max))
    (setq buffer-read-only nil)))

(defun cc-status-display-output ()
  "Show the output from recent ClearCase commands."
  (interactive)
  (cc-status-prep-output-buffer)
  (pop-to-buffer cc-status-output-buffer-name)
  (setq buffer-read-only t))

(defun cc-status-get-current-elm ()
  "Get the element on the current line."
  (let* ((elm-num (- (line-number-at-pos) 3))
         (elm (elt cc-status-elms elm-num)))
    elm))

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
  (find-file (cc-status-elm-filename (cc-status-get-current-elm))))

(defun cc-status-ediff (&optional arg)
  "Ediff the current file."
  (interactive "P")
  (beginning-of-line)
  (let* ((elm (cc-status-get-current-elm))
         (filename (cc-status-elm-filename elm)))
    (if (cc-status-elm-private elm)
        (error "Can't diff a view-private file.")
      (if arg
          (clearcase-ediff-file-with-version filename (clearcase-fprop-predecessor-version filename))
        (clearcase-ediff-file-with-version filename "/main/LATEST")))))

(defun cc-status-mark (&optional arg)
  "Mark a file.  With prefix argument, mark all relevent files."
  (interactive "P")
  (setq buffer-read-only nil)
  (beginning-of-line)
  (let ((op (downcase (string-to-char (substring (this-command-keys) -1)))))
    (if arg
        (save-excursion
          (cc-status-goto-first-file)
          (let (mark-it)
            (dolist (elm cc-status-elms)
              (cond ((= op ?p)
                     (setq mark-it (cc-status-elm-pull elm)))
                    ((= op ?m)
                     (setq mark-it (cc-status-elm-merge elm)))
                    ((= op ?i)
                     (setq mark-it (not (cc-status-elm-private elm))))
                    ((= op ?u)
                     (setq mark-it (not (cc-status-elm-private elm))))
                    ((= op ?r)
                     (setq mark-it (not (cc-status-elm-private elm))))
                    ((= op ?d)
                     (setq mark-it (cc-status-elm-private elm)))
                    (t
                     (setq mark-it t)))
              (when mark-it
                (delete-char 1)
                (insert (upcase op)))
              (cc-status-next-file))))
      (let ((elm (cc-status-get-current-elm)))
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
        (delete-char 1)
        (insert (upcase op))
        (cc-status-next-file)))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun cc-status-sync ()
  "Sync files."
  (let (filename)
    (dolist (elm cc-status-elms)
      (setq filename (cc-status-elm-filename elm))
      (when (find-buffer-visiting filename)
        (clearcase-sync-from-disk filename t)))))

(defun cc-status-pull (files)
  "Pull files."
  (when files
    (message "Pulling files ...")
    (cc-status-prep-output-buffer)
    (let ((cmd (concat "cpull " (mapconcat 'identity files " "))))
      (with-current-buffer cc-status-output-buffer-name
        (insert "--> " cmd "\n")
        (call-process-shell-command cmd nil t)
        (setq buffer-read-only t)))))

(defun cc-status-merge (files)
  "Merge files."
  (when files
    (message "Merging files ...")
    (cc-status-prep-output-buffer)
    (let (cmd)
      (with-current-buffer cc-status-output-buffer-name
        (dolist (filename files)
          (setq cmd (concat "cleartool merge -abort -to " filename " " filename "@@/main/LATEST"))
          (insert "--> " cmd "\n")
          (call-process-shell-command cmd nil t))
        (setq buffer-read-only t)))))

(defun cc-status-uncheckout-keep (files)
  "Uncheckout keeping files."
  (when files
    (message "Uncheckout keeping files ...")
    (cc-status-prep-output-buffer)
    (let ((cmd (concat "cleartool unco -keep " (mapconcat 'identity files " "))))
      (with-current-buffer cc-status-output-buffer-name
        (insert "--> " cmd "\n")
        (call-process-shell-command cmd nil t)
        (setq buffer-read-only t)))))

(defun cc-status-uncheckout-rm (files)
  "Uncheckout removing files."
  (when files
    (message "Uncheckout removing files ...")
    (cc-status-prep-output-buffer)
    (let ((cmd (concat "cleartool unco -rm " (mapconcat 'identity files " "))))
      (with-current-buffer cc-status-output-buffer-name
        (insert "--> " cmd "\n")
        (call-process-shell-command cmd nil t)
        (setq buffer-read-only t)))))

(defun cc-status-delete (files)
  "Delete files."
  (when files
    (message "Deleting files ...")
    (dolist (filename files)
      (if (file-directory-p filename)
          (delete-directory filename t)
        (delete-file filename)))))

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
        (cond ((= op ?P)
               (push (cc-status-elm-filename elm) files-to-pull))
              ((= op ?M)
               (push (cc-status-elm-filename elm) files-to-merge))
              ((= op ?I)
               (push (cc-status-elm-filename elm) cc-status-files-to-checkin))
              ((= op ?U)
               (push (cc-status-elm-filename elm) files-to-uncheckout-keep))
              ((= op ?R)
               (push (cc-status-elm-filename elm) files-to-uncheckout-rm))
              ((= op ?D)
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
      (cc-status-comment-mode)
      (message "Enter checkin comment for file(s) ..."))))

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
    (cc-status-prep-output-buffer)
    (let (pos cs-changes)
      (with-current-buffer cc-status-output-buffer-name
        (setq pos (point))
        (if (not comment)
            (let ((cmd (concat "cleartool ci -nc " (mapconcat 'identity files " "))))
              (insert "--> " cmd "\n")
              (call-process-shell-command cmd nil t))
          (let* ((temp-file (make-temp-file "cc-status-comment-"))
                 (cmd (concat "cleartool ci -cfile " temp-file " " (mapconcat 'identity files " "))))
            (with-temp-file temp-file
              (insert comment))
            (insert "--> " cmd "\n")
            (call-process-shell-command cmd nil t)
            (delete-file temp-file)))
        (goto-char pos)
        (while (re-search-forward "Checked in \"\\(.+?\\)\" version \"\\(.+?\\)\"" nil t)
          (setq cs-changes
                (concat cs-changes "element " (match-string-no-properties 1) " " (match-string-no-properties 2) "\n")))
        (setq buffer-read-only t)
        (goto-char (point-max)))
      (message "")
      cs-changes)))

(defun cc-status-update-config-spec (cs-changes)
  "Update the current config spec with the supplied changes."
  (message "Updating config spec ...")
  (let ((temp-file (make-temp-file "cc-status-config-spec-")))
    (with-temp-file temp-file
      (call-process-shell-command (concat "cleartool catcs -tag " clearcase-setview-viewtag) nil t)
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*element\\s-+[*]\\s-+CHECKEDOUT.*$" nil t)
        (forward-line 1)
        (insert "\n" cs-changes)
        (unless (looking-at "^\\s-*$")
          (insert "\n"))))
    (shell-command-to-string (concat "cleartool setcs -tag " clearcase-setview-viewtag " " temp-file))
    (clearcase-fprop-clear-all-properties)
    (delete-file temp-file)))

(defun cc-status-toggle-private ()
  "Toggle showing private files."
  (interactive)
  (setq cc-status-no-private (not cc-status-no-private))
  (cc-status-refresh))

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
    (define-key map "~" 'cc-status-toggle-private)

    (define-key map (kbd "SPC") 'cc-status-mark)
    (define-key map (kbd "p") 'cc-status-mark)
    (define-key map (kbd "P") 'cc-status-mark)
    (define-key map (kbd "m") 'cc-status-mark)
    (define-key map (kbd "M") 'cc-status-mark)
    (define-key map (kbd "i") 'cc-status-mark)
    (define-key map (kbd "I") 'cc-status-mark)
    (define-key map (kbd "u") 'cc-status-mark)
    (define-key map (kbd "U") 'cc-status-mark)
    (define-key map (kbd "r") 'cc-status-mark)
    (define-key map (kbd "R") 'cc-status-mark)
    (define-key map (kbd "d") 'cc-status-mark)
    (define-key map (kbd "D") 'cc-status-mark)
    (define-key map (kbd "x") 'cc-status-execute)
    (define-key map (kbd "X") 'cc-status-execute)

    (define-key map (kbd "M-<") 'cc-status-goto-first-file)
    (define-key map (kbd "M->") 'cc-status-goto-last-file)

    (define-key map (kbd "$") 'cc-status-display-output)

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
