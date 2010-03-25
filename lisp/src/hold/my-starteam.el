;;; starteam.el

;; user variables

(defvar starteam-host "10.208.96.16" "*Host for StarTeam.")
(defvar starteam-port 49201 "*Port on starteam-host to talk to.")
(defvar starteam-executable "stcmd" "*Starteam command-line executable.")
(defvar starteam-view-alist
  '(
    ( "a380_ods/ohdu_sw"   . "s:/a380/ohdu_sw" )
    ( "a380_ods/ohdu_fpga" . "s:/a380/ohdu_fpga" )
    ( "a380_ods/ohdf_sw"   . "s:/a380/ohdf_sw" )
    ( "jsf/fodc_fpga"      . "s:/jsf/fodc_fpga" )
    )
  "*Map StarTeam views to working directories.")

;; mode variables

(defvar starteam-diff-buffer-name "*starteam-diff*")
(defvar starteam-view-buffer-name "starteam-view")
(defvar starteam-login-info nil)
(defvar starteam-current-view nil)
(defvar starteam-line-regexp "^[ \t]+\\(Current\\|Merge\\|Missing\\|Modified\\|Not in View\\|Out of Date\\|Unknown\\)[ \t]+\\(.+\\)$")

;; get view

(defun starteam-get-view ()
  "Get the StarTeam view."
  (let ((project-views (mapcar 'car starteam-view-alist)))
    (setq starteam-current-view
          (read-string (format "StarTeam Project View [%s]: "
                               starteam-current-view)
                       nil 'project-views starteam-current-view))))

;; get StarTeam login-info => user:passwd@host:port

(defun starteam-get-login-info ()
  "Get the login information for the StarTeam server."
  (interactive)
  (setq starteam-login-info
        (concat (read-string "Enter user name: " (user-login-name)) ":"
                (read-passwd "Enter password: ") "@"
                starteam-host ":" (int-to-string starteam-port))))

;; update StarTeam view

(defun starteam-update-view ()
  "Update StarTeam view."
  (interactive)
  (set-buffer starteam-view-buffer-name)
  (toggle-truncate-lines 1)
  (erase-buffer)
  (message "Updating StarTeam view ...")
  (call-process starteam-executable nil starteam-view-buffer-name nil
                "list" "-nologo" "-cmp" "-is" "-x" "-p"
                (concat starteam-login-info "/" starteam-current-view))
  (goto-char (point-min))
  (while (re-search-forward "^\\(Current    \\|Merge      \\|Missing    \\|Modified   \\|Not in View\\|Out of Date\\|Unknown    \\).*[ \t]+[0-9]+[ \t]+\\(.+\\)$" nil t)
    (replace-match "    \\1    \\2" nil nil))
  (goto-char (point-min))
  (re-search-forward starteam-line-regexp)
  (goto-char (match-beginning 2))
  (font-lock-fontify-buffer)
  (message ""))

;; get filename on line at point

(defun starteam-get-file-at-point ()
  "Get the filename on line at point."
  (save-excursion
    (beginning-of-line)
    (re-search-forward starteam-line-regexp)
    (match-string 2)))

;; get status of file on line at point

(defun starteam-get-file-status-at-point ()
  "Get the status of the file on line at point."
  (save-excursion
    (beginning-of-line)
    (re-search-forward starteam-line-regexp)
    (match-string 1)))

;; get directory associated with file on line at point

(defun starteam-get-assoc-dir-at-point ()
  "Get the directory associated with the file on line at point."
  (save-excursion
    (re-search-backward "^Folder:.*working dir:[ \t]+\\(.+\\)[)]")
    (replace-in-string (match-string 1) "\\\\" "/")))

;; Merge directory to current view

(defun starteam-dir-to-view (dir)
  "Map directory to the current view."
  (let ((base-dir (cdr (assoc starteam-current-view starteam-view-alist))))
    (string-match base-dir dir)
    (concat starteam-current-view (substring dir (match-end 0)))))

;; diff file against repository head revision

(defun starteam-diff ()
  "Diff the file on line at point against the head revision."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point))
        (status (starteam-get-file-status-at-point)))
    (if (not (equal status "Modified"))
        (message "Can only diff modified files.")
      (get-buffer-create starteam-diff-buffer-name)
      (set-buffer starteam-diff-buffer-name)
      (erase-buffer)
      (message "Doing diff of file against head revision ...")
      (call-process starteam-executable nil starteam-diff-buffer-name nil
                    "diff" "-nologo" "-cmp" "-n" "-q" "-x" "-p"
                    (concat starteam-login-info "/"
                            (starteam-dir-to-view directory)) " " filename)
      (goto-char (point-min))
      (if (not (re-search-forward "^-" nil t))
          (message "File is identical to head revision.")
        (beginning-of-line)
        (next-line 1)
        (delete-region 1 (point))
        (delete-matching-lines "^+")
        (while (re-search-forward "^[- ]\\(.*\\)$" nil t)
          (replace-match "\\1" nil nil))
        (re-search-forward "^-" nil t)
        (beginning-of-line)
        (delete-region (point) (point-max))
        (find-file (concat directory "/" filename))
        (ediff-buffers (buffer-name) starteam-diff-buffer-name)))))

;; edit the file on line at point

(defun starteam-edit ()
  "Edit file on line at point."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point)))
    (find-file (concat directory "/" filename))))

;; check-in the file on line at point

(defun starteam-checkin ()
  "Check-in file on line at point."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point))
        (status (starteam-get-file-status-at-point))
        (comment))
    (if (not (equal status "Modified"))
        (message "Can only check-in modified files.")
      (setq comment (read-string (format "Check-in comment for '%s': "
                                         filename)))
      (message (format "Checking in file '%s' ..." filename))
      (call-process starteam-executable nil starteam-diff-buffer-name nil
                    "ci" "-nologo" "-cmp" "-x" "-r" comment "-p"
                    (concat starteam-login-info "/"
                            (starteam-dir-to-view directory)) " " filename)
      (starteam-update-view))))

;; check-out the file on line at point

(defun starteam-checkout ()
  "Check-out file on line at point."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point)))
    (if (yes-or-no-p (format "Are you SURE you want to check out '%s'? "
                             filename))
        (progn
          (message (format "Checking out file '%s' ..." filename))
          (call-process starteam-executable nil starteam-diff-buffer-name nil
                        "co" "-nologo" "-cmp" "-x" "-o" "-p"
                        (concat starteam-login-info "/"
                                (starteam-dir-to-view directory)) " " filename)
          (starteam-update-view)))))

;; add the file on line at point

(defun starteam-add ()
  "Add file on line at point."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point))
        (status (starteam-get-file-status-at-point))
        (description))
    (if (not (equal status "Not in View"))
        (message "Can only add files that are not in the current view.")
      (if (yes-or-no-p (format "Are you SURE you want to add '%s'? "
                               filename))
          (progn
            (setq description (read-string (format "Description for '%s': "
                                                   filename)))
            (message (format "Adding file '%s' ..." filename))
            (call-process starteam-executable nil starteam-diff-buffer-name nil
                          "add" "-nologo" "-cmp" "-x" "-d" description "-p"
                          (concat starteam-login-info "/"
                            (starteam-dir-to-view directory)) " " filename)
            (starteam-update-view))))))

;; update the status of the file on line at point

(defun starteam-update-status ()
  "Update the status of the file on line at point."
  (interactive)
  (let ((filename (starteam-get-file-at-point))
        (directory (starteam-get-assoc-dir-at-point)))
    (message (format "Updating the status of '%s' ..." filename))
    (call-process starteam-executable nil starteam-diff-buffer-name nil
                  "update-status" "-nologo" "-cmp" "-x" "-contents" "-p"
                  (concat starteam-login-info "/"
                          (starteam-dir-to-view directory)) " " filename)
    (starteam-update-view)))

;; new view

(defun starteam-new-view ()
  "Set a new view."
  (interactive)
  (starteam-get-view)
  (starteam-update-view))

;; quit StarTeam buffer

(defun starteam-quit ()
  "Quit StarTeam."
  (interactive)
  (kill-buffer starteam-view-buffer-name))

;; start StarTeam

(defun starteam ()
  "Start (or switch to) StarTeam buffer."
  (interactive)
  (if (get-buffer starteam-view-buffer-name)
      (switch-to-buffer starteam-view-buffer-name)
    (starteam-get-view)
    (starteam-get-login-info)
    (get-buffer-create starteam-view-buffer-name)
    (switch-to-buffer starteam-view-buffer-name)
    (starteam-mode)
    (starteam-update-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'derived)

(defvar starteam-font-lock-keywords
  (list
   (cons "\\(^Folder:.*$\\)"
         'font-lock-builtin-face)
   (cons "\\(^[ \t]*Merge.*$\\)"
         'font-lock-string-face)
   (cons "\\(^[ \t]*Missing.*$\\)"
         'font-lock-warning-face)
   (cons "\\(^[ \t]*Modified.*$\\)"
         'font-lock-keyword-face)
   (cons "\\(^[ \t]*Not in View.*$\\)"
         'font-lock-comment-face)
   (cons "\\(^[ \t]*Out of Date.*$\\)"
         'font-lock-function-name-face)
   (cons "\\(^[ \t]*Unknown.*$\\)"
         'font-lock-preprocessor-face)
   )
  "StarTeam mode font-locking.")

(define-derived-mode starteam-mode fundamental-mode "StarTeam"
  "Mode for working with StarTeam."
  (set (make-local-variable 'font-lock-defaults)
       '(starteam-font-lock-keywords)))

(suppress-keymap starteam-mode-map)
(define-key starteam-mode-map "a" 'starteam-add)
(define-key starteam-mode-map "d" 'starteam-diff)
(define-key starteam-mode-map "g" 'starteam-update-view)
(define-key starteam-mode-map "i" 'starteam-checkin)
(define-key starteam-mode-map "o" 'starteam-checkout)
(define-key starteam-mode-map "q" 'starteam-quit)
(define-key starteam-mode-map "u" 'starteam-update-status)
(define-key starteam-mode-map "v" 'starteam-new-view)
(define-key starteam-mode-map [(return)] 'starteam-edit)

(provide 'my-starteam)
