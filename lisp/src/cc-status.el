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

(defstruct cc-status-elm filename private reserved mastered location successor)

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

(defun cc-status-refresh ()
  "Refresh the cc-status buffer."
  (interactive)
  (setq buffer-read-only nil)
  (message "Refreshing ClearCase status ...")
  (let (elms filename)
    ;; Private files
    (with-temp-buffer
      (call-process-shell-command "cleartool lspri -other" nil t)
      (goto-char (point-min))
      (while (not (eobp))
        (setq filename (buffer-substring-no-properties (point) (point-at-eol)))
        (unless (and cc-status-filter
                     (catch 'ignore
                       (dolist (regexp cc-status-ignore-regexps)
                         (when (string-match regexp filename)
                           (throw 'ignore t)))))
          (push (make-cc-status-elm :filename filename :private t) elms))
        (forward-line 1)))
    ;; Checked out files
    (let (elm user version reserved mastered view location master-replica latest-version successor)
      (with-temp-buffer
        (call-process-shell-command "cleartool lspri -co -short" nil t)
        (goto-char (point-min))
        (while (not (eobp))
          (setq filename (buffer-substring-no-properties (point) (point-at-eol)))
          (setq elm (make-cc-status-elm :filename filename))
          (with-temp-buffer
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
                      (setf (cc-status-elm-location elm) (concat "Needs to be pulled from " master-replica)))
                    (unless (string= version latest-version)
                      (setf (cc-status-elm-successor elm) (concat "Checked out version " version ", /main/LATEST is version " latest-version))))
                (when (string= reserved "reserved")
                  (setf (cc-status-elm-reserved elm) (concat "Reserved by " user " in view " view)))
                (when (string= mastered "mastered")
                  (setf (cc-status-elm-mastered elm) (concat "Mastered by " user " in view " view))))
              (forward-line 1)))
          (push elm elms)
          (forward-line 1))))
    ;; Update buffer
    (erase-buffer)
    (insert "ClearCase Status:\n\n")
    (insert "  PRMLS  Filename\n")
    (insert "  -----  ------------------------------------------------------------------------\n")
    (setq elms (sort elms #'(lambda (x y) (string< (cc-status-elm-filename x) (cc-status-elm-filename y)))))
    (dolist (elm elms)
      (insert "  "
              (if (cc-status-elm-private elm) "P" " ")
              (if (cc-status-elm-reserved elm) "R" " ")
              (if (cc-status-elm-mastered elm) "M" " ")
              (if (cc-status-elm-location elm) "L" " ")
              (if (cc-status-elm-successor elm) "S" " ")
              "  " (cc-status-elm-filename elm) "\n")))
  (cc-status-goto-first-file)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (message "Done"))

(defun cc-status-goto-first-file ()
  "Go to the first file."
  (interactive)
  (goto-char (point-min))
  (forward-line 4))

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
  (when (looking-at "\\s-+-")
    (forward-line 1)))

(defun cc-status-open-file ()
  "Open file."
  (interactive)
  (beginning-of-line)
  (when (looking-at "[^/]+\\(/.+\\)$")
    (find-file (match-string-no-properties 1))))

(defun cc-status-ediff ()
  "Ediff the current file."
  (interactive)
  (beginning-of-line)
  (if (looking-at ".  [^/]+\\(/.+\\)$")
      (let ((filename (match-string-no-properties 1)))
        (clearcase-ediff-file-with-version filename (clearcase-fprop-predecessor-version filename)))
    (error "Can't diff a view-private file.")))

;; TODO
(defun cc-status-show-status ()
  (interactive))

;; TODO
(defun cc-status-unreserve ()
  (interactive))
;;   (message (concat "Unreserving " filename " ..."))
;;   (let (view)
;;     (with-temp-buffer
;;       (insert (clearcase-ct-blocking-call "lsco" "-l" filename))
;;       (goto-char (point-min))
;;       (when (re-search-forward "(reserved)" nil t)
;;         (when (re-search-forward "by view:.+?:\\(.+?\\)\"" nil t)
;;           (setq view (match-string 1)))))
;;     (if (not view)
;;         (message (concat filename " is not reserved by anyone"))
;;       (clearcase-ct-blocking-call "unres" "-view" view filename)
;;       (let ((buf (get-file-buffer filename)))
;;         (when buf
;;           (with-current-buffer buf
;;             (revert-buffer nil t))))))
;;   (message ""))

;; TODO Pull mastership and sync
(defun cc-status-pull ()
  (interactive))

;; TODO
(defun cc-status-merge ()
  (interactive))

;; TODO
(defun cc-status-checkin ()
  (interactive))

;; TODO
(defun cc-status-uncheckout-keep ()
  (interactive))

;; TODO
(defun cc-status-uncheckout-rm ()
  (interactive))

;; TODO
(defun cc-status-mkelem ()
  (interactive))

;; TODO
(defun cc-status-delete ()
  (interactive))

(defun cc-status-toggle-mark ()
  "Mark/unmark a file."
  (interactive)
  (setq buffer-read-only nil)
  (beginning-of-line)
  (if (looking-at "\\*")
      (progn (delete-char 1)
             (insert " "))
    (delete-char 1)
    (insert "*"))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (cc-status-next-file))

(defun cc-status-unmark-all ()
  "Unmark file."
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (cc-status-goto-first-file)
    (while (not (eobp))
      (delete-char 1)
      (insert " ")
      (forward-line 1)))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-status-get-marked-files ()
  (let (files)
    (save-excursion
      (cc-status-goto-first-file)
      (while (not (eobp))
        (when (looking-at "\\*[^/]+\\(/.+\\)")
          (push (match-string-no-properties 1) files))))
    (unless files
      (when (looking-at "[^/]+\\(/.+\\)")
        (push (match-string-no-properties 1) files)))
    files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defconst cc-status-comment-buffer-name "*cc-status-comment*")
;; (defvar cc-status-prev-window-config nil)
;;
;; (defun cc-status-execute ()
;;   "Execute commands on marked files."
;;   (interactive)
;;   (when (y-or-n-p "Operate on marked files? ")
;;     (cc-status-goto-first-file)
;;     (if (not (save-excursion (re-search-forward "^[IA]" nil t)))
;;         (cc-status-do-operations)
;;       (setq cc-status-prev-window-config (current-window-configuration))
;;       (pop-to-buffer cc-status-comment-buffer-name)
;;       (cc-status-comment-mode))))
;;
;; (defun cc-status-do-operations (&optional comment)
;;   "Do marked operations."
;;   (cc-status-goto-first-file)
;;   (let (operation filename files-to-pull files-to-ci files-to-unco-keep files-to-unco-rm cs-changes)
;;     (while (not (eobp))
;;       (when (looking-at "\\([RSMIU!AD]\\)[^/]+\\(/.+\\)$")
;;         (setq operation (match-string-no-properties 1)
;;               filename (match-string-no-properties 2))
;;         (cond ((string= operation "R")
;;                (cc-status-unreserve filename))
;;               ((string= operation "S")
;;                nil) ;; TODO Merge with successor
;;               ((string= operation "M")
;;                (push filename files-to-pull))
;;               ((string= operation "I")
;;                (push filename files-to-ci))
;;               ((string= operation "U")
;;                (push filename files-to-unco-keep))
;;               ((string= operation "!")
;;                (push filename files-to-unco-rm))
;;               ((string= operation "A")
;;                nil) ;; TODO (clearcase-commented-mkelem filename t comment))
;;               ((string= operation "D")
;;                (delete-file filename))))
;;       (forward-line 1))
;;     (cc-status-pull files-to-pull)
;;     (setq cs-changes (cc-status-ci files-to-ci comment))
;;     (cc-status-unco files-to-unco-keep t)
;;     (cc-status-unco files-to-unco-rm nil)
;;     (cc-status-refresh)
;;     (when cs-changes
;;       (kill-new cs-changes)
;;       (clearcase-edcs-edit clearcase-setview-viewtag)
;;       (message "Some files are no longer visible, config spec changes are in the kill-ring."))))
;;
;; (defun cc-status-ci (files comment)
;;   "Check in files and gather config spec changes."
;;   (when files
;;     (message "Checking files in ...")
;;     (let (cs-changes)
;;       (with-temp-buffer
;;         (if (not comment)
;;             (insert (clearcase-ct-blocking-call "ci" "-nc" (mapconcat 'identity files " ")))
;;           (let ((temp-file (make-temp-file "cc-status-comment-")))
;;             (with-temp-file temp-file
;;               (insert comment))
;;             (insert (clearcase-ct-blocking-call "ci" "-cfile" temp-file (mapconcat 'identity files " ")))
;;             (delete-file temp-file)))
;;         (goto-char (point-min))
;;         ;; TODO Should look for file not visible warning
;;         (when (re-search-forward "Checked in \"\\(.+?\\)\" version \"\\(.+?\\)\"" nil t)
;;           (setq cs-changes
;;                 (concat cs-changes "element " (match-string-no-properties 1) " " (match-string-no-properties 2) "\n"))))
;;       (dolist (filename files)
;;         (when (find-buffer-visiting filename)
;;           (clearcase-sync-from-disk filename t)))
;;       (message "")
;;       cs-changes)))
;;
;; (defun cc-status-unco (files keep)
;;   "Uncheckout files."
;;   (when files
;;     (message (concat "Uncheckout files " (if keep "(keep)" "(rm)") " ..."))
;;     (clearcase-ct-blocking-call "unco" (if keep "-keep" "-rm") (mapconcat 'identity files " "))
;;     (dolist (filename files)
;;       (when (find-buffer-visiting filename)
;;         (clearcase-sync-from-disk filename t)))))
;;
;; (define-derived-mode cc-status-comment-mode text-mode "cc-status-comment-mode"
;;   "Add comments for checkin or mkelem."
;;   (local-set-key (kbd "C-x C-s") 'cc-status-comment-mode-finish))
;;
;; (defun cc-status-comment-mode-finish ()
;;   "Finish entering comment and do the operations."
;;   (interactive)
;;   (let ((comment (buffer-substring-no-properties (point-min) (point-max))))
;;     (when (string= comment "")
;;       (setq comment nil))
;;     (kill-buffer cc-status-comment-buffer-name)
;;     (when cc-status-prev-window-config
;;       (set-window-configuration cc-status-prev-window-config)
;;       (setq cc-status-prev-window-config nil))
;;     (cc-status-do-operations comment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keymap

(defvar cc-status-mode-map nil
  "`cc-status-mode' keymap.")

(unless cc-status-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" 'bury-buffer)
    (define-key map "g" 'cc-status-refresh)

    (define-key map "n" 'cc-status-next-file)
    (define-key map "p" 'cc-status-prev-file)
    (define-key map (kbd "C-n") 'cc-status-next-file)
    (define-key map (kbd "C-p") 'cc-status-prev-file)
    (define-key map (kbd "<down>") 'cc-status-next-file)
    (define-key map (kbd "<up>") 'cc-status-prev-file)

    (define-key map (kbd "RET") 'cc-status-open-file)
    (define-key map "=" 'cc-status-ediff)
    (define-key map "?" 'cc-status-show-status)

    (define-key map "r" 'cc-status-unreserve)
    (define-key map "p" 'cc-status-pull)
    (define-key map "m" 'cc-status-merge)
    (define-key map "i" 'cc-status-checkin)
    (define-key map "u" 'cc-status-uncheckout-keep)
    (define-key map "U" 'cc-status-uncheckout-rm)
    (define-key map "a" 'cc-status-mkelem)
    (define-key map "d" 'cc-status-delete)
    (define-key map (kbd "SPC") 'cc-status-toggle-mark)
    (define-key map "c" 'cc-status-unmark-all)

    (define-key map (kbd "M-<") 'cc-status-goto-first-file)
    (define-key map (kbd "M->") 'cc-status-goto-last-file)

    (setq cc-status-mode-map map)))

;; Font-lock

(defvar cc-status-mode-font-lock-keywords
  '(
    ("^  PRMLS  Filename"
     (0 'font-lock-keyword-face))
    ("^       .+"
     (0 'default))
    ("^  P.+"
     (0 'font-lock-comment-face))
    ("^   [RMLS ]+.+"
     (0 'font-lock-warning-face))
    ("^\\([*]\\) \\(P\\)      \\(.+\\)"
     (1 'dired-mark)
     (2 'font-lock-comment-face)
     (3 'dired-marked))
    ("^\\([*]\\)  \\(....\\)  \\(.+\\)"
     (1 'dired-mark)
     (2 'font-lock-warning-face)
     (3 'dired-marked))
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
