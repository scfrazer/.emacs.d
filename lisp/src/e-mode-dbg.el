;;; e-mode-dbg.el

(require 'custom)
(require 'e-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup e-mode-dbg nil
  "*Specman mode debug."
  :group 'debug)

;;;###autoload
(defcustom e-mode-dbg-hook nil
  "*List of functions to call on entry to e-mode-dbg mode."
  :group 'e-mode-dbg
  :type 'hook)

;;;###autoload
(defcustom e-mode-dbg-ask-for-confirmation nil
  "*Ask for confirmation before doing 'destructive' things."
  :group 'e-mode-dbg
  :type 'boolean)

;;;###autoload
(defface e-mode-dbg-fringe-face
  '((t (:foreground "red")))
  "Face to highlight e-mode-dbg fringe markers"
  :group 'e-mode-dbg)

;;;###autoload
(defface e-mode-dbg-cond-fringe-face
  '((t (:foreground "yellow")))
  "Face to highlight e-mode-dbg conditional fringe markers"
  :group 'e-mode-dbg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar e-mode-dbg-buffer-name "*e-mode-dbg*"
  "Buffer name.")

(defvar e-mode-dbg-font-lock-keywords
  '(
    ("^\\(Breakpoints\\|Tags\\):"
     (0 'font-lock-string-face))
    ("^\\(.+\\):\\([0-9]+\\) \\(@\\) [0-9]+ .s \\(if\\) .+$"
     (1 'font-lock-function-name-face)
     (2 'font-lock-variable-name-face)
     (3 'font-lock-keyword-face)
     (4 'font-lock-keyword-face))
    ("^\\(.+\\):\\([0-9]+\\) \\(@\\) [0-9]+ .s$"
     (1 'font-lock-function-name-face)
     (2 'font-lock-variable-name-face)
     (3 'font-lock-keyword-face))
    ("^\\([a-zA-Z0-9_]+\\)=\\(.+\\) \\(@\\) [0-9]+ .s$"
     (1 'font-lock-constant-face)
     (2 'font-lock-type-face)
     (3 'font-lock-keyword-face))
    )
  "Font-lock-keywords.")

(defvar e-mode-dbg-target-buf nil
  "Buffer (verbose.txt) that is the target for all breakpoints, etc.")

(defvar e-mode-dbg-breakpoints nil
  "Breakpoints.")

(defvar e-mode-dbg-tags nil
  "Tags.")

(defvar e-mode-dbg-cond-history nil
  "Conditional breakpoint history.")

;; Fringe marker

(defconst e-mode-dbg-fringe-str "*e-mode-dbg*"
  "Dummy string to make fringe marker")

(defconst e-mode-dbg-cond-fringe-str "*e-mode-dbg*"
  "Dummy string to make fringe marker")

(define-fringe-bitmap 'e-mode-dbg-marker [#x3C #x7E #xFF #xFF #xFF #xFF #x7E #x3C])

(put-text-property 0 (length e-mode-dbg-fringe-str) 'display
                   (list 'left-fringe 'e-mode-dbg-marker 'e-mode-dbg-fringe-face)
                   e-mode-dbg-fringe-str)

(put-text-property 0 (length e-mode-dbg-cond-fringe-str) 'display
                   (list 'left-fringe 'e-mode-dbg-marker 'e-mode-dbg-cond-fringe-face)
                   e-mode-dbg-cond-fringe-str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;;; Breakpoints

(defun e-mode-dbg-mouse-toggle-breakpoint (ev)
  "Toggle breakpoint with mouse."
  (interactive "e")
  (mouse-set-point ev)
  (beginning-of-line)
  (e-mode-dbg-toggle-breakpoint))

(defun e-mode-dbg-mouse-conditional-breakpoint (ev)
  "Toggle conditional breakpoint with mouse."
  (interactive "e")
  (mouse-set-point ev)
  (beginning-of-line)
  (e-mode-dbg-toggle-breakpoint t))

(defun e-mode-dbg-toggle-breakpoint (&optional arg)
  "Toggle a breakpoint on the current line."
  (interactive "P")
  (if (not e-mode-dbg-target-buf)
      (error "No verbose.txt file loaded")
    (save-excursion
      (beginning-of-line)
      (let ((bpnt (e-mode-dbg-breakpoint-at (point))))
        (if bpnt
            (if arg
                (e-mode-dbg-set-breakpoint-condition-1 (file-name-nondirectory
                                                        (file-name-sans-extension (buffer-file-name)))
                                                       (line-number-at-pos)
                                                       (plist-get (overlay-properties bpnt)
                                                                  'e-mode-dbg-breakpoint-condition)
                                                       (plist-get (overlay-properties bpnt)
                                                                  'e-mode-dbg-breakpoint-time))
              (e-mode-dbg-remove-breakpoint bpnt))
          (let ((condition (when arg (read-from-minibuffer "Break if? " nil nil nil
                                                           e-mode-dbg-cond-history))))
            (e-mode-dbg-set-breakpoint condition "0 ns")))
        (e-mode-dbg-update-control-window-and-target)))))

(defun e-mode-dbg-breakpoint-at (pos)
  "Is there a breakpoint at POS?"
  (let ((ovls (overlays-at pos))
        before-string)
    (catch 'break
      (dolist (ovl ovls)
        (setq before-string (plist-get (overlay-properties ovl) 'before-string))
        (when (and before-string (string= before-string e-mode-dbg-fringe-str))
          (throw 'break ovl)))
      nil)))

(defun e-mode-dbg-set-breakpoint (condition time)
  "Set a breakpoint on the current line."
  (if (looking-at "[ \t]*\\(//\\|--\\|$\\)")
      (progn
        (message "Can't set a breakpoint on an empty or comment-only line")
        nil)
    (e-mode-dbg-add-overlay condition time)
    (e-mode-dbg-write-breakpoint condition time)))

(defun e-mode-dbg-add-overlay (condition time)
  "Add the breakpoint overlay"
  (let ((ovl (make-overlay (point-at-bol) (point-at-eol) nil t t)))
    (if (not condition)
        (overlay-put ovl 'before-string e-mode-dbg-fringe-str)
      (overlay-put ovl 'before-string e-mode-dbg-cond-fringe-str)
      (overlay-put ovl 'e-mode-dbg-breakpoint-condition condition))
    (overlay-put ovl 'e-mode-dbg-breakpoint-time time)
    (overlay-put ovl 'face '(:underline t))
    (overlay-put ovl 'evaporate t)))

(defun e-mode-dbg-write-breakpoint (condition time)
  "Write breakpoint to verbose.txt"
  (e-mode-dbg-write-breakpoint-1 (buffer-file-name) (line-number-at-pos) condition time))

(defun e-mode-dbg-write-breakpoint-1 (filename line-num condition time)
  "Do real write-breakpoint work."
  (push (list filename line-num condition time) e-mode-dbg-breakpoints)
  (with-current-buffer e-mode-dbg-target-buf
    (goto-char (point-max))
    (insert "change " time " break on line " (number-to-string line-num) " @"
            (file-name-sans-extension (file-name-nondirectory filename))
            (if condition
                (concat " if " condition)
              "")
            " -- " filename "\n"))
  (with-temp-buffer
    (when (equal this-command 'e-mode-dbg-set-breakpoint-condition)
      (clipboard-yank))
    (insert "break on line " (number-to-string line-num) " @"
            (file-name-sans-extension (file-name-nondirectory filename)))
    (when condition
      (insert " if " condition))
    (insert "\n")
    (clipboard-kill-region (point-min) (point-max))))

(defun e-mode-dbg-remove-breakpoint (bpnt)
  "Remove a breakpoint."
  (e-mode-dbg-remove-breakpoint-1 (buffer-file-name (overlay-buffer bpnt))
                                  (line-number-at-pos (overlay-start bpnt))
                                  (plist-get (overlay-properties bpnt) 'e-mode-dbg-breakpoint-condition)
                                  (plist-get (overlay-properties bpnt) 'e-mode-dbg-breakpoint-time))
  (delete-overlay bpnt))

(defun e-mode-dbg-remove-breakpoint-1 (filename line-num condition time)
  "Do real remove-breakpoint work."
  (with-current-buffer e-mode-dbg-target-buf
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^change [0-9]+ .s break on line " (number-to-string line-num) ".+" filename)
            nil t)
      (delete-region (point-at-bol) (1+ (point-at-eol)))))
  (setq e-mode-dbg-breakpoints (delete (list filename line-num condition time) e-mode-dbg-breakpoints))
  (with-temp-buffer
    (insert "delete break \"line " (number-to-string line-num) ".*"
            (file-name-sans-extension (file-name-nondirectory filename)) "\"\n")
    (clipboard-kill-region (point-min) (point-max))))

(defun e-mode-dbg-find-file-hook ()
  "Stuff to do when a file is loaded."
  ;; Is it a verbose.txt file?
  (if (and (buffer-file-name) (string= (file-name-nondirectory (buffer-file-name)) "verbose.txt"))
      (if e-mode-dbg-target-buf
          (message "WARNING: A verbose.txt file is already loaded, this one will be ignored")
        (setq e-mode-dbg-target-buf (current-buffer))
        (e-mode-dbg-parse-target-buffer)
        (dolist (buf (buffer-list))
          (when (and (buffer-file-name buf)
                     (string= (file-name-extension (buffer-file-name buf)) "e"))
            (e-mode-dbg-update-code-buffer buf))))
    ;; If it's a .e file and there is a target buffer, add any breakpoints
    (when (and e-mode-dbg-target-buf
               (string= (file-name-extension (buffer-file-name)) "e"))
      (e-mode-dbg-update-code-buffer (current-buffer)))))

(defun e-mode-dbg-parse-target-buffer ()
  "Parse the verbose.txt target buffer."
  (setq e-mode-dbg-breakpoints nil)
  (setq e-mode-dbg-tags nil)
  (with-current-buffer e-mode-dbg-target-buf
    ;; Breakpoints
    (goto-char (point-min))
    (while (re-search-forward
            "^change \\(.+\\) break on line \\([0-9]+\\) @[a-zA-Z0-9_]+ \\(if \\(.+\\) \\)?-- \\(.+\\)" nil t)
      (push (list (match-string-no-properties 5)
                  (string-to-number (match-string-no-properties 2))
                  (match-string-no-properties 4)
                  (match-string-no-properties 1))
            e-mode-dbg-breakpoints))
    ;; Tags
    (goto-char (point-min))
    (while (re-search-forward "^change \\(.+\\) set message -logger=sys.logger -add -verbosity=\\(.+\\) -tags={\\(.+\\)}" nil t)
      (push (list (match-string-no-properties 3)
                  (match-string-no-properties 2)
                  (match-string-no-properties 1)) e-mode-dbg-tags))))

(defun e-mode-dbg-update-code-buffer (buf)
  "Update a code buffer."
  (save-excursion
    (set-buffer buf)
    (let ((filename (buffer-file-name)))
      (dolist (bpnt e-mode-dbg-breakpoints)
        (when (string= (car bpnt) filename)
          (goto-line (nth 1 bpnt))
          (beginning-of-line)
          (e-mode-dbg-add-overlay (nth 2 bpnt) (nth 3 bpnt)))))))

(defun e-mode-dbg-after-save-hook ()
  "Stuff to do when a file is saved."
  (when (and e-mode-dbg-target-buf (string= (file-name-extension (buffer-file-name)) "e"))
    ;; Delete all breakpoints for this file from the breakpoint list and verbose.txt
    (let ((filename (buffer-file-name)))
      (with-current-buffer e-mode-dbg-target-buf
        (goto-char (point-min))
        (while (re-search-forward (concat "^change [0-9]+ .s break on line .+" filename) nil t)
          (delete-region (point-at-bol) (1+ (point-at-eol)))))
      (dolist (bpnt e-mode-dbg-breakpoints)
        (when (string= filename (car bpnt))
          (setq e-mode-dbg-breakpoints (delete bpnt e-mode-dbg-breakpoints)))))
    ;; Write the new and/or old breakpoints
    (let ((ovls (overlays-in (point-min) (point-max))))
      (dolist (ovl ovls)
        (when (e-mode-dbg-breakpoint-at (overlay-start ovl))
          (save-excursion
            (goto-char (overlay-start ovl))
            (e-mode-dbg-write-breakpoint
             (plist-get (overlay-properties ovl) 'e-mode-dbg-breakpoint-condition)
             (plist-get (overlay-properties ovl) 'e-mode-dbg-breakpoint-time))))))
    (e-mode-dbg-update-control-window-and-target)))

(defun e-mode-dbg-kill-buffer-hook ()
  "Stuff to do when a buffer is killed."
  (when (and e-mode-dbg-target-buf
             (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name)) "verbose.txt"))
    (setq e-mode-dbg-target-buf nil)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string= (file-name-extension (buffer-file-name buf)) "e"))
        (with-current-buffer buf
          (let ((ovls (overlays-in (point-min) (point-max)))
                before-string)
            (dolist (ovl ovls)
              (setq before-string (plist-get (overlay-properties ovl) 'before-string))
              (when (and before-string (string= before-string e-mode-dbg-fringe-str))
                (delete-overlay ovl)))))))))

(add-hook 'find-file-hook 'e-mode-dbg-find-file-hook)
(add-hook 'after-save-hook 'e-mode-dbg-after-save-hook)
(add-hook 'kill-buffer-hook 'e-mode-dbg-kill-buffer-hook)

;;; Tags

(defun e-mode-dbg-set-tag-verbosity ()
  "Set verbosity for current tag in verbose.txt"
  (interactive)
  (if (not e-mode-dbg-target-buf)
      (error "No verbose.txt file loaded")
    (let ((tag (thing-at-point 'symbol))
          (possible-levels (list "NONE" "LOW" "MEDIUM" "HIGH" "FULL"))
          level)
      (if (not tag)
          (message "Cursor must be on a tag.")
        (if (and (boundp 'ido-mode) ido-mode)
            (setq level (ido-completing-read "Level: " possible-levels nil t))
          (setq level (read-from-minibuffer "Level? ")))
        (if (not (member (upcase level) possible-levels))
            (error (concat "Level must be one of NONE, LOW, MEDIUM, HIGH, or FULL"))
          (e-mode-dbg-remove-tag tag)
          (e-mode-dbg-add-tag tag level "0 ns")
          (e-mode-dbg-update-control-window-and-target))))))

(defun e-mode-dbg-remove-tag-verbosity ()
  "Remove verbosity for current tag in verbose.txt"
  (interactive)
  (if (not e-mode-dbg-target-buf)
      (error "No verbose.txt file loaded")
    (let ((tag (thing-at-point 'symbol)))
      (if (not tag)
          (message "Cursor must be on a tag.")
        (e-mode-dbg-remove-tag tag)
        (e-mode-dbg-update-control-window-and-target)))))

(defun e-mode-dbg-remove-tag (tag)
  "Delete tag from verbose.txt"
  (with-current-buffer e-mode-dbg-target-buf
    (goto-char (point-min))
    (while (re-search-forward (concat "^change .+ set message -logger=sys.logger -add .+ -tags={" tag "}") nil t)
      (delete-region (point-at-bol) (1+ (point-at-eol)))))
  (dolist (tag-info e-mode-dbg-tags)
    (when (string= tag (car tag-info))
      (setq e-mode-dbg-tags (delete tag-info e-mode-dbg-tags))))
  (e-mode-dbg-tag-to-clipboard tag "MEDIUM"))

(defun e-mode-dbg-tag-to-clipboard (tag level)
  "Put a tag level on the clipboard."
  (with-temp-buffer
    (insert "set message -logger=sys.logger -add -verbosity=" (upcase level) " -tags={" tag "}\n")
    (clipboard-kill-region (point-min) (point-max))))

(defun e-mode-dbg-add-tag (tag level time)
  "Add a tag level to verbose.txt"
  (with-current-buffer e-mode-dbg-target-buf
    (goto-char (point-max))
    (insert "change " time " set message -logger=sys.logger -add -verbosity=" (upcase level) " -tags={" tag "}\n"))
  (push (list tag level time) e-mode-dbg-tags)
  (e-mode-dbg-tag-to-clipboard tag level))

;;; Control window

(defun e-mode-dbg-show-control-window ()
  "Show the debug control window."
  (interactive)
  (if (not e-mode-dbg-target-buf)
      (error "No verbose.txt file loaded")
    (let ((module (and (buffer-file-name)
                       (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
          (line-num (line-number-at-pos)))
      (e-mode-dbg-update-control-window)
      (select-window (split-window-vertically))
      (switch-to-buffer e-mode-dbg-buffer-name)
      (fit-window-to-buffer)
      (goto-char (point-min))
      (when module
        (re-search-forward (concat module ":" (number-to-string line-num)) nil t))
      (beginning-of-line)
      (e-mode-dbg-mode))))

(defun e-mode-dbg-update-control-window ()
  "Update the debug control window."
  (save-excursion
    (get-buffer-create e-mode-dbg-buffer-name)
    (set-buffer e-mode-dbg-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Breakpoints:\n")
    (setq e-mode-dbg-breakpoints
          (sort e-mode-dbg-breakpoints
                (lambda (x y)
                  (if (string= (car x) (car y))
                      (< (nth 1 x) (nth 1 y))
                    (string< (car x) (car y))))))
    (dolist (bpnt e-mode-dbg-breakpoints)
      (insert (file-name-sans-extension (file-name-nondirectory (car bpnt)))
              ":" (number-to-string (nth 1 bpnt))
              (if (nth 3 bpnt)
                  (concat " @ " (nth 3 bpnt))
                "")
              (if (nth 2 bpnt)
                  (concat " if " (nth 2 bpnt))
                "")
              "\n"))
    (insert "\n");
    (insert "Tags:\n")
    (let ((start (point)))
      (dolist (tag e-mode-dbg-tags)
        (insert (nth 0 tag) "=" (nth 1 tag) " @ " (nth 2 tag) "\n"))
      (sort-lines nil start (point)))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun e-mode-dbg-jump-to-breakpoint (&optional other-window)
  "Jump to a breakpoint."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\):\\(.+\\)"))
      (error "Cursor not on a breakpoint")
    ;; Breakpoints start on line 2
    (let* ((idx (- (line-number-at-pos) 2))
           (elt (nth idx e-mode-dbg-breakpoints))
           (filename (car elt))
           (line-num (nth 1 elt)))
      (if other-window
          (save-excursion
            (find-file-other-window filename)
            (goto-line line-num)
            (recenter)
            (other-window 1))
        (e-mode-dbg-quit)
        (find-file filename)
        (goto-line line-num)
        (recenter)))))

(defun e-mode-dbg-set-breakpoint-condition ()
  "Add/change/remove breakpoint condition."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\):\\([0-9]+\\) @ \\([0-9]+ .s\\)\\(\\( if \\(.+\\)\\)\\|$\\)"))
      (error "Cursor not on a breakpoint")
    (e-mode-dbg-set-breakpoint-condition-1 (match-string-no-properties 1)
                                           (string-to-number (match-string-no-properties 2))
                                           (match-string-no-properties 6)
                                           (match-string-no-properties 3))))

(defun e-mode-dbg-set-breakpoint-condition-1 (module line-num orig-condition time)
  "Do real work of set-breakpoint-condition."
  (let ((new-condition (read-from-minibuffer "Break if? " orig-condition nil nil e-mode-dbg-cond-history))
        filename)
    (when (string-match "^\\s-*$" new-condition)
      (setq new-condition nil))
    (catch 'break
      (dolist (bpnt e-mode-dbg-breakpoints)
        (when (and (string-match (concat ".+/" module "\\.e$") (car bpnt))
                   (= (nth 1 bpnt) line-num))
          (setq filename (car bpnt))
          (e-mode-dbg-remove-breakpoint-1 filename line-num orig-condition time)
          (setq this-command 'e-mode-dbg-set-breakpoint-condition)
          (e-mode-dbg-write-breakpoint-1 filename line-num new-condition time)
          (when (or (and (not orig-condition) new-condition)
                    (and orig-condition (not new-condition)))
            (with-current-buffer (get-file-buffer filename)
              (save-excursion
                (goto-line line-num)
                (delete-overlay (e-mode-dbg-breakpoint-at (point)))
                (e-mode-dbg-add-overlay new-condition time))))
          (e-mode-dbg-update-control-window-and-target)
          (throw 'break nil))))))

(defun e-mode-dbg-set-time ()
  "Set breakpoint or tag time."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\([0-9]+\\) @ \\([0-9]+ .s\\)\\(\\( if \\(.+\\)\\)\\|$\\)")
      (e-mode-dbg-set-breakpoint-time (match-string-no-properties 1)
                                      (string-to-number (match-string-no-properties 2))
                                      (match-string-no-properties 6)
                                      (match-string-no-properties 3))
    (if (looking-at "^\\([a-zA-Z0-9_]+\\)=\\(.+\\) \\(@\\) \\([0-9]+ .s\\)$")
        (e-mode-dbg-set-tag-time (match-string-no-properties 1)
                                 (match-string-no-properties 2)
                                 (match-string-no-properties 4))
      (error "Cursor not on a breakpoint or tag"))))

(defun e-mode-dbg-set-breakpoint-time (module line-num condition orig-time)
  "Set time for breakpoint."
  (let ((new-time (read-from-minibuffer "Time? " orig-time))
        filename)
    (unless (string-match "^[0-9]+ .s$" new-time)
      (error "Time format must be a number, a space, and (m|u|n|p|f)s"))
    (catch 'break
      (dolist (bpnt e-mode-dbg-breakpoints)
        (when (and (string-match (concat ".+/" module "\\.e$") (car bpnt))
                   (= (nth 1 bpnt) line-num))
          (setq filename (car bpnt))
          (e-mode-dbg-remove-breakpoint-1 filename line-num condition orig-time)
          (e-mode-dbg-write-breakpoint-1 filename line-num condition new-time)
          (with-current-buffer (get-file-buffer filename)
            (save-excursion
              (goto-line line-num)
              (delete-overlay (e-mode-dbg-breakpoint-at (point)))
              (e-mode-dbg-add-overlay condition new-time)))
          (e-mode-dbg-update-control-window-and-target)
          (throw 'break nil))))))

(defun e-mode-dbg-show-breakpoint-other-window ()
  "Show a breakpoint in the other window."
  (interactive)
  (e-mode-dbg-jump-to-breakpoint t))

(defun e-mode-dbg-raise-tag-verbosity (&optional lower)
  "Raise (or lower) the verbosity of current tag."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\)=\\(.+\\) @ \\([0-9]+ .s\\)"))
      (error "Cursor not on a tag")
    (let* ((tag (match-string-no-properties 1))
           (level (match-string-no-properties 2))
           (time (match-string-no-properties 3))
           (possible-levels (if lower
                                (list "FULL" "HIGH" "MEDIUM" "LOW" "NONE")
                              (list "NONE" "LOW" "MEDIUM" "HIGH" "FULL")))
           (level-elt (member level possible-levels)))
      (if (not (cdr level-elt))
          (error (concat "Tag level can't go " (if lower "lower" "higher")))
        (e-mode-dbg-remove-tag tag)
        (e-mode-dbg-add-tag tag (cadr level-elt) time)
        (e-mode-dbg-update-control-window-and-target)))))

(defun e-mode-dbg-lower-tag-verbosity ()
  "Lower the verbosity of current tag."
  (interactive)
  (e-mode-dbg-raise-tag-verbosity t))

(defun e-mode-dbg-set-tag-time (tag level orig-time)
  "Set time for tag."
  (let ((new-time (read-from-minibuffer "Time? " orig-time)))
    (unless (string-match "^[0-9]+ .s$" new-time)
      (error "Time format must be a number, a space, and (m|u|n|p|f)s"))
    (e-mode-dbg-remove-tag tag)
    (e-mode-dbg-add-tag tag level new-time)
    (e-mode-dbg-update-control-window-and-target)))

(defun e-mode-dbg-delete-breakpoint-or-tag ()
  "Delete the current breakpoint or tag."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\(.+\\)")
      ;; Breakpoints start on line 2
      (when (or (not e-mode-dbg-ask-for-confirmation) (y-or-n-p "Delete current breakpoint? "))
        (let* ((idx (- (line-number-at-pos) 2))
               (elt (nth idx e-mode-dbg-breakpoints))
               (buf (find-buffer-visiting (car elt))))
          (if buf
              (with-current-buffer buf
                (goto-line (nth 1 elt))
                (e-mode-dbg-remove-breakpoint (e-mode-dbg-breakpoint-at (point))))
            (setq e-mode-dbg-breakpoints (delete elt e-mode-dbg-breakpoints)))))
    (if (not (looking-at "\\(.+\\)=\\(.+\\)"))
        (error "Cursor not on a breakpoint or tag")
      (when (or (not e-mode-dbg-ask-for-confirmation) (y-or-n-p "Delete current tag? "))
        (e-mode-dbg-remove-tag (match-string-no-properties 1)))))
  (e-mode-dbg-update-control-window-and-target))

(defun e-mode-dbg-quit ()
  "Exit the debug window."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun e-mode-dbg-update-control-window-and-target ()
  "Update the control window and target buffer."
  (when e-mode-dbg-target-buf
    (with-current-buffer e-mode-dbg-target-buf
      (when (buffer-modified-p)
        (save-buffer)
        (message "")))
    (let ((buf (get-buffer e-mode-dbg-buffer-name)))
      (when buf
        (with-current-buffer buf
          (let ((pos (point))
                (win (get-buffer-window buf)))
            (e-mode-dbg-update-control-window)
            (goto-char (min pos (point-max)))
            (beginning-of-line)
            (when win
              (fit-window-to-buffer win))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(defvar e-mode-dbg-map nil "'e-mode-dbg' keymap.")
(if (not e-mode-dbg-map)
    (let ((map (make-keymap)))
      (define-key map (kbd "<return>") 'e-mode-dbg-jump-to-breakpoint)
      (define-key map "s" 'e-mode-dbg-show-breakpoint-other-window)
      (define-key map "c" 'e-mode-dbg-set-breakpoint-condition)
      (define-key map "t" 'e-mode-dbg-set-time)
      (define-key map "+" 'e-mode-dbg-raise-tag-verbosity)
      (define-key map "-" 'e-mode-dbg-lower-tag-verbosity)
      (define-key map "d" 'e-mode-dbg-delete-breakpoint-or-tag)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      (define-key map "q" 'e-mode-dbg-quit)
      (setq e-mode-dbg-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup

(define-key e-mode-map (kbd "<f10>") 'e-mode-dbg-toggle-breakpoint)
(define-key e-mode-map (kbd "S-<f10>") 'e-mode-dbg-set-tag-verbosity)
(define-key e-mode-map (kbd "C-<f10>") 'e-mode-dbg-show-control-window)
(define-key e-mode-map (kbd "M-<f10>") (lambda ()
                                         (interactive)
                                         (e-mode-dbg-toggle-breakpoint t)))

(define-key e-mode-map (kbd "<left-fringe> <mouse-1>") 'e-mode-dbg-mouse-toggle-breakpoint)
(define-key e-mode-map (kbd "<left-fringe> <M-mouse-1>") 'e-mode-dbg-mouse-conditional-breakpoint)

(defun e-mode-dbg-mode ()
  "e-mode-dbg is a mode for debugging Specman code.\n\n
\\{e-mode-dbg-map}"
  (kill-all-local-variables)
  (setq major-mode 'e-mode-dbg)
  (setq mode-name "e-mode-dbg")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map e-mode-dbg-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(e-mode-dbg-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'e-mode-dbg-hook))

(provide 'e-mode-dbg)
