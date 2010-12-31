;;; hdl-dbg.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup hdl-dbg nil
  "*HDL debug."
  :group 'debug)

;;;###autoload
(defcustom hdl-dbg-hook nil
  "*List of functions to call on entry to hdl-dbg mode."
  :group 'hdl-dbg
  :type 'hook)

;;;###autoload
(defcustom hdl-dbg-ask-for-confirmation nil
  "*Ask for confirmation before doing 'destructive' things."
  :group 'hdl-dbg
  :type 'boolean)

;;;###autoload
(defface hdl-dbg-fringe-face
  '((t (:foreground "red" :background "black")))
  "Face to highlight hdl-dbg fringe markers"
  :group 'hdl-dbg)

;;;###autoload
(defface hdl-dbg-cond-fringe-face
  '((t (:foreground "yellow" :background "black")))
  "Face to highlight hdl-dbg conditional fringe markers"
  :group 'hdl-dbg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Things to be overridden in simulator extensions

(defvar hdl-dbg-parse-target-buffer-regexp nil
  "*Regexp to parse target buffer.
Important capture groups are in `hdl-dbg-parse-target-buffer-regexp-groups'")

(defvar hdl-dbg-parse-target-buffer-regexp-groups nil
  "*List of capture group numbers for `hdl-dbg-parse-target-buffer-regexp' that
correspond to filename line-num condition time.")

(defvar hdl-dbg-bpnt-not-allowed-fcn nil
  "*Function to call to see if a breakpoint is allowed at the current point
in the current buffer.  Point will be a the beginning of a line.")

(defvar hdl-dbg-bpnt-str-fcn nil
  "*Function to call to create a breakpoint string for the target buffer.
Arguments are filename line-num condition time.")

(defvar hdl-dbg-sim-bpnt-str-fcn nil
  "*Function to call to create a breakpoint string for the simulator.
Arguments are filename line-num condition time.")

(defvar hdl-dbg-bpnt-regexp-fcn nil
  "*Function to call to create a breakpoint regexp for the target buffer.
Arguments are filename line-num condition time.  If an arg is nil substitute an
empty string where it would be.")

(defvar hdl-dbg-sim-del-bpnt-str-fcn nil
  "*Function to call to create a delete breakpoint string for the simulator.
Arguments are filename line-num condition time.")

(defvar hdl-dbg-target-file-p-fcn nil
  "*Function to call to determine if a file is the target file.
Argument is filename.")

(defvar hdl-dbg-source-file-p-fcn nil
  "*Function to call to determine if a file is a source file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar hdl-dbg-buffer-name "*hdl-dbg*"
  "Buffer name.")

(defvar hdl-dbg-font-lock-keywords
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

(defvar hdl-dbg-target-buf nil
  "Buffer that is the target for all breakpoints, etc.")

(defvar hdl-dbg-breakpoints nil
  "Breakpoints.")

(defvar hdl-dbg-tags nil
  "Tags.")

(defvar hdl-dbg-cond-history nil
  "Conditional breakpoint history.")

;; Fringe marker

(defconst hdl-dbg-fringe-str "*hdl-dbg*"
  "Dummy string to make fringe marker")

(defconst hdl-dbg-cond-fringe-str "*hdl-dbg*"
  "Dummy string to make fringe marker")

(define-fringe-bitmap 'hdl-dbg-marker [#x3C #x7E #xFF #xFF #xFF #xFF #x7E #x3C])

(put-text-property 0 (length hdl-dbg-fringe-str) 'display
                   (list 'left-fringe 'hdl-dbg-marker 'hdl-dbg-fringe-face)
                   hdl-dbg-fringe-str)

(put-text-property 0 (length hdl-dbg-cond-fringe-str) 'display
                   (list 'left-fringe 'hdl-dbg-marker 'hdl-dbg-cond-fringe-face)
                   hdl-dbg-cond-fringe-str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun hdl-dbg-goto-line (line)
  "Goto LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

;;; Breakpoints

(defun hdl-dbg-mouse-toggle-breakpoint (ev)
  "Toggle breakpoint with mouse."
  (interactive "e")
  (mouse-set-point ev)
  (beginning-of-line)
  (hdl-dbg-toggle-breakpoint))

(defun hdl-dbg-mouse-conditional-breakpoint (ev)
  "Toggle conditional breakpoint with mouse."
  (interactive "e")
  (mouse-set-point ev)
  (beginning-of-line)
  (hdl-dbg-toggle-breakpoint t))

(defun hdl-dbg-toggle-breakpoint (&optional arg)
  "Toggle a breakpoint on the current line."
  (interactive "P")
  (if (not hdl-dbg-target-buf)
      (error "No targer file loaded")
    (save-excursion
      (beginning-of-line)
      (let ((bpnt (hdl-dbg-breakpoint-at (point))))
        (if bpnt
            (if arg
                (hdl-dbg-set-breakpoint-condition-1 (file-name-nondirectory
                                                     (file-name-sans-extension (buffer-file-name)))
                                                    (line-number-at-pos)
                                                    (plist-get (overlay-properties bpnt)
                                                               'hdl-dbg-breakpoint-condition)
                                                    (plist-get (overlay-properties bpnt)
                                                               'hdl-dbg-breakpoint-time))
              (hdl-dbg-remove-breakpoint bpnt))
          (let ((condition (when arg (read-from-minibuffer "Break if? " nil nil nil
                                                           hdl-dbg-cond-history))))
            (hdl-dbg-set-breakpoint condition "0 ns")))
        (hdl-dbg-update-control-window-and-target)))))

(defun hdl-dbg-breakpoint-at (pos)
  "Is there a breakpoint at POS?"
  (let ((ovls (overlays-at pos))
        before-string)
    (catch 'break
      (dolist (ovl ovls)
        (setq before-string (plist-get (overlay-properties ovl) 'before-string))
        (when (and before-string (string= before-string hdl-dbg-fringe-str))
          (throw 'break ovl)))
      nil)))

(defun hdl-dbg-set-breakpoint (condition time)
  "Set a breakpoint on the current line."
  (if (funcall hdl-dbg-bpnt-not-allowed-fcn)
      (progn
        (message "Can't set a breakpoint here (empty or comment-only line?)")
        nil)
    (hdl-dbg-add-overlay condition time)
    (hdl-dbg-write-breakpoint condition time)))

(defun hdl-dbg-add-overlay (condition time)
  "Add the breakpoint overlay"
  (let ((ovl (make-overlay (point-at-bol) (point-at-eol) nil t t)))
    (if (not condition)
        (overlay-put ovl 'before-string hdl-dbg-fringe-str)
      (overlay-put ovl 'before-string hdl-dbg-cond-fringe-str)
      (overlay-put ovl 'hdl-dbg-breakpoint-condition condition))
    (overlay-put ovl 'hdl-dbg-breakpoint-time time)
    (overlay-put ovl 'face '(:underline t))
    (overlay-put ovl 'evaporate t)))

(defun hdl-dbg-write-breakpoint (condition time)
  "Write breakpoint to target file."
  (hdl-dbg-write-breakpoint-1 (buffer-file-name) (line-number-at-pos) condition time))

(defun hdl-dbg-write-breakpoint-1 (filename line-num condition time)
  "Do real write-breakpoint work."
  (push (list filename line-num condition time) hdl-dbg-breakpoints)
  (with-current-buffer hdl-dbg-target-buf
    (goto-char (point-max))
    (insert (funcall hdl-dbg-bpnt-str-fcn filename line-num condition time)))
  (with-temp-buffer
    (when (equal this-command 'hdl-dbg-set-breakpoint-condition)
      (clipboard-yank))
    (insert (funcall hdl-dbg-sim-bpnt-str-fcn filename line-num condition time))
    (clipboard-kill-region (point-min) (point-max))))

(defun hdl-dbg-remove-breakpoint (bpnt)
  "Remove a breakpoint."
  (hdl-dbg-remove-breakpoint-1 (buffer-file-name (overlay-buffer bpnt))
                               (line-number-at-pos (overlay-start bpnt))
                               (plist-get (overlay-properties bpnt) 'hdl-dbg-breakpoint-condition)
                               (plist-get (overlay-properties bpnt) 'hdl-dbg-breakpoint-time))
  (delete-overlay bpnt))

(defun hdl-dbg-remove-breakpoint-1 (filename line-num condition time)
  "Do real remove-breakpoint work."
  (with-current-buffer hdl-dbg-target-buf
    (goto-char (point-min))
    (let ((bpnt-regexp (funcall hdl-dbg-bpnt-regexp-fcn filename line-num condition time)))
      (while (re-search-forward bpnt-regexp nil t)
        (delete-region (point-at-bol) (1+ (point-at-eol))))))
  (setq hdl-dbg-breakpoints (delete (list filename line-num condition time) hdl-dbg-breakpoints))
  (with-temp-buffer
    (insert (funcall hdl-dbg-sim-del-bpnt-str-fcn filename line-num condition time))
    (clipboard-kill-region (point-min) (point-max))))

(defun hdl-dbg-find-file-hook ()
  "Stuff to do when a file is loaded."
  ;; Is it a target file?
  (if (and (buffer-file-name) (funcall hdl-dbg-target-file-p-fcn (buffer-file-name)))
      (if hdl-dbg-target-buf
          (message "WARNING: A target file is already loaded, this one will be ignored")
        (setq hdl-dbg-target-buf (current-buffer))
        (hdl-dbg-parse-target-buffer)
        (dolist (buf (buffer-list))
          (when (and (buffer-file-name buf)
                     (funcall hdl-dbg-source-file-p-fcn (buffer-file-name buf)))
            (hdl-dbg-update-code-buffer buf))))
    ;; If it's a .e file and there is a target buffer, add any breakpoints
    (when (and hdl-dbg-target-buf
               (funcall hdl-dbg-source-file-p-fcn (buffer-file-name)))
      (hdl-dbg-update-code-buffer (current-buffer)))))

(defun hdl-dbg-parse-target-buffer ()
  "Parse the target buffer."
  (setq hdl-dbg-breakpoints nil)
  (setq hdl-dbg-tags nil)
  (with-current-buffer hdl-dbg-target-buf
    ;; Breakpoints
    (goto-char (point-min))
    (while (re-search-forward hdl-dbg-parse-target-buffer-regexp nil t)
      (push (list (match-string-no-properties
                   (nth 0 hdl-dbg-parse-target-buffer-regexp-groups))
                  (string-to-number (match-string-no-properties
                                     (nth 1 hdl-dbg-parse-target-buffer-regexp-groups)))
                  (match-string-no-properties
                   (nth 1 hdl-dbg-parse-target-buffer-regexp-groups))
                  (match-string-no-properties
                   (nth 1 hdl-dbg-parse-target-buffer-regexp-groups)))
            hdl-dbg-breakpoints))
    ;; Tags
    (goto-char (point-min))
    (while (re-search-forward "^change \\(.+\\) set message -logger=sys.logger -add -verbosity=\\(.+\\) -tags={\\(.+\\)}" nil t)
      (push (list (match-string-no-properties 3)
                  (match-string-no-properties 2)
                  (match-string-no-properties 1)) hdl-dbg-tags))))

(defun hdl-dbg-update-code-buffer (buf)
  "Update a code buffer."
  (with-current-buffer buf
    (let ((filename (buffer-file-name)))
      (dolist (bpnt hdl-dbg-breakpoints)
        (when (string= (car bpnt) filename)
          (hdl-dbg-goto-line (nth 1 bpnt))
          (beginning-of-line)
          (hdl-dbg-add-overlay (nth 2 bpnt) (nth 3 bpnt)))))))

(defun hdl-dbg-after-save-hook ()
  "Stuff to do when a file is saved."
  (when (and hdl-dbg-target-buf (funcall hdl-dbg-source-file-p-fcn (buffer-file-name)))
    ;; Delete all breakpoints for this file from the breakpoint list and target file
    (let ((filename (buffer-file-name)))
      (with-current-buffer hdl-dbg-target-buf
        (goto-char (point-min))
        (let ((bpnt-regexp (funcall hdl-dbg-bpnt-regexp-fcn filename)))
          (while (re-search-forward bpnt-regexp nil t)
            (delete-region (point-at-bol) (1+ (point-at-eol))))))
      (dolist (bpnt hdl-dbg-breakpoints)
        (when (string= filename (car bpnt))
          (setq hdl-dbg-breakpoints (delete bpnt hdl-dbg-breakpoints)))))
    ;; Write the new and/or old breakpoints
    (let ((ovls (overlays-in (point-min) (point-max))))
      (dolist (ovl ovls)
        (when (hdl-dbg-breakpoint-at (overlay-start ovl))
          (save-excursion
            (goto-char (overlay-start ovl))
            (hdl-dbg-write-breakpoint
             (plist-get (overlay-properties ovl) 'hdl-dbg-breakpoint-condition)
             (plist-get (overlay-properties ovl) 'hdl-dbg-breakpoint-time))))))
    (hdl-dbg-update-control-window-and-target)))

(defun hdl-dbg-kill-buffer-hook ()
  "Stuff to do when a buffer is killed."
  (when (and hdl-dbg-target-buf
             (buffer-file-name)
             (funcall hdl-dbg-target-file-p-fcn (buffer-file-name)))
    (setq hdl-dbg-target-buf nil)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (funcall hdl-dbg-source-file-p-fcn (buffer-file-name buf)))
        (with-current-buffer buf
          (let ((ovls (overlays-in (point-min) (point-max)))
                before-string)
            (dolist (ovl ovls)
              (setq before-string (plist-get (overlay-properties ovl) 'before-string))
              (when (and before-string (string= before-string hdl-dbg-fringe-str))
                (delete-overlay ovl)))))))))

(add-hook 'find-file-hook 'hdl-dbg-find-file-hook)
(add-hook 'after-save-hook 'hdl-dbg-after-save-hook)
(add-hook 'kill-buffer-hook 'hdl-dbg-kill-buffer-hook)

;;; Tags

(defun hdl-dbg-set-tag-verbosity ()
  "Set verbosity for current tag in target file."
  (interactive)
  (if (not hdl-dbg-target-buf)
      (error "No target file loaded")
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
          (hdl-dbg-remove-tag tag)
          (hdl-dbg-add-tag tag level "0 ns")
          (hdl-dbg-update-control-window-and-target))))))

(defun hdl-dbg-remove-tag-verbosity ()
  "Remove verbosity for current tag in target file"
  (interactive)
  (if (not hdl-dbg-target-buf)
      (error "No target file loaded")
    (let ((tag (thing-at-point 'symbol)))
      (if (not tag)
          (message "Cursor must be on a tag.")
        (hdl-dbg-remove-tag tag)
        (hdl-dbg-update-control-window-and-target)))))

(defun hdl-dbg-remove-tag (tag)
  "Delete tag from target file"
  (with-current-buffer hdl-dbg-target-buf
    (goto-char (point-min))
    (while (re-search-forward (concat "^change .+ set message -logger=sys.logger -add .+ -tags={" tag "}") nil t)
      (delete-region (point-at-bol) (1+ (point-at-eol)))))
  (dolist (tag-info hdl-dbg-tags)
    (when (string= tag (car tag-info))
      (setq hdl-dbg-tags (delete tag-info hdl-dbg-tags))))
  (hdl-dbg-tag-to-clipboard tag "MEDIUM"))

(defun hdl-dbg-tag-to-clipboard (tag level)
  "Put a tag level on the clipboard."
  (with-temp-buffer
    (insert "set message -logger=sys.logger -add -verbosity=" (upcase level) " -tags={" tag "}\n")
    (clipboard-kill-region (point-min) (point-max))))

(defun hdl-dbg-add-tag (tag level time)
  "Add a tag level to target file"
  (with-current-buffer hdl-dbg-target-buf
    (goto-char (point-max))
    (insert "change " time " set message -logger=sys.logger -add -verbosity=" (upcase level) " -tags={" tag "}\n"))
  (push (list tag level time) hdl-dbg-tags)
  (hdl-dbg-tag-to-clipboard tag level))

;;; Control window

(defun hdl-dbg-show-control-window ()
  "Show the debug control window."
  (interactive)
  (if (not hdl-dbg-target-buf)
      (error "No target file loaded")
    (let ((module (and (buffer-file-name)
                       (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
          (line-num (line-number-at-pos)))
      (hdl-dbg-update-control-window)
      (select-window (split-window-vertically))
      (switch-to-buffer hdl-dbg-buffer-name)
      (fit-window-to-buffer)
      (goto-char (point-min))
      (when module
        (re-search-forward (concat module ":" (number-to-string line-num)) nil t))
      (beginning-of-line)
      (hdl-dbg-mode))))

(defun hdl-dbg-update-control-window ()
  "Update the debug control window."
  (save-excursion
    (get-buffer-create hdl-dbg-buffer-name)
    (set-buffer hdl-dbg-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Breakpoints:\n")
    (setq hdl-dbg-breakpoints
          (sort hdl-dbg-breakpoints
                (lambda (x y)
                  (if (string= (car x) (car y))
                      (< (nth 1 x) (nth 1 y))
                    (string< (car x) (car y))))))
    (dolist (bpnt hdl-dbg-breakpoints)
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
      (dolist (tag hdl-dbg-tags)
        (insert (nth 0 tag) "=" (nth 1 tag) " @ " (nth 2 tag) "\n"))
      (sort-lines nil start (point)))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun hdl-dbg-jump-to-breakpoint (&optional other-window)
  "Jump to a breakpoint."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\):\\(.+\\)"))
      (error "Cursor not on a breakpoint")
    ;; Breakpoints start on line 2
    (let* ((idx (- (line-number-at-pos) 2))
           (elt (nth idx hdl-dbg-breakpoints))
           (filename (car elt))
           (line-num (nth 1 elt)))
      (if other-window
          (save-excursion
            (find-file-other-window filename)
            (hdl-dbg-goto-line line-num)
            (recenter)
            (other-window 1))
        (hdl-dbg-quit)
        (find-file filename)
        (hdl-dbg-goto-line line-num)
        (recenter)))))

(defun hdl-dbg-set-breakpoint-condition ()
  "Add/change/remove breakpoint condition."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\):\\([0-9]+\\) @ \\([0-9]+ .s\\)\\(\\( if \\(.+\\)\\)\\|$\\)"))
      (error "Cursor not on a breakpoint")
    (hdl-dbg-set-breakpoint-condition-1 (match-string-no-properties 1)
                                        (string-to-number (match-string-no-properties 2))
                                        (match-string-no-properties 6)
                                        (match-string-no-properties 3))))

(defun hdl-dbg-set-breakpoint-condition-1 (module line-num orig-condition time)
  "Do real work of set-breakpoint-condition."
  (let ((new-condition (read-from-minibuffer "Break if? " orig-condition nil nil hdl-dbg-cond-history))
        filename)
    (when (string-match "^\\s-*$" new-condition)
      (setq new-condition nil))
    (catch 'break
      (dolist (bpnt hdl-dbg-breakpoints)
        (when (and (string-match (concat ".+/" module "\\.e$") (car bpnt))
                   (= (nth 1 bpnt) line-num))
          (setq filename (car bpnt))
          (hdl-dbg-remove-breakpoint-1 filename line-num orig-condition time)
          (setq this-command 'hdl-dbg-set-breakpoint-condition)
          (hdl-dbg-write-breakpoint-1 filename line-num new-condition time)
          (when (or (and (not orig-condition) new-condition)
                    (and orig-condition (not new-condition)))
            (with-current-buffer (get-file-buffer filename)
              (save-excursion
                (hdl-dbg-goto-line line-num)
                (delete-overlay (hdl-dbg-breakpoint-at (point)))
                (hdl-dbg-add-overlay new-condition time))))
          (hdl-dbg-update-control-window-and-target)
          (throw 'break nil))))))

(defun hdl-dbg-set-time ()
  "Set breakpoint or tag time."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\([0-9]+\\) @ \\([0-9]+ .s\\)\\(\\( if \\(.+\\)\\)\\|$\\)")
      (hdl-dbg-set-breakpoint-time (match-string-no-properties 1)
                                   (string-to-number (match-string-no-properties 2))
                                   (match-string-no-properties 6)
                                   (match-string-no-properties 3))
    (if (looking-at "^\\([a-zA-Z0-9_]+\\)=\\(.+\\) \\(@\\) \\([0-9]+ .s\\)$")
        (hdl-dbg-set-tag-time (match-string-no-properties 1)
                              (match-string-no-properties 2)
                              (match-string-no-properties 4))
      (error "Cursor not on a breakpoint or tag"))))

(defun hdl-dbg-set-breakpoint-time (module line-num condition orig-time)
  "Set time for breakpoint."
  (let ((new-time (read-from-minibuffer "Time? " orig-time))
        filename)
    (unless (string-match "^[0-9]+ .s$" new-time)
      (error "Time format must be a number, a space, and (m|u|n|p|f)s"))
    (catch 'break
      (dolist (bpnt hdl-dbg-breakpoints)
        (when (and (string-match (concat ".+/" module "\\.e$") (car bpnt))
                   (= (nth 1 bpnt) line-num))
          (setq filename (car bpnt))
          (hdl-dbg-remove-breakpoint-1 filename line-num condition orig-time)
          (hdl-dbg-write-breakpoint-1 filename line-num condition new-time)
          (with-current-buffer (get-file-buffer filename)
            (save-excursion
              (hdl-dbg-goto-line line-num)
              (delete-overlay (hdl-dbg-breakpoint-at (point)))
              (hdl-dbg-add-overlay condition new-time)))
          (hdl-dbg-update-control-window-and-target)
          (throw 'break nil))))))

(defun hdl-dbg-show-breakpoint-other-window ()
  "Show a breakpoint in the other window."
  (interactive)
  (hdl-dbg-jump-to-breakpoint t))

(defun hdl-dbg-raise-tag-verbosity (&optional lower)
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
        (hdl-dbg-remove-tag tag)
        (hdl-dbg-add-tag tag (cadr level-elt) time)
        (hdl-dbg-update-control-window-and-target)))))

(defun hdl-dbg-lower-tag-verbosity ()
  "Lower the verbosity of current tag."
  (interactive)
  (hdl-dbg-raise-tag-verbosity t))

(defun hdl-dbg-set-tag-time (tag level orig-time)
  "Set time for tag."
  (let ((new-time (read-from-minibuffer "Time? " orig-time)))
    (unless (string-match "^[0-9]+ .s$" new-time)
      (error "Time format must be a number, a space, and (m|u|n|p|f)s"))
    (hdl-dbg-remove-tag tag)
    (hdl-dbg-add-tag tag level new-time)
    (hdl-dbg-update-control-window-and-target)))

(defun hdl-dbg-delete-breakpoint-or-tag ()
  "Delete the current breakpoint or tag."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\(.+\\)")
      ;; Breakpoints start on line 2
      (when (or (not hdl-dbg-ask-for-confirmation) (y-or-n-p "Delete current breakpoint? "))
        (let* ((idx (- (line-number-at-pos) 2))
               (elt (nth idx hdl-dbg-breakpoints))
               (buf (find-buffer-visiting (car elt))))
          (if buf
              (with-current-buffer buf
                (hdl-dbg-goto-line (nth 1 elt))
                (hdl-dbg-remove-breakpoint (hdl-dbg-breakpoint-at (point))))
            (setq hdl-dbg-breakpoints (delete elt hdl-dbg-breakpoints)))))
    (if (not (looking-at "\\(.+\\)=\\(.+\\)"))
        (error "Cursor not on a breakpoint or tag")
      (when (or (not hdl-dbg-ask-for-confirmation) (y-or-n-p "Delete current tag? "))
        (hdl-dbg-remove-tag (match-string-no-properties 1)))))
  (hdl-dbg-update-control-window-and-target))

(defun hdl-dbg-quit ()
  "Exit the debug window."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun hdl-dbg-update-control-window-and-target ()
  "Update the control window and target buffer."
  (when hdl-dbg-target-buf
    (with-current-buffer hdl-dbg-target-buf
      (when (buffer-modified-p)
        (save-buffer)
        (message "")))
    (let ((buf (get-buffer hdl-dbg-buffer-name)))
      (when buf
        (with-current-buffer buf
          (let ((pos (point))
                (win (get-buffer-window buf)))
            (hdl-dbg-update-control-window)
            (goto-char (min pos (point-max)))
            (beginning-of-line)
            (when win
              (fit-window-to-buffer win))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(defvar hdl-dbg-map nil "'hdl-dbg' keymap.")
(if (not hdl-dbg-map)
    (let ((map (make-keymap)))
      (define-key map (kbd "<return>") 'hdl-dbg-jump-to-breakpoint)
      (define-key map "s" 'hdl-dbg-show-breakpoint-other-window)
      (define-key map "c" 'hdl-dbg-set-breakpoint-condition)
      (define-key map "t" 'hdl-dbg-set-time)
      (define-key map "+" 'hdl-dbg-raise-tag-verbosity)
      (define-key map "-" 'hdl-dbg-lower-tag-verbosity)
      (define-key map "d" 'hdl-dbg-delete-breakpoint-or-tag)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      (define-key map "q" 'hdl-dbg-quit)
      (setq hdl-dbg-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup

(defun hdl-dbg-mode ()
  "hdl-dbg is a mode for debugging Specman code.\n\n
\\{hdl-dbg-map}"
  (kill-all-local-variables)
  (setq major-mode 'hdl-dbg)
  (setq mode-name "hdl-dbg")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map hdl-dbg-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hdl-dbg-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'hdl-dbg-hook))

(provide 'hdl-dbg)
