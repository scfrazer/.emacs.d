;;; hdl-dbg.el

;; TODO Tags

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

(defvar hdl-dbg-filename-to-module-fcn nil
  "*Function to call to convert a filename into a 'module' name.
This is just a shortened name to display in the control window.")

(defvar hdl-dbg-module-eq-filename-fcn nil
  "*Function to call to compare a 'module' name to a filename.
Arguments are module filename.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar hdl-dbg-buffer-name "*hdl-dbg*"
  "Buffer name.")

(defvar hdl-dbg-no-target-buffer-name "*hdl-dbg-target*"
  "If there is no target, use a buffer with this name.")

(defvar hdl-dbg-font-lock-keywords
  '(
    ("^\\(Breakpoints\\):"
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
    )
  "Font-lock-keywords.")

(defvar hdl-dbg-target-buf nil
  "Buffer that is the target for all breakpoints, etc.")

(defvar hdl-dbg-breakpoints nil
  "Breakpoints.")

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

(defun hdl-dbg-line-number-at-pos (&optional pos)
  "Absolute line-number-at-pos"
  (save-restriction
    (widen)
    (line-number-at-pos pos)))

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
  (hdl-dbg-ensure-target-buffer)
  (save-excursion
    (beginning-of-line)
    (let ((bpnt (hdl-dbg-breakpoint-at (point))))
      (if bpnt
          (if arg
              (hdl-dbg-set-breakpoint-condition-1 (funcall
                                                   hdl-dbg-filename-to-module-fcn
                                                   (buffer-file-name))
                                                  (hdl-dbg-line-number-at-pos)
                                                  (plist-get (overlay-properties bpnt)
                                                             'hdl-dbg-breakpoint-condition)
                                                  (plist-get (overlay-properties bpnt)
                                                             'hdl-dbg-breakpoint-time))
            (hdl-dbg-remove-breakpoint bpnt))
        (let ((condition (when arg (read-from-minibuffer "Break if? " nil nil nil
                                                         hdl-dbg-cond-history))))
          (hdl-dbg-set-breakpoint condition "0 ns")))
      (hdl-dbg-update-control-window-and-target))))

(defun hdl-dbg-ensure-target-buffer ()
  "If a target buffer doesn't exist, create one."
  (unless hdl-dbg-target-buf
    (setq hdl-dbg-target-buf (get-buffer-create hdl-dbg-no-target-buffer-name))))

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
  (hdl-dbg-write-breakpoint-1 (buffer-file-name) (hdl-dbg-line-number-at-pos) condition time))

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
                               (hdl-dbg-line-number-at-pos (overlay-start bpnt))
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
    ;; If it's a source file and there is a target buffer, add any breakpoints
    (when (and hdl-dbg-target-buf
               (funcall hdl-dbg-source-file-p-fcn (buffer-file-name)))
      (hdl-dbg-update-code-buffer (current-buffer)))))

(defun hdl-dbg-parse-target-buffer ()
  "Parse the target buffer."
  (setq hdl-dbg-breakpoints nil)
  (with-current-buffer hdl-dbg-target-buf
    ;; Breakpoints
    (goto-char (point-min))
    (while (re-search-forward hdl-dbg-parse-target-buffer-regexp nil t)
      (push (list (match-string-no-properties
                   (nth 0 hdl-dbg-parse-target-buffer-regexp-groups))
                  (string-to-number (match-string-no-properties
                                     (nth 1 hdl-dbg-parse-target-buffer-regexp-groups)))
                  (match-string-no-properties
                   (nth 2 hdl-dbg-parse-target-buffer-regexp-groups))
                  (match-string-no-properties
                   (nth 3 hdl-dbg-parse-target-buffer-regexp-groups)))
            hdl-dbg-breakpoints))))

(defun hdl-dbg-update-code-buffer (buf)
  "Update a code buffer."
  (save-excursion
    (with-current-buffer buf
      (let ((filename (buffer-file-name)))
        (dolist (bpnt hdl-dbg-breakpoints)
          (when (string= (car bpnt) filename)
            (hdl-dbg-goto-line (nth 1 bpnt))
            (beginning-of-line)
            (hdl-dbg-add-overlay (nth 2 bpnt) (nth 3 bpnt))))))))

(defun hdl-dbg-copy-all-to-clipboard ()
  "Copy all breakpoints to clipboard."
  (interactive)
  (when hdl-dbg-target-buf
    (with-current-buffer hdl-dbg-target-buf
      (clipboard-kill-region (point-min) (point-max)))))

(defun hdl-dbg-after-save-hook ()
  "Stuff to do when a file is saved."
  (when (and hdl-dbg-target-buf (funcall hdl-dbg-source-file-p-fcn (buffer-file-name)))
    ;; Delete all breakpoints for this file from the breakpoint list and target file
    (let ((filename (buffer-file-name)))
      (with-current-buffer hdl-dbg-target-buf
        (goto-char (point-min))
        (let ((bpnt-regexp (funcall hdl-dbg-bpnt-regexp-fcn filename nil nil nil)))
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
             (or (and (buffer-file-name) (funcall hdl-dbg-target-file-p-fcn (buffer-file-name)))
                 (equal (buffer-name) hdl-dbg-no-target-buffer-name)))
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

;;; Control window

(defun hdl-dbg-show-control-window ()
  "Show the debug control window."
  (interactive)
  (hdl-dbg-ensure-target-buffer)
  (let ((module (and (buffer-file-name)
                     (funcall hdl-dbg-filename-to-module-fcn (buffer-file-name))))
        (line-num (hdl-dbg-line-number-at-pos)))
    (hdl-dbg-update-control-window)
    (select-window (split-window-vertically))
    (switch-to-buffer hdl-dbg-buffer-name)
    (fit-window-to-buffer)
    (goto-char (point-min))
    (if module
        (if (re-search-forward (concat module ":" (number-to-string line-num)) nil t)
            (beginning-of-line)
          (forward-line 1))
      (forward-line 1))
    (hdl-dbg-mode)))

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
      (insert (funcall hdl-dbg-filename-to-module-fcn (car bpnt))
              ":" (number-to-string (nth 1 bpnt))
              (if (nth 3 bpnt)
                  (concat " @ " (nth 3 bpnt))
                "")
              (if (nth 2 bpnt)
                  (concat " if " (nth 2 bpnt))
                "")
              "\n"))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun hdl-dbg-jump-to-breakpoint (&optional other-window)
  "Jump to a breakpoint."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\(.+\\):\\(.+\\)"))
      (error "Cursor not on a breakpoint")
    ;; Breakpoints start on line 2
    (let* ((idx (- (hdl-dbg-line-number-at-pos) 2))
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
        (when (and (funcall hdl-dbg-module-eq-filename-fcn module (car bpnt))
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
  "Set breakpoint time."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\([0-9]+\\) @ \\([0-9]+ .s\\)\\(\\( if \\(.+\\)\\)\\|$\\)")
      (hdl-dbg-set-breakpoint-time (match-string-no-properties 1)
                                   (string-to-number (match-string-no-properties 2))
                                   (match-string-no-properties 6)
                                   (match-string-no-properties 3))
    (error "Cursor not on a breakpoint")))

(defun hdl-dbg-set-breakpoint-time (module line-num condition orig-time)
  "Set time for breakpoint."
  (let ((new-time (read-from-minibuffer "Time? " orig-time))
        filename)
    (unless (string-match "^[0-9]+ .s$" new-time)
      (error "Time format must be a number, a space, and (m|u|n|p|f)s"))
    (catch 'break
      (dolist (bpnt hdl-dbg-breakpoints)
        (when (and (funcall hdl-dbg-module-eq-filename-fcn module (car bpnt))
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

(defun hdl-dbg-delete-breakpoint ()
  "Delete the current breakpoint."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(.+\\):\\(.+\\)")
      ;; Breakpoints start on line 2
      (when (or (not hdl-dbg-ask-for-confirmation) (y-or-n-p "Delete current breakpoint? "))
        (let* ((idx (- (hdl-dbg-line-number-at-pos) 2))
               (elt (nth idx hdl-dbg-breakpoints))
               (buf (find-buffer-visiting (car elt))))
          (if buf
              (with-current-buffer buf
                (hdl-dbg-goto-line (nth 1 elt))
                (hdl-dbg-remove-breakpoint (hdl-dbg-breakpoint-at (point))))
            (setq hdl-dbg-breakpoints (delete elt hdl-dbg-breakpoints)))))
    (error "Cursor not on a breakpoint"))
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
        (if (buffer-file-name)
            (progn
              (save-buffer)
              (message ""))
          (set-buffer-modified-p nil))))
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
      (define-key map (kbd "RET") 'hdl-dbg-jump-to-breakpoint)
      (define-key map "s" 'hdl-dbg-show-breakpoint-other-window)
      (define-key map "c" 'hdl-dbg-set-breakpoint-condition)
      (define-key map "t" 'hdl-dbg-set-time)
      (define-key map "d" 'hdl-dbg-delete-breakpoint)
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
