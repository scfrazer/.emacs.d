;;; bind-remind.el

(defvar bind-remind-key-alist '(
                                ("C-c d"   . "Debugging")
                                ("C-c y"   . "Yank to Target")
                                ("C-x 5"   . "Frames")
                                ("C-x C-k" . "Keyboard Macros")
                                ("C-x n"   . "Narrowing")
                                ("C-x r"   . "Rectangles and Registers")
                                ("C-x t"   . "Tasks")
                                ("C-x v"   . "ClearCase")
                                ("M-r"     . "Rectangles")
                                ("M-s"     . "Search")
                                )
  "Prefix keys to give binding reminders for.")

(defvar bind-remind-buf nil)

(defun bind-remind-show-bindings ()
  (let* ((keys (this-single-command-keys))
         (desc (key-description keys))
         (key-prefix (assoc desc bind-remind-key-alist)))
    (when key-prefix
      (let ((source-buf (current-buffer)))
        (setq bind-remind-buf (get-buffer-create "*bind-remind*"))
        (with-current-buffer bind-remind-buf
          (erase-buffer)
          (insert (cdr key-prefix) "\n"
                  (make-string (length (cdr key-prefix)) ?-) "\n")
          (save-excursion
            (describe-buffer-bindings source-buf keys))
          (bind-remind-reformat-buffer)
          (goto-char (point-min)))
        (fit-window-to-buffer
         (display-buffer-pop-up-window bind-remind-buf nil))))))

(defun bind-remind-reformat-buffer ()
  (let ((deleting t))
    (while (not (eobp))
      (if deleting
          (progn
            (setq deleting (not (looking-at "---             -------")))
            (delete-region (point) (point-at-bol 2)))
        (setq deleting (= (char-after) ?))
        (unless deleting
          (forward-line))))))

(defun bind-remind-close ()
  (when bind-remind-buf
    (delete-windows-on bind-remind-buf)
    (setq bind-remind-buf nil)))

(run-with-idle-timer 1.0 t 'bind-remind-show-bindings)
(add-hook 'pre-command-hook 'bind-remind-close)

(provide 'bind-remind)
