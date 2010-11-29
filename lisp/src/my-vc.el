;;; my-vc.el

(require 'ediff)

(defvar my-vc-handled-backends '(Git Hg)
  "*Handled backends")

(defun my-vc-activate ()
  (interactive)
  (unless vc-mode
    (let ((vc-handled-backends my-vc-handled-backends))
      (vc-find-file-hook))))

(defun my-vc-ediff (&optional rev)
  "Use ediff with vc."
  (interactive)
  (unless (buffer-file-name)
    (error "Current buffer is not visiting a file"))
  (when (and (buffer-modified-p)
             (y-or-n-p (message "Buffer %s is modified. Save buffer? " (buffer-name))))
    (save-buffer (current-buffer)))
  (my-vc-activate)
  (ediff-load-version-control)
  (ediff-vc-internal (or rev "") ""))

(defadvice vc-print-log (before my-vc-print-log activate)
  "Activate vc mode before getting the log."
  (my-vc-activate))

(defadvice vc-find-revision (after my-vc-find-revision activate)
  "Delete foo.~rev~ file after it is retreived, but not the buffer it went into."
  (let* ((file (ad-get-arg 0))
         (revision (ad-get-arg 1))
         (filename (vc-version-backup-file-name file revision 'manual)))
    (when (file-exists-p filename)
      (delete-file filename))))

(defadvice ediff-delete-version-file (around my-vc-ediff-delete-version-file activate)
  (let ((file (ad-get-arg 0)))
    (when (file-exists-p file)
      ad-do-it)))

(defun my-vc-log-view-ediff ()
  "ediff revision at point and current."
  (interactive)
  (let ((rev (log-view-current-tag)))
    (my-vc-log-view-quit)
    (my-vc-ediff rev)))

(defun my-vc-log-view-find-revision (pos)
  "Visit the version at point."
  (interactive "d")
  (unless log-view-per-file-logs
    (when (> (length log-view-vc-fileset) 1)
      (error "Multiple files shown in this buffer, cannot use this command here")))
  (goto-char pos)
  (let ((rev-buffer (vc-find-revision (if log-view-per-file-logs
                                          (log-view-current-file)
                                        (car log-view-vc-fileset))
                                      (log-view-current-tag)))
        (log-buffer (current-buffer)))
    (delete-window)
    (kill-buffer log-buffer)
    (switch-to-buffer rev-buffer)))

(defun my-vc-log-view-quit ()
  "Clean up vc buffers when quitting."
  (interactive)
  (delete-window)
  (kill-buffer "*vc-change-log*"))

(define-key magit-log-edit-mode-map (kbd "C-x C-s") 'magit-log-edit-commit)
(define-key magit-mode-map "q" 'my-magit-quit)

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'my-vc-ediff))

(defun my-log-view-mode-hook ()
  (define-key log-view-mode-map "=" 'my-vc-log-view-ediff)
  (define-key log-view-mode-map "f" 'my-vc-log-view-find-revision)
  (define-key log-view-mode-map "q" 'my-vc-log-view-quit))

(add-hook 'log-view-mode-hook 'my-log-view-mode-hook)

(provide 'my-vc)
