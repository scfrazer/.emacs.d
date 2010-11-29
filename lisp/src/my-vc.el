;;; my-vc.el

(defvar my-vc-handled-backends '(Git Hg)
  "*Handled backends")

(defun my-vc-activate ()
  (interactive)
  (unless vc-mode
    (let ((vc-handled-backends my-vc-handled-backends))
      (vc-find-file-hook))))

; TODO
; Show log for file (C-x v l) then:
; * ediff current vs. a given revision
; * Show a revision
; and kill *vc-log* buffer

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

(eval-after-load "vc-hooks"
  '(progn
     (define-key vc-prefix-map "=" 'my-vc-ediff)
     (define-key log-view-mode-map "=" 'my-vc-ediff)))

(provide 'my-vc)
