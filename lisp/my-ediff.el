;;; my-ediff.el

(require 'ediff)
;; (require 'my-clearcase)

(setq-default ediff-ignore-similar-regions t
              ediff-keep-variants t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

(defun my-ediff-dwim (&optional arg)
  "If buffer is modified, diff against file.  If not modified,
either do ClearCase diff or Git diff depending on where the file is."
  (interactive "P")
  (if (buffer-modified-p)
      (my-ediff-buffer-with-file)

    ;; (if use-clearcase
    ;;     (if (string= (buffer-name) "*clearcase*")
    ;;         (my-clearcase-list-history-diff)
    ;;       (when (clearcase-file-is-in-mvfs-p (buffer-file-name))
    ;;         (my-clearcase-ediff-current arg)))
    ;;   (require 'my-vc)
    ;;   (my-vc-ediff))
    (require 'my-vc)
    (my-vc-ediff)))

(defun my-ediff-buffer-with-file (&optional arg)
  "View the differences between current buffer and it's associated file using ediff."
  (interactive "P")
  (let* ((ediff-ignore-similar-regions (not arg))
         (modified-buffer (current-buffer))
         (filename (or (buffer-file-name modified-buffer)
                       (error "Buffer %s has no associated file" modified-buffer)))
         (mode major-mode)
         (original-buffer (get-buffer-create "*my-ediff-buffer-with-file*")))
    (with-current-buffer original-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (call-interactively mode))
    (ediff-buffers original-buffer modified-buffer)))

(defadvice ediff-buffers (around my-ediff-buffers activate)
  "Compare buffers first and don't start ediff if they are identical."
  (let* ((buf-A (get-buffer (ad-get-arg 0)))
         (buf-A-file-name (buffer-file-name buf-A))
         (buf-B (get-buffer (ad-get-arg 1)))
         (buf-B-file-name (buffer-file-name buf-B))
         tmp-A-file-name tmp-B-file-name)
    (if (or (buffer-modified-p buf-A) (buffer-modified-p buf-B))
        ad-do-it
      (setq tmp-A-file-name (unless (and buf-A-file-name (file-exists-p buf-A-file-name))
                              (make-temp-file "buf-A-"))
            tmp-B-file-name (unless (and buf-B-file-name (file-exists-p buf-B-file-name))
                              (make-temp-file "buf-B-")))
      (when tmp-A-file-name
        (with-current-buffer buf-A
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) tmp-A-file-name))))
      (when tmp-B-file-name
        (with-current-buffer buf-B
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) tmp-B-file-name))))
      (if (ediff-same-file-contents (or tmp-A-file-name buf-A-file-name)
                                    (or tmp-B-file-name buf-B-file-name))
          (progn
            (dolist (buf (buffer-list))
              (when (string-match ".+\.~.+~$" (buffer-name buf))
                (kill-buffer buf)))
            (message "No differences"))
        ad-do-it)
      (when tmp-A-file-name
        (delete-file tmp-A-file-name))
      (when tmp-B-file-name
        (delete-file tmp-B-file-name)))))

(defadvice ediff-files (around my-ediff-files activate)
  "Compare files first and don't start ediff if they are identical."
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1)))
    (if (ediff-same-file-contents file-A file-B)
        (message "No differences")
      ad-do-it)))

(defun my-ediff-quit (reverse-default-keep-variants)
  "Don't ask if I really want to quit"
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (ediff-really-quit reverse-default-keep-variants))

(defvar my-ediff-window-config nil)

(defun my-ediff-before-setup-hook ()
  (setq my-ediff-window-config (current-window-configuration)))

(defun my-ediff-keymap-setup-hook ()
  (define-key ediff-mode-map "q" 'my-ediff-quit))

(defun my-ediff-quit-hook ()
  (set-window-configuration my-ediff-window-config)
  (let (buf (buf-list (buffer-list)))
    ;; Kill Clearcase revision buffers
    ;; (when use-clearcase
    ;;   (mapc (lambda (x)
    ;;           (when (and (buffer-name x) (string-match ".+?@@.+?$" (buffer-name x)))
    ;;             (unless (member (buffer-name x) my-clearcase-versioned-buffers-before-ediff)
    ;;               (kill-buffer x))))
    ;;         buf-list))
    ;; Kill vc revision buffers
    (mapc (lambda (x)
            (when (and (buffer-name x) (string-match "\\.~.+~$" (buffer-name x)))
              (kill-buffer x)))
          buf-list)
    ;; Kill p4 revision buffers
    ;; (mapc (lambda (x)
    ;;         (when (and (buffer-name x) (string-match "[*]P4 print .+have[*]$" (buffer-name x)))
    ;;           (kill-buffer x)))
    ;;       buf-list)
    ;; Cleanup ediff buffers
    (mapc (lambda (x)
            (when (setq buf (get-buffer x))
              (kill-buffer buf)))
          (list "*Ediff Control Panel*"
                "*Ediff Registry*"
                "*ediff-diff*"
                "*ediff-errors*"
                "*ediff-fine-diff*"
                "*my-ediff-buffer-with-file*"))))

(add-hook 'ediff-before-setup-hook 'my-ediff-before-setup-hook)
(add-hook 'ediff-keymap-setup-hook 'my-ediff-keymap-setup-hook)
(add-hook 'ediff-quit-hook 'my-ediff-quit-hook t)

(provide 'my-ediff)
