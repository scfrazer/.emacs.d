;;; my-magit.el

(require 'magit)

(defun my-magit-quit ()
  "Clean up magit buffers when quitting."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match "\\s-*\\*magit" (buffer-name buf))
      (kill-buffer buf))))

(defun my-magit-status ()
  "Don't split window."
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))

(defun magit-password (proc string)
  "Checks if git/ssh asks for a password and ask the user for it."
  (when (or (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
            (string-match "^\\\(.*\\\)'s password:" string))
    (let ((passwd-file "~/.magit-passwd"))
      (process-send-string proc
                           (if (file-exists-p passwd-file)
                               (with-temp-buffer
                                 (insert-file-contents passwd-file)
                                 (buffer-substring-no-properties (point-min) (point-max)))
                             (concat (read-passwd
                                      (format "Password for '%s': " (match-string 1 string))
                                      nil) "\n"))))))

;; (define-key magit-log-edit-mode-map (kbd "C-x C-s") 'magit-log-edit-commit)
(define-key magit-mode-map "q" 'my-magit-quit)

(provide 'my-magit)
