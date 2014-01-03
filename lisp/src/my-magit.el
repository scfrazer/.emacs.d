;;; my-magit.el

(require 'magit)

(setq-default magit-restore-window-configuration t
              magit-status-buffer-switch-function 'switch-to-buffer
              magit-status-tags-line-subject 'tag)

(defun my-magit-quit ()
  "Clean up magit buffers when quitting."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match "\\s-*\\*magit" (buffer-name buf))
      (kill-buffer buf))))

(defun magit-process-password-prompt (proc string)
  "Forward password prompts to the user."
  (let ((prompt (magit-process-match-prompt
                 magit-process-password-prompt-regexps string)))
    (when prompt
      (process-send-string proc (concat (let ((passwd-file "~/.magit-passwd"))
                                          (if (file-exists-p passwd-file)
                                              (with-temp-buffer
                                                (insert-file-contents passwd-file)
                                                (buffer-substring-no-properties (point-min) (point-max)))
                                            (read-passwd prompt))) "\n")))))

(define-key git-commit-mode-map (kbd "C-x C-s") 'git-commit-commit)

(provide 'my-magit)
