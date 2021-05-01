;;; my-magit.el

(require 'magit)
(require 'git-timemachine)

(setq-default
 magit-commit-show-diff nil
 magit-display-buffer-function 'my-magit-display-buffer
 magit-ellipsis ?…
 magit-push-always-verify nil
 magit-restore-window-configuration t
 magit-set-upstream-on-push 'askifnotset)

(setq-default git-commit-summary-max-length 120)

(magit-auto-revert-mode -1)

(defun my-magit-history (&optional arg)
  "Show history for current file, either using git-timemachine
or, with C-u, magit-file-log."
  (interactive "P")
  (if arg
      (magit-log-buffer-file)
    (git-timemachine)))

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

(defun my-magit-display-buffer (buffer)
  "Derived from `magit-display-buffer-traditional'."
  (display-buffer
   buffer (if (memq (with-current-buffer buffer major-mode)
                    '(magit-process-mode
                      magit-revision-mode
                      magit-diff-mode
                      magit-stash-mode
                      magit-status-mode))
              '(display-buffer-same-window)
            nil))) ; display in another window

(define-key magit-mode-map (kbd "{") 'magit-goto-previous-sibling-section)
(define-key magit-mode-map (kbd "}") 'magit-goto-next-sibling-section)

(provide 'my-magit)
