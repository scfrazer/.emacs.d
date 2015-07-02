;;; my-magit.el

(require 'magit)
(require 'git-timemachine)

(setq-default magit-ellipsis ?>
              magit-restore-window-configuration t
              magit-set-upstream-on-push 'askifnotset
              magit-status-buffer-switch-function 'switch-to-buffer
              magit-turn-on-auto-revert-mode nil)

(setq-default git-commit-summary-max-length 120)

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

(define-key magit-mode-map (kbd "{") 'magit-goto-previous-sibling-section)
(define-key magit-mode-map (kbd "}") 'magit-goto-next-sibling-section)

(define-key git-commit-mode-map (kbd "C-x C-s") 'git-commit-commit)

(provide 'my-magit)
