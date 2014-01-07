;;; my-magit.el

(require 'magit)

(setq-default magit-ellipsis ?>
              magit-restore-window-configuration t
              magit-set-upstream-on-push 'askifnotset
              magit-status-buffer-switch-function 'switch-to-buffer
              magit-status-tags-line-subject 'tag
              magit-turn-on-auto-revert-mode nil)

(defun my-magit-file-log (&optional arg)
  "Show log for current file.
By default shows diff against previous commit.  With prefix arg, show
entire history.  With numeric prefix, show that many commits."
  (interactive "P")
  (let ((num-commits 1)
        (buf (get-buffer-create "*git-log*"))
        (filename (buffer-file-name)))
    (when arg
      (cond ((listp arg)
             (setq num-commits 0))
            ((numberp arg)
             (setq num-commits arg))))
    (with-current-buffer buf
      (erase-buffer)
      (shell-command (concat "git log -p --follow "
                             (if (> num-commits 0) (format "-%d " num-commits) "")
                             filename) t)
      (diff-mode))
    (pop-to-buffer buf)))

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
