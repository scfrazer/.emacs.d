;;; my-tmux.el

(defvar my-tmux-max-copy-length 5000
  "*Max length of string to copy to tmux in `my-interprogram-cut-function'.")

(defun my-interprogram-cut-function (text)
  "Don't copy if in a keyboard macro, and if in a tmux session also copy TEXT to a tmux buffer (if it's not too big)."
  (unless executing-kbd-macro
    (when (display-graphic-p)
      (x-select-text text))
    (when (and (getenv "TMUX")
               (<= (length text) my-tmux-max-copy-length))
      (my-tmux-copy text))))

(setq-default interprogram-cut-function 'my-interprogram-cut-function)

(defun my-tmux-copy-region (beg end)
  "Copy region to tmux buffer."
  (interactive "r")
  (my-tmux-copy (buffer-substring-no-properties beg end)))

(defun my-tmux-copy (text)
  "Copy text to tmux buffer."
  (call-process-shell-command
   (format "tmux set-buffer -- \"%s\""
           (replace-regexp-in-string
            "\\$" "\"'$'\""
            (replace-regexp-in-string
             "\\\"" "\"'\"'\""
             (replace-regexp-in-string
              "!" "\\\\!"
              (replace-regexp-in-string "\n" "\\\\\n" text))))) nil 0))

(provide 'my-tmux)
