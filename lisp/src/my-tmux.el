;;; my-tmux.el

;; (defvar my-tmux-max-copy-length 5000
;;   "*Max length of string to copy to tmux in `my-interprogram-cut-function'.")
;;
;; (defun my-interprogram-cut-function (text)
;;   "Don't copy if in a keyboard macro, and if in a tmux session also copy TEXT to a tmux buffer (if it's not too big)."
;;   (unless executing-kbd-macro
;;     (when (display-graphic-p)
;;       (x-select-text text))
;;     (when (and (getenv "TMUX")
;;                (<= (length text) my-tmux-max-copy-length))
;;       (my-tmux-copy text))))
;; (setq-default interprogram-cut-function 'my-interprogram-cut-function)

(defun my-tmux-copy (&optional arg)
  "Copy last kill to tmux paste buffer.  With active region or
prefix arg, copy region."
  (interactive "P")
  (let ((use-region (or arg (use-region-p))))
    (my-tmux-copy-text
     (if use-region
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (current-kill 0 t))))
    (if use-region
        (progn
          (when (use-region-p)
            (deactivate-mark))
          (message "Copied region to tmux buffer"))
      (message "Copied last kill to tmux buffer"))))

(defun my-tmux-copy-text (text)
  "Copy text to tmux buffer."
  (call-process-shell-command
   (format "tmux set-buffer -- \"%s\""
           (replace-regexp-in-string
            "`" "\"'`'\""
            (replace-regexp-in-string
             "\\$" "\"'$'\""
             (replace-regexp-in-string
              "\\\"" "\"'\"'\""
              (replace-regexp-in-string
               "!" "\\\\!"
               (replace-regexp-in-string "\n" "\\\\\n" text)))))) nil 0))

(defun my-tmux-iterm-copy (&optional arg)
  "Copy last kill to clipboard through tmux and iterm.  With
active region or prefix arg, copy region."
  (interactive "P")
  (let ((use-region (or arg (use-region-p))))
    (my-tmux-iterm-copy-text
     (if use-region
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (current-kill 0 t))))
    (if use-region
        (progn
          (when (use-region-p)
            (deactivate-mark))
          (message "Copied region to clipboard"))
      (message "Copied last kill to clipboard"))))

(defun my-tmux-iterm-copy-text (text)
  "Copy text to clipboard through tmux and iterm"
  (interactive)
  (send-string-to-terminal
   (format "\033Ptmux;\033\033]50;CopyToClipboard\007%s\033\033]50;EndCopy\007\033\\" text))
  (redraw-display))

(provide 'my-tmux)
