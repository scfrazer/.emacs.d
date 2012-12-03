;;; my-tmux.el

(require 'emamux)

(setq-default interprogram-cut-function 'my-interprogram-cut-function)

(defun my-interprogram-cut-function (text)
  "If in a tmux session, also copy TEXT to a tmux buffer."
  (when (getenv "TMUX")
    (emamux:set-buffer text 0))
  (x-select-text text))

(provide 'my-tmux)
