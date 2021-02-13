;;; my-bm.el  -*- lexical-binding: t; -*-

(require 'bm)
(require 'desktop)

(setq bm-goto-position nil
      bm-recenter t
      bm-wrap-immediately nil)

(defun my-bm-toggle-or-show (&optional arg)
  "Toggle or show bookmarks"
  (interactive "P")
  (if arg (bm-show-all) (bm-toggle)))

(provide 'my-bm)
