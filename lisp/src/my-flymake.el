;;; my-flymake.el

(require 'flymake)
(require 'flymake-cursor)

(defun flymake-get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `flymake-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm flymake-allowed-file-name-masks)
        (mode-and-masks nil)
        (matcher nil))
    (while (and (not mode-and-masks) fnm)
      (setq matcher (car (car fnm)))
      (if (or (and (stringp matcher) (string-match matcher file-name))
              (and (symbolp matcher) (equal matcher major-mode)))
          (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))

(provide 'my-flymake)
