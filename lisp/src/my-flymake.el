;;; my-flymake.el

(require 'flymake)

(setq-default flymake-start-syntax-check-on-find-file nil)

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

(defun my-flymake-create-temp (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (unless prefix
    (setq prefix "flymake"))
  (let* ((name (concat (file-name-nondirectory (file-name-sans-extension file-name)) "_" prefix))
         (ext (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext)))
    (flymake-log 3 "my-flymake-create-temp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (when help
        (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

(provide 'my-flymake)
