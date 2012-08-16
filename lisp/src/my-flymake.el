;;; my-flymake.el

(require 'flymake)

(setq-default flymake-no-changes-timeout 2.0
              flymake-start-syntax-check-on-newline nil
              flymake-start-syntax-check-on-find-file nil)

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

(defun my-flymake-goto-next-error ()
  "Go to next flymake error and show the error in the minibuffer."
  (interactive)
  (unless (get-char-property (point) 'flymake-overlay)
    (forward-line -1))
  (flymake-goto-next-error)
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (when help
        (message "%s" help)))))

(defun my-flymake-goto-prev-error ()
  "Go to prev flymake error and show the error in the minibuffer."
  (interactive)
  (flymake-goto-prev-error)
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (when help
        (message "%s" help)))))

(defadvice flymake-start-syntax-check-process (around my-flymake-start-syntax-check-process-advice activate)
  "Don't query to kill flymake process."
  ad-do-it
  (when (and ad-return-value (processp ad-return-value))
    (set-process-query-on-exit-flag ad-return-value nil)))

(provide 'my-flymake)
