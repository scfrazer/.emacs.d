;;; my-ffap.el

(require 'ffap)

(setq-default ffap-url-regexp nil)
(setcdr (assq 'file ffap-string-at-point-mode-alist) (list "-a-zA-Z0-9_.~=@/$(){}:" "" ""))

(defun my-ffap (&optional arg)
  "ffap, or ffap-other-window when preceded with C-u."
  (interactive "P")
  (call-interactively (if arg 'ffap-other-window 'ffap)))

(defvar my-ffap-line-number nil)

(defadvice ffap-string-at-point (around my-ffap-string-at-point activate)
  "Capture and expand $(FOO) or ${FOO} or $FOO or $ENV{FOO} from env vars."
  ad-do-it
  (when ad-return-value
    (with-temp-buffer
      (insert ad-return-value)
      ;; Expand env vars
      (goto-char (point-min))
      (while (re-search-forward "[$]" nil t)
        (let (env-var)
          (delete-char -1)
          (when (looking-at "ENV{")
            (delete-char 3))
          (when (looking-at "[({]")
            (delete-char 1))
          (when (looking-at "[a-zA-Z0-9_]+")
            (setq env-var (match-string-no-properties 0))
            (replace-match ""))
          (when (looking-at "[})]")
            (delete-char 1))
          (when (and (stringp env-var) (getenv env-var))
            (insert (getenv env-var)))))
      (goto-char (point-min))
      (while (re-search-forward "[$(){}]" nil t)
        (replace-match ""))
      ;; Map DOS drives into WSL paths
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z]\\):" nil t)
        (replace-match (concat "/mnt/" (downcase (match-string-no-properties 1))) t))
      ;; Strip off line-number
      (goto-char (point-min))
      (when (re-search-forward "[@:][0-9]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (let ((end (cadr ffap-string-at-point-region)))
          (setcar (cdr ffap-string-at-point-region) (- end (- (match-end 0) (match-beginning 0))))))
      (setq ad-return-value (buffer-substring (point-min) (point-max)))
      (setq ffap-string-at-point ad-return-value))
    ;; Try to find a line number
    (save-excursion
      (goto-char (cadr ffap-string-at-point-region))
      (when (looking-at "[@:,]\\s-*\\([Ll]ine\\s-*\\)?\\([0-9]+\\)")
        (setq my-ffap-line-number (string-to-number (match-string 2)))
        (setcar (cdr ffap-string-at-point-region) (match-end 0))))))

(defadvice find-file-at-point (after my-ffap-find-file-at-point activate)
  "Go to `my-ffap-line-number' if non-nil."
  (when my-ffap-line-number
    (goto-char (point-min))
    (forward-line (1- my-ffap-line-number))
    (recenter)
    (setq my-ffap-line-number nil)))

(provide 'my-ffap)
