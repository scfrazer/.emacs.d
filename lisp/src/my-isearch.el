;;; my-isearch.el

(defun my-isearch-buffers ()
  "isearch multiple buffers."
  (interactive)
  (multi-isearch-buffers
   (delq nil (mapcar (lambda (buf)
                       (set-buffer buf)
                       (and (not (equal major-mode 'dired-mode))
                            (not (string-match "^\\([ *]\\|TAGS\\)" (buffer-name buf)))
                            buf))
                     (buffer-list)))))

(defun my-isearch-exit-other-end ()
  "Exit isearch at the other end"
  (interactive)
  (when isearch-forward (goto-char isearch-other-end))
  (isearch-exit))

(defun my-isearch-word-at-point ()
  "Search for word at point"
  (interactive)
  (call-interactively 'isearch-forward))

(defun my-isearch-mode-hook ()
  (if (equal this-command 'my-isearch-word-at-point)
      (isearch-yank-string
       (buffer-substring-no-properties (progn (skip-syntax-backward "w_") (point))
                                       (save-excursion (skip-syntax-forward "w_") (point))))
    (when (and transient-mark-mode mark-active)
      (when (= (point) (region-end))
        (exchange-point-and-mark))
      (isearch-yank-string (buffer-substring (point) (region-end))))
    (setq mark-active nil)))

(add-hook 'isearch-mode-hook 'my-isearch-mode-hook)

(define-key isearch-mode-map (kbd "<return>") 'my-isearch-exit-other-end)

(provide 'my-isearch)
