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

(defun my-isearch-forward-dwim ()
  "Read char, then isearch-foward-word if alphanumeric,
or jump forward to input char."
  (interactive)
  (message "Forward: ")
  (let ((char (read-char)))
    (if (not (string-match "[a-zA-Z0-9_]" (char-to-string char)))
        (let ((pos (point))
              (case-fold-search nil))
          (forward-char)
          (if (search-forward (char-to-string char) nil t)
              (backward-char)
            (goto-char pos)
            (error "No matching char found.")))
      (push char unread-command-events)
      (push ?\< unread-command-events)
      (push ?\\ unread-command-events)
      (isearch-forward-regexp))))

(defun my-isearch-mode-hook ()
  "Special setup for isearch."
  (cond ((equal this-command 'my-isearch-word-at-point)
         (isearch-yank-string
          (buffer-substring-no-properties (progn (skip-syntax-backward "w_") (point))
                                          (save-excursion (skip-syntax-forward "w_") (point)))))
        (t
         (when (and transient-mark-mode mark-active (/= (region-beginning) (region-end)))
           (when (= (point) (region-end))
             (exchange-point-and-mark))
           (isearch-yank-string (buffer-substring (point) (region-end)))
           (setq mark-active nil)))))

(add-hook 'isearch-mode-hook 'my-isearch-mode-hook)

(defun my-isearch-yank-sexp ()
  "Pull next sexp from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))

(define-key isearch-mode-map (kbd "<return>") 'my-isearch-exit-other-end)
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-b") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-.") (lambda ()
                                           (interactive)
                                           (push (read-char) unread-command-events)
                                           (push ?\* unread-command-events)
                                           (push ?\. unread-command-events)))

(provide 'my-isearch)
