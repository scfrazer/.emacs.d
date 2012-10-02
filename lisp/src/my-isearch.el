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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-isearch-dwim (&optional arg)
  "Do `my-isearch-forward-dwim' or, with prefix arg, `my-isearch-backward-dwim'."
  (interactive "P")
  (call-interactively (if arg 'my-isearch-backward-dwim 'my-isearch-forward-dwim)))

(defun my-isearch-forward-dwim ()
  "Read char, then isearch-foward at word boundaries if alphanumeric,
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

(defun my-isearch-backward-dwim ()
  "Read char, then isearch-backward at word boundaries if alphanumeric,
or jump forward to input char."
  (interactive)
  (message "Backward: ")
  (let ((char (read-char)))
    (if (not (string-match "[a-zA-Z0-9_]" (char-to-string char)))
        (let ((pos (point))
              (case-fold-search nil))
          (unless (search-backward (char-to-string char) nil t)
            (goto-char pos)
            (error "No matching char found.")))
      (push char unread-command-events)
      (push ?\< unread-command-events)
      (push ?\\ unread-command-events)
      (isearch-backward-regexp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-isearch-word-at-point ()
  "Search for word at point"
  (interactive)
  (call-interactively 'isearch-forward))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-isearch-last-regexp nil)

(defadvice isearch-mode (before my-isearch-mode activate)
  "Remember if last isearch was regular or regexp."
  (setq my-isearch-last-regexp isearch-regexp))

(defadvice isearch-repeat (before my-isearch-repeat activate)
  "If last isearch was a regexp, do it this time even if normal isearch was called."
  (setq isearch-regexp (or isearch-regexp
                           (and (eq isearch-forward (eq direction 'forward))
                                (equal isearch-string "")
                                my-isearch-last-regexp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-isearch-yank-word (&optional arg)
  "Pull next sexp or, with C-u, word from buffer into search string."
  (interactive "P")
  (if arg
      (isearch-yank-internal (lambda () (forward-word 1) (point)))
    (isearch-yank-internal (lambda () (forward-sexp 1) (point)))))

(defun my-isearch-word ()
  "Surround current input with word/symbol delimiters and turn on regexp matching if necessary."
  (interactive)
  (unless isearch-regexp
    (isearch-toggle-regexp))
  (setq isearch-string (concat "\\_<" isearch-string "\\_>")
        isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
  (isearch-search-and-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-g") 'isearch-cancel)
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-yank-word)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-w") 'my-isearch-word)
(define-key isearch-mode-map (kbd "RET") 'my-isearch-exit-other-end)

(provide 'my-isearch)
