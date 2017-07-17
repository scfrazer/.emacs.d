;;; my-isearch.el

(defvar my-isearch-region-str nil)

(defun my-isearch-forward (&optional arg)
  "Same as `isearch-forward', but C-u means take from-string from region,"
  (interactive "P")
  (when (and arg (mark t))
    (let ((start (region-beginning)))
      (setq my-isearch-region-str (buffer-substring-no-properties start (region-end)))
      (goto-char start)))
  (isearch-mode t))

(defun my-isearch-backward (&optional arg)
  "Same as `isearch-backward', but C-u means take from-string from region,"
  (interactive "P")
  (when (and arg (mark t))
    (let ((start (region-beginning)))
      (setq my-isearch-region-str (buffer-substring-no-properties start (region-end)))
      (goto-char start)))
  (isearch-mode nil))

(defun my-isearch-mode-hook ()
  (when my-isearch-region-str
    (isearch-yank-string my-isearch-region-str)
    (setq my-isearch-region-str nil)))

(add-hook 'isearch-mode-hook 'my-isearch-mode-hook)

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

;; Avoid byte-compiler warnings
(defvar mc--this-command)

(defun my-isearch-search-forward-line (str)
  "`search-forward' only to end-of-line, and exit at other end."
  (interactive "sForward on line to: ")
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (setq mc--this-command `(lambda () (interactive) (my-isearch-search-forward-line-1 ',str))))
  (my-isearch-search-forward-line-1 str))

(defun my-isearch-search-forward-line-1 (str)
  "Real work for my-isearch-search-forward-line."
  (when (search-forward str (point-at-eol) t)
    (backward-char (length str))))

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

(defun my-isearch-yank-sexp ()
  "Yank next sexp."
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (let ((forward-sexp-function nil))
       (forward-sexp 1) (point)))))

(defun my-isearch-yank-region ()
  "Yank region."
  (interactive)
  (let ((m (mark t))
        (p (point)))
    (when (and isearch-forward (> p m))
      (setq isearch-other-end m)))
  (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-g") 'isearch-cancel)
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "DEL") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "M-w") 'my-isearch-yank-region)
(define-key isearch-mode-map (kbd "RET") 'my-isearch-exit-other-end)

(provide 'my-isearch)
