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
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))

(defun my-isearch-yank-region ()
  "Yank region."
  (interactive)
  (let ((m (mark t))
        (p (point)))
    (when (and isearch-forward (> p m))
      (setq isearch-other-end m)))
  (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))

(defun my-isearch-word ()
  "Surround current input with word/symbol delimiters and turn on regexp matching if necessary."
  (interactive)
  (unless isearch-regexp
    (isearch-toggle-regexp))
  (setq isearch-string (concat "\\_<" isearch-string)
        isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
  (isearch-search-and-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-g") 'isearch-cancel)
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-i") 'my-isearch-yank-region)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-w") 'my-isearch-word)
(define-key isearch-mode-map (kbd "RET") 'my-isearch-exit-other-end)

(provide 'my-isearch)
