;; my-query-replace

;; query-replace-read-args

(defun my-query-replace-read-args (noerror)
  "Take from-string from region if it is active."
  (if (and transient-mark-mode mark-active)
    (let* ((from (buffer-substring (region-beginning) (region-end)))
           (to (read-from-minibuffer (format "Query replace %s with: " from) nil nil nil 'query-replace-history)))
      (goto-char (region-beginning))
      (setq mark-active nil)
      (list from to))
    (query-replace-read-args "Query replace" nil noerror)))

;; query-replace ... take from-string from region if it is active

(defun my-query-replace ()
  "query-replace ... take from-string from region if it is active"
  (interactive "*")
  (let ((from-to (my-query-replace-read-args nil)))
    (query-replace (car from-to) (cadr from-to))))

;; tags-query-replace

(defun my-tags-query-replace ()
  "tags-query-replace ... take from-string from region if it is active"
  (interactive "*")
  (let ((from-to (my-query-replace-read-args t)))
    (tags-query-replace (car from-to) (cadr from-to))))

(provide 'my-query-replace)
