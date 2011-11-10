;;; my-query-replace.el

(defun my-query-replace (&optional arg)
  "query-replace ... take from-string from region if it is active, or guess
based on where point is.  With prefix arg, call the standard query-replace
\(good for repeating previous replacement)."
  (interactive "*P")
  (if arg
      (let ((current-prefix-arg nil))
        (call-interactively 'query-replace))
    (let (from to)
      (if (region-active-p)
          (progn (setq from (buffer-substring (region-beginning) (region-end))
                       to (read-from-minibuffer
                           (format "Query replace %s with: " from) nil nil nil
                           'query-replace-history))
                 (goto-char (region-beginning))
                 (setq mark-active nil))
        (let* ((default (buffer-substring-no-properties
                         (point)
                         (save-excursion (skip-syntax-forward "w_") (point))))
               (from-str (read-from-minibuffer
                          (format "Query replace (default %s): " default) nil nil nil
                          'query-replace-history default)))
          (setq from (if (string= from-str "") default from-str)
                to (read-from-minibuffer
                    (format "Query replace %s with: " from) nil nil nil
                    'query-replace-history))))
      (query-replace from to)
      (setq query-replace-defaults (cons from to)))))

(provide 'my-query-replace)
