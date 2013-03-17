;;; my-register-list.el

(require 'register-list)

(defun my-register-list-quit ()
  (interactive)
  (kill-buffer nil))

(defun my-register-list-insert ()
  (interactive)
  (beginning-of-line)
  (when (re-search-forward "^.\\s-+\\([a-z]\\)" (point-at-eol) t)
    (let ((char (string-to-char (match-string-no-properties 1))))
      (my-register-list-quit)
      (insert-register char t))))

(define-key register-list-mode-map "q" 'my-register-list-quit)
(define-key register-list-mode-map "i" 'my-register-list-insert)

(provide 'my-register-list)
