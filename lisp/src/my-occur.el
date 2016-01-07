;;; my-occur.el

(require 'my-buf)

(defun my-occur (&optional arg)
  "Like `occur', but with prefix arg take the string from the region."
  (interactive "P")
  (if arg
      (occur (read-from-minibuffer "List lines matching regexp: "
                                   (regexp-quote (buffer-substring (region-beginning) (region-end)))
                                   nil nil 'regexp-history))
    (let* ((search-spaces-regexp search-whitespace-regexp)
           (default (buffer-substring-no-properties
                     (point)
                     (save-excursion (skip-syntax-forward "w_") (point))))
           (regexp (read-from-minibuffer
                    (format "List lines matching regexp (default %s): " default) nil nil nil
                    'regexp-history default)))
      (setq regexp (if (string= regexp "") default regexp))
      (occur regexp))))

(defun my-multi-occur (&optional arg)
  "Like `multi-occur-in-matching-buffers', but with prefix arg take the string from the region."
  (interactive "P")
  (let ((search-spaces-regexp search-whitespace-regexp) regexp bufs)
    (if arg
        (setq regexp (read-from-minibuffer
                      "List lines matching regexp: "
                      (regexp-quote (buffer-substring (region-beginning) (region-end))) nil nil
                      'regexp-history))
      (let ((default (buffer-substring-no-properties
                      (point)
                      (save-excursion (skip-syntax-forward "w_") (point)))))
        (setq regexp (read-from-minibuffer
                      (format "List lines matching regexp (default %s): " default) nil nil nil
                      'regexp-history default))
        (when (string= regexp "")
          (setq regexp default))))
    (dolist (buf (buffer-list))
      (unless (my-buf-ignore-buffer (buffer-name buf))
        (push buf bufs)))
    (when bufs
      (multi-occur bufs regexp))))

(defun my-occur-mode-hook ()
  (define-key occur-mode-map "q" (lambda ()
                                   (interactive)
                                   (kill-buffer nil)
                                   (when (> (count-windows) 1)
                                     (delete-window))))
  (define-key occur-mode-map "n" 'next-line)
  (define-key occur-mode-map "p" 'previous-line)
;;   (setq list-matching-lines-prefix-face 'my-occur-prefix-face)
;;   (setq list-matching-lines-buffer-name-face 'my-occur-buffer-name-face)
  (setq truncate-lines t)
  (when (eq major-mode 'occur-mode)
    (occur-rename-buffer t)))

(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(provide 'my-occur)
