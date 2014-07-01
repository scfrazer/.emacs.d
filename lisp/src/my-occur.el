;;; my-occur.el

(defun my-occur (&optional arg)
  "Like `occur', but with prefix arg take the string from the region."
  (interactive "P")
  (if arg
      (occur (read-from-minibuffer "List lines matching regexp: "
                                   (regexp-quote (buffer-substring (region-beginning) (region-end)))
                                   nil nil 'regexp-history))
    (let* ((default (buffer-substring-no-properties
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
  (if arg
      (multi-occur-in-matching-buffers ".+"
                                       (read-from-minibuffer "List lines matching regexp: "
                                                             (regexp-quote (buffer-substring (region-beginning) (region-end)))
                                                             nil nil 'regexp-history))
    (let* ((default (buffer-substring-no-properties
                     (point)
                     (save-excursion (skip-syntax-forward "w_") (point))))
           (regexp (read-from-minibuffer
                    (format "List lines matching regexp (default %s): " default) nil nil nil
                    'regexp-history default)))
      (setq regexp (if (string= regexp "") default regexp))
      (multi-occur-in-matching-buffers ".+" regexp))))

(defface my-occur-prefix-face
  '((t (:foreground "#AFAFD7")))
  "Face for occur line numbers."
  :group 'faces)
(setq-default list-matching-lines-prefix-face 'my-occur-prefix-face)

(defface my-occur-buffer-name-face
  '((t (:underline t :background "#AFAFD7")))
  "Face for occur buffer names."
  :group 'faces)
(setq-default list-matching-lines-buffer-name-face 'my-occur-buffer-name-face)

(defun my-occur-mode-hook ()
  (define-key occur-mode-map "q" 'bury-buffer)
  (define-key occur-mode-map "n" 'next-line)
  (define-key occur-mode-map "p" 'previous-line))

(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(provide 'my-occur)
