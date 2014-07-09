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
  '((t (:background "#D7D7AF")))
  "Face for occur line numbers."
  :group 'faces)

(defface my-occur-buffer-name-face
  '((t (:underline t :background "#D7D7AF")))
  "Face for occur buffer names."
  :group 'faces)

;; TODO Remove this when new Emacs version with patches available
(load-library "replace")
(defadvice face-differs-from-default-p (around my-occur-face-differs-from-default-p activate)
  (if (equal (ad-get-arg 0) 'my-occur-prefix-face)
      (setq ad-return-value t)
    ad-do-it))
;; TODO

(defun my-occur-mode-hook ()
  (define-key occur-mode-map "q" (lambda ()
                                   (interactive)
                                   (kill-buffer nil)
                                   (when (> (count-windows) 1)
                                     (delete-window))))
  (define-key occur-mode-map "n" 'next-line)
  (define-key occur-mode-map "p" 'previous-line)
  (setq list-matching-lines-prefix-face 'my-occur-prefix-face)
  (setq list-matching-lines-buffer-name-face 'my-occur-buffer-name-face)
  (setq truncate-lines t)
  (occur-rename-buffer t))

(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(provide 'my-occur)
