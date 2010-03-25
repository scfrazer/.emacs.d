;;; my-confluence.el

(require 'htmlize)

(setq htmlize-output-type 'inline-css)

(defun my-confluence-html (start end)
  (interactive "r")
  (let ((buf (htmlize-region start end)))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (re-search-forward "<body[^>]*>")
    (delete-region (point-min) (1+ (point)))
    (insert "{panel}\n{html}\n")
    (re-search-forward "</body>")
    (backward-char 7)
    (delete-region (point) (point-max))
    (insert "\n{html}\n{panel}\n")
    (goto-char (point-min))
    (while (re-search-forward "<[/a-zA-Z0-9]+" nil t)
      (replace-match (upcase (match-string 0))))
    (goto-char (point-min))))

(defun my-confluence-highlight (start end)
  (interactive "r")
  (goto-char end)
  (insert "{highlight}")
  (save-excursion
    (goto-char start)
    (insert "{highlight:#f0f0f0}")))

(provide 'my-confluence)
