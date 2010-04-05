;;; my-occur.el

(defun my-occur (&optional nlines)
  "Take the string from the region if it is active."
  (interactive "P")
  (if (region-active-p)
      (progn
        (occur (buffer-substring (region-beginning) (region-end)) nlines)
        (deactivate-mark))
    (call-interactively 'occur))
  (when (buffer-live-p (get-buffer "*Occur*"))
    (pop-to-buffer "*Occur*")
    (setq truncate-lines 'one-line-each)
    (fit-window-to-buffer nil (/ (frame-height) 2))))

(defun my-occur-mode-hook ()
  (define-key occur-mode-map "q" (lambda ()
                                   (interactive)
                                   (kill-buffer nil)
                                   (when (> (count-windows) 1)
                                     (delete-window))))
  (define-key occur-mode-map "n" 'next-line)
  (define-key occur-mode-map "p" 'previous-line))

(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(provide 'my-occur)
