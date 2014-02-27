;;; my-ibuffer.el

(require 'ibuffer)

(setq-default ibuffer-default-shrink-to-minimum-size t
              ibuffer-default-sorting-mode 'filename-and-dired
              ibuffer-display-summary nil
              ibuffer-use-other-window t)

(define-ibuffer-sorter filename-and-dired
  "Sort the buffers by their pathname."
  (:description "Filenames plus dired")
  (string-lessp
   (with-current-buffer (car a)
     (or buffer-file-name
         (if (eq major-mode 'dired-mode)
             (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))
   (with-current-buffer (car b)
     (or buffer-file-name
         (if (eq major-mode 'dired-mode)
             (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))))

(defun my-ibuffer ()
  "Open ibuffer with point on last buffer name."
  (interactive)
  (let ((buf (buffer-name)))
    (ibuffer)
    (ibuffer-jump-to-buffer buf)))

(defun my-ibuffer-mode-hook ()
  (define-key ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename-and-dired)
  (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-1-window)
  (hl-line-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

(provide 'my-ibuffer)
