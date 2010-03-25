;;; my-tmp-bookmark.el

(defvar my-tmp-bookmark-stack nil
  "Temporary bookmark stack.")

(defun my-tmp-bookmark-set ()
  "Set temporary bookmark"
  (interactive)
  (push (list (point-marker) (buffer-file-name) (point)) my-tmp-bookmark-stack)
  (message "Temporary bookmark set"))

(defun my-tmp-bookmark-go ()
  "Go to temporary bookmark"
  (interactive)
  (if my-tmp-bookmark-stack
      (let* ((item (car my-tmp-bookmark-stack))
             (marker (nth 0 item))
             (file (nth 1 item))
             (pos (nth 2 item)))
        (if (marker-buffer marker)
            (progn
              (switch-to-buffer (marker-buffer marker))
              (goto-char (marker-position marker)))
          (find-file file)
          (goto-char pos))
        (setq my-tmp-bookmark-stack (cdr my-tmp-bookmark-stack)))
    (message "No more temporary bookmarks")))

(provide 'my-tmp-bookmark)
