;;; my-colors.el  -*- lexical-binding: t; -*-

(defun my-colors ()
  "Display editable-colors"
  (interactive)
  (let ((list (list-colors-duplicates (defined-colors)))
        (buf (get-buffer-create "*Colors*")))
    (when list-colors-sort
      ;; Schwartzian transform with `(color key1 key2 key3 ...)'.
      (setq list (mapcar
                  'car
                  (sort (delq nil (mapcar
                                   (lambda (c)
                                     (let ((key (list-colors-sort-key
                                                 (car c))))
                                       (when key
                                         (cons c (if (consp key) key
                                                   (list key))))))
                                   list))
                        (lambda (a b)
                          (let* ((a-keys (cdr a))
                                 (b-keys (cdr b))
                                 (a-key (car a-keys))
                                 (b-key (car b-keys)))
                            ;; Skip common keys at the beginning of key lists.
                            (while (and a-key b-key (equal a-key b-key))
                              (setq a-keys (cdr a-keys) a-key (car a-keys)
                                    b-keys (cdr b-keys) b-key (car b-keys)))
                            (cond
                             ((and (numberp a-key) (numberp b-key))
                              (< a-key b-key))
                             ((and (stringp a-key) (stringp b-key))
                              (string< a-key b-key)))))))))
    (when (memq (display-visual-class) '(gray-scale pseudo-color direct-color))
      ;; Don't show more than what the display can handle.
      (let ((lc (nthcdr (1- (display-color-cells)) list)))
        (if lc
            (setcdr lc nil))))
    (with-current-buffer buf
      (erase-buffer)
      (list-colors-print list)
      (set-buffer-modified-p nil)
      (setq truncate-lines t))
    (pop-to-buffer buf)))

(provide 'my-colors)
