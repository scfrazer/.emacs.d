;;; my-tags.el  -*- lexical-binding: t; -*-

(defun my-tags-complete ()
  "Completing read for a tag."
  (interactive)
  (let ((start (save-excursion (skip-syntax-backward "w_") (point)))
        (end (point)))
    (or tags-table-list
        tags-file-name
        (visit-tags-table-buffer))
    (when-let (result
               (completing-read "Tag: "
                                (my-tags-completion-table (buffer-substring-no-properties start end))
                                nil t))
      (delete-region start end)
      (insert result))))

(defun my-tags-completion-table (input)
  "Create a tags completion table starting with INPUT."
  (with-current-buffer (get-file-buffer tags-file-name)
    (let (table
          (progress-reporter
           (make-progress-reporter
            (format "Making tags completion table for %s..." buffer-file-name)
            (point-min) (point-max)))
          (case-fold-search nil)
          (re (concat "\177\\(" (regexp-quote input) "[^\001]+\\)\001")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (progress-reporter-update progress-reporter (point))
          (push (buffer-substring (match-beginning 1) (match-end 1)) table)))
      table)))

(provide 'my-tags)
