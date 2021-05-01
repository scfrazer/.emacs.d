;;; my-tags-search.el

(require 'etags)
(require 'grep)

(defun my-tags-search (regexp)
  "Search through all files listed in tags table for match for REGEXP.
Show all matches at once."
  (interactive "sTags search (regexp): ")
  ;; Get all unique filenames in TAGS files.
  (let ((keep-going t) files)
    (when (visit-tags-table-buffer)
      (while keep-going
        (dolist (filename (tags-table-files))
          (add-to-list 'files (file-truename filename)))
        (setq keep-going (visit-tags-table-buffer t))))
    ;; grep through every file for regexp
    (when files
      (grep-compute-defaults)
      (let ((outbuf (get-buffer-create "*tags-search-sel*")))
        (with-current-buffer outbuf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "Searching for '" regexp "' in tags files ...\n\n")
          (dolist (file files)
            (call-process-shell-command (concat grep-command regexp " " file) nil t))
          (grep-mode)
          (setq overlay-arrow-position nil)
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (pop-to-buffer outbuf)))))

(provide 'my-tags-search)
