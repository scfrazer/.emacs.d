;;; my-bookmark.el

(require 'bookmark)

(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)

(defun my-bookmark-reload ()
  "Reload bookmarks"
  (interactive)
  (when (file-exists-p bookmark-default-file)
    (bookmark-load bookmark-default-file t)))

(defun my-bookmark-munge-filenames (coding)
  "Munge bookmark filenames."
  (dolist (var (list "PROJ" "WSPATH"))
    (when-let ((val (getenv var)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat "\\(" val "\\)") nil t)
          (replace-match (concat "$" var) t))))))

(advice-add #'bookmark-insert-file-format-version-stamp :after #'my-bookmark-munge-filenames)

(defun my-bookmark-unmunge-filenames ()
  "Unmunge bookmark filenames."
  (dolist (var (list "PROJ" "WSPATH"))
    (when-let ((val (getenv var)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat "\\([$]" var "\\)") nil t)
          (replace-match val t)))))
  (set-buffer-modified-p nil))

(advice-add #'bookmark-alist-from-buffer :before #'my-bookmark-unmunge-filenames)

(defun my-bookmark-write-shell-bookmarks (file)
  "Convert bookmarks to format zsh and tcsh (yuck!) can use."
  (my-bookmark-write-shell-bookmark "hash -d " "~/.zsh_bmk"))
  ;; (my-bookmark-write-shell-bookmark "set " "~/.cshrc_bmk"))

(defun my-bookmark-write-shell-bookmark (line-prefix bmk-filename)
  "Write a shell bookmark file using line-prefix."
    (with-temp-buffer
      (let (name filename)
        (dolist (bmk bookmark-alist)
          (setq name (bookmark-name-from-full-record bmk)
                filename (bookmark-get-filename bmk))
          (unless (file-directory-p filename)
            (setq filename (file-name-directory filename)))
          (unless (string-match "[^-a-zA-Z0-9_.~/]" name)
            (dolist (var (list "PROJ" "WSPATH"))
              (when-let ((val (getenv var)))
                (setq filename (replace-regexp-in-string val (concat "$" var) filename t))))
            (insert line-prefix name "=" filename)
            (delete-char -1)
            (newline))))
      (write-file bmk-filename)))

(advice-add #'bookmark-write-file :after #'my-bookmark-write-shell-bookmarks)

(provide 'my-bookmark)
