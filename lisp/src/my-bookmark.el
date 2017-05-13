;;; my-bookmark.el

(require 'bookmark)

(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)

(defun my-bookmark-reload ()
  "Reload bookmarks"
  (interactive)
  (bookmark-load bookmark-default-file t))

(defun my-bookmark-reseat ()
  "Reseat bookmarks in Perforce."
  (let ((proj (getenv "PROJ")))
    (when proj
      (let ((p4-ws (getenv "P4WS")) regexp)
        (unless p4-ws
          (let* ((host (getenv "HOST"))
                 (location (progn (string-match "asic-vm-\\([a-z]+\\)[0-9]+" host) (match-string 1 host))))
            (setq p4-ws (concat "/ws/" (getenv "USER") "-" location))))
        (setq regexp (concat "\\(/vob/sse\\|" p4-ws "/[^/]+\\)"))
        (dolist (bmk bookmark-alist)
          (let ((filename (bookmark-get-filename bmk)))
            (when (string-match regexp filename)
              (bookmark-set-filename bmk (replace-regexp-in-string regexp proj filename)))))))))

(defadvice bookmark-load (after my-bookmark-reseat activate)
  (my-bookmark-reseat))

(defadvice bookmark-write-file (after my-bookmark-to-shell activate)
  "Convert bookmarks to format zsh and tcsh (yuck!) can use."
  (let (name filename)
    (with-temp-buffer
      (dolist (bmk bookmark-alist)
        (setq name (bookmark-name-from-full-record bmk)
              filename (bookmark-get-filename bmk))
        (unless (file-directory-p filename)
          (setq filename (file-name-directory filename)))
        (unless (string-match "[^-a-zA-Z0-9_.~/]" name)
          (insert "hash -d " name "=" filename)
          (delete-char -1)
          (newline)))
      (write-file "~/.zsh_bmk")
      (erase-buffer)
      (dolist (bmk bookmark-alist)
        (setq name (bookmark-name-from-full-record bmk)
              filename (bookmark-get-filename bmk))
        (unless (file-directory-p filename)
          (setq filename (file-name-directory filename)))
        (unless (string-match "[^a-zA-Z0-9_.~/]" name)
          (insert "set " name "=" filename)
          (delete-char -1)
          (newline)))
      (write-file "~/.cshrc_bmk"))))

(my-bookmark-reseat)

(provide 'my-bookmark)
