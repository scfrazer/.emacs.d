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
  (let ((p4-client (getenv "P4CLIENT")))
    (when p4-client
      (let* ((p4-ws (or (getenv "P4WS") "/ws"))
             (user (getenv "USER"))
             (host (getenv "HOST"))
             (location (progn (string-match "asic-vm-\\([a-z]+\\)[0-9]+" host) (match-string 1 host)))
             (client-base (concat p4-ws "/" user "-" location))
             (client-root (replace-regexp-in-string "[ \t\n]" "" (shell-command-to-string "p4 -F %clientRoot% -ztag info")))
             (regexp (concat "\\(/vob/sse\\|" client-base "\\)")))
        (dolist (bmk bookmark-alist)
          (let ((filename (bookmark-get-filename bmk)))
            (when (string-match regexp filename)
              (bookmark-set-filename bmk (replace-regexp-in-string regexp client-root filename)))))))))

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
