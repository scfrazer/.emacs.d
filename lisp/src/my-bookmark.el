;;; my-bookmark.el

(require 'bookmark)

(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)

(defun my-bookmark-reload ()
  "Reload bookmarks"
  (interactive)
  (bookmark-load bookmark-default-file t))

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

;; Do this in your .zshrc to use bookmarks:
;; bmk_file=~/.zsh_bmk
;; if [[ -f $bmk_file ]]; then
;;     source $bmk_file
;; fi
;; alias bmk_reload='source $bmk_file'
;; alias bmk_list="sort $bmk_file | sed -nre 's/.+ (.+)=(.+)/\1\t\2/p' | expand --tabs=25"
;;
;; Do this in your .cshrc to use bookmarks:
;; set bmk_file=~/.cshrc_bmk
;; if ( -f $bmk_file ) source $bmk_file
;; alias bmk_reload "source $bmk_file"
;; alias bmk_list "sort $bmk_file | awk 'BEGIN { FS = "'"[ =]" }; { printf("%-25s%s\n", $2, $3) }'"'"
;; complete - 'c//v'

(provide 'my-bookmark)
