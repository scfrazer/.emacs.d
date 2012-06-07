;;; my-dired.el

(require 'dired)

(setq dired-boring-extensions '("~" "#" ".o" ".obj" ".d" ".elc" ".pyc" ".lst" ".log" ".orig" ".keep"))
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-al")
(setq dired-isearch-filenames 'dwim)

(setq dired-font-lock-keywords
      '(("^. \\([^\n]+\\)\\(:\\)[\n]" (1 font-lock-function-name-face))
        ("^[^ \n]" (0 font-lock-constant-face) (".+" (dired-move-to-filename) nil (0 dired-marked-face)))
        ("^. [0-9 	]*d[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-keyword-face)))
        ("^. [0-9 	]*l[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-string-face)))
        ("^. [0-9 	]*...\\(x\\|...x\\|......x\\)[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-variable-name-face)))
        (eval let ((extensions (mapcar 'regexp-quote dired-boring-extensions)))
              (list
               (concat "\\(" (mapconcat 'identity extensions "\\|") "\\|#\\)$")
               '(".+" (dired-move-to-filename) nil (0 font-lock-comment-face))))))

;; Find marked files in dired, but don't display all at once

(defun my-dired-do-find-file ()
   "Visit all marked files at once."
   (interactive)
   (let ((file-list (reverse (dired-get-marked-files))))
     (mapcar 'find-file file-list)))

;; Open directory in current buffer, or file in new buffer

(defun my-dired-open ()
  "Open directory in current buffer, or file in new buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (beginning-of-line-text)
      (if (looking-at "d")
          (find-alternate-file (dired-get-filename))
        (dired-find-file)))))

;; Jump to directory

(defvar my-dired-prev-dir "~")

(defun my-dired-jump-to-dir ()
  "Jump to directory in current buffer."
  (interactive)
  (setq my-dired-prev-dir (dired-current-directory))
  (find-alternate-file
   (read-file-name "Dired (directory): " nil default-directory nil)))

(defun my-dired-jump-to-prev-dir ()
  "Quick jump to last directory jumped from."
  (interactive)
  (let ((dir-to-jump-to my-dired-prev-dir))
    (setq my-dired-prev-dir (dired-current-directory))
    (find-alternate-file dir-to-jump-to)))

;; Pop to dired buffer if one exists, otherwise create one

(defun my-dired-pop-to-or-create (&optional arg)
  "Pop to first dired buffer, or create one."
  (interactive "P")
  (if (and (boundp 'dired-buffers) dired-buffers)
      (catch 'done
        (dolist (dbuf dired-buffers)
          (when (buffer-live-p (cdr dbuf))
            (let ((dir default-directory))
              (switch-to-buffer (cdr dbuf))
              (when arg
                (setq my-dired-prev-dir (dired-current-directory))
                (find-alternate-file dir)))
            (throw 'done t)))
        (dired default-directory))
    (dired default-directory)))

;; Modified find-name-dired

(defun my-find-name-dired (&optional arg)
  "Same as `find-name-dired', but uses default dir unless
there is a prefix arg."
  (interactive "P")
  (if arg
      (call-interactively 'find-name-dired)
    (find-name-dired
     default-directory
     (read-from-minibuffer "Find-name (filename wildcard): "))))

;; Sort directories first

(defun my-dired-sort ()
  "Sort directories first."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (dired-insert-set-properties (point-min) (point-max))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'my-dired-sort)

;; Toggle current file mark in dired

(defun my-dired-toggle-mark ()
  "Toggle mark on current file."
  (interactive)
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (if (looking-at (char-to-string dired-marker-char))
          (dired-unmark 1)
        (dired-mark 1)))
    (dired-next-line 1)))

;; Go up a directory and put cursor on dir we came from

(defun my-dired-up-dir ()
  "Go up a directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (find-alternate-file "..")
    (dired-goto-file current-dir)))

;; Replace the current sub-dir with one at point

(defun my-dired-replace-sub-dir ()
  "Replace sub-dir with the one at point."
  (interactive)
  (let ((dir (dired-get-filename)))
    (dired-kill-subdir)
    (dired-maybe-insert-subdir dir)))

;; Dired hook

(defun my-dired-mode-hook ()
  (define-key dired-mode-map " " 'my-dired-toggle-mark)
  (define-key dired-mode-map (kbd "C-o") nil)
  (define-key dired-mode-map (kbd "M-o") nil)
  (define-key dired-mode-map "c" 'dired-do-copy)
  (define-key dired-mode-map "I" 'my-dired-replace-sub-dir)
  (define-key dired-mode-map "j" 'my-dired-jump-to-dir)
  (define-key dired-mode-map "J" 'my-dired-jump-to-prev-dir)
  (define-key dired-mode-map "o" 'my-dired-do-find-file)
  (define-key dired-mode-map "r" 'dired-do-rename)
  (define-key dired-mode-map "u" 'my-dired-up-dir)
  (define-key dired-mode-map "w" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "RET") 'my-dired-open)
  (define-key dired-mode-map (kbd "<return>") 'my-dired-open)
  (toggle-truncate-lines))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(provide 'my-dired)
