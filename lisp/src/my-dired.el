;;; my-dired.el

(require 'dired)
(require 'my-font-lock)
(require 'my-ibuffer)

;; Settings

(put 'dired-find-alternate-file 'disabled nil)

(setq dired-auto-revert-buffer t
      dired-isearch-filenames 'dwim
      dired-listing-switches "-alv"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(defface my-dired-debug-face
  '((t (:foreground "orange2")))
  "Debug file face."
  :group 'dired-faces)

(setq dired-font-lock-keywords
      '(("^. \\([^\n]+\\)\\(:\\)[\n]" (1 font-lock-function-name-face))
        ("^[^ \n]" (0 font-lock-constant-face) (".+" (dired-move-to-filename) nil (0 dired-marked-face)))
        ("^. [0-9 	]*d[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-keyword-face)))
        ("^. [0-9 	]*l[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-string-face)))
        ("^. [0-9 	]*...\\(x\\|...x\\|......x\\)[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-variable-name-face)))
        ("^. [0-9 	]*l[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-string-face)))
        ("[.]log$" (".+" (dired-move-to-filename) nil (0 'my-dired-debug-face)))
        ("~\\|#\\|\\([.]\\(o\\|obj\\|d\\|elc\\|pyc\\|orig\\|keep\\(\.[0-9]+\\)?\\|contrib\\(\.[0-9]+\\)?\\)$\\)" (".+" (dired-move-to-filename) nil (0 font-lock-comment-face)))))

;; (when (featurep 'my-clearcase)
;;   (setq dired-font-lock-keywords
;;         (append dired-font-lock-keywords
;;                 '(("^.+ \\(CHECKOUT-[RU]\\|HIJACK\\)\\s-+[0-9]" 1 'clearcase-dired-checkedout-face)
;;                   ("^.+ \\(cc-element\\)\\s-+[0-9]" 1 'clearcase-dired-element-face)
;;                   ("^  \\[ClearCase View: \\(.*\\)\\]" 1 font-lock-builtin-face)))))

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
  (let (dir-p)
    (save-match-data
      (save-excursion
        (beginning-of-line-text)
        (setq dir-p (looking-at "d"))))
    (if dir-p
        (find-alternate-file (dired-get-filename))
      (dired-find-file))))

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
  (let ((curr-filename (buffer-file-name))
        live-buf)
    (if (and (boundp 'dired-buffers) dired-buffers)
        (progn
          (setq live-buf
                (catch 'done
                  (dolist (dbuf dired-buffers)
                    (when (buffer-live-p (cdr dbuf))
                      (let ((dir default-directory))
                        (switch-to-buffer (cdr dbuf))
                        (when arg
                          (setq my-dired-prev-dir (dired-current-directory))
                          (find-alternate-file dir)))
                      (throw 'done t)))))
          (dired default-directory)
          (when (and (or arg (not live-buf)) curr-filename)
            (dired-goto-file curr-filename)))
      (dired default-directory)
      (when curr-filename
        (dired-goto-file curr-filename)))))

;; Modified find-name-dired

(defun my-dired-find-name-dired (&optional arg)
  "Same as `find-name-dired', but uses default dir unless there is a prefix arg."
  (interactive "P")
  (if arg
      (call-interactively 'find-name-dired)
    (find-name-dired
     default-directory
     (read-from-minibuffer "Find-name (filename wildcard): "))))

;; Update display

(defvar my-dired-path-uses-bookmarks nil)

(defun my-dired-toggle-path ()
  "Toggle using bookmarks in path."
  (interactive)
  (setq my-dired-path-uses-bookmarks (not my-dired-path-uses-bookmarks))
  (my-dired-update-path))

(defun my-dired-update-path ()
  "Update the path shown at the top."
  (save-excursion
    (let (buffer-read-only
          (path (expand-file-name dired-directory)))
      (goto-char (point-min))
      (delete-region (point-min) (point-at-eol))
      (insert "  "
              (if (not my-dired-path-uses-bookmarks)
                  (propertize path 'font-lock-face 'font-lock-function-name-face)
                (catch 'done
                  (dolist (sub my-ibuffer-bookmark-subs)
                    (when (string-match (car sub) path)
                      (throw 'done (concat (propertize (cdr sub) 'font-lock-face 'font-lock-variable-name-face)
                                           (propertize (concat "/" (match-string 1 path)) 'font-lock-face 'font-lock-function-name-face)))))
                  path))
              ":")))
  (set-buffer-modified-p nil))

(defun my-dired-after-readin-hook ()
  (interactive)
  (my-dired-update-path)
  ;; Sort directories first
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (dired-insert-set-properties (point-min) (point-max))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'my-dired-after-readin-hook)

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

;; Smarter movement

(defun my-dired-next-line ()
  "Smarter `dired-next-line'."
  (interactive)
  (forward-line)
  (unless (looking-at "..[-?dl]")
    (forward-line -1))
  (dired-move-to-filename))

(defun my-dired-previous-line ()
  "Smarter `dired-previous-line'."
  (interactive)
  (forward-line -1)
  (unless (looking-at "..[-?dl]")
    (forward-line 1))
  (dired-move-to-filename))

(defun my-dired-beginning-of-buffer ()
  "Smarter `beginning-of-buffer'."
  (interactive)
  (call-interactively 'beginning-of-buffer)
  (when (re-search-forward "^..[-?dl]" nil t)
    (dired-move-to-filename)))

(defun my-dired-end-of-buffer ()
  "Smarter `end-of-buffer'."
  (interactive)
  (call-interactively 'end-of-buffer)
  (when (re-search-backward "^..[-?dl]" nil t)
    (dired-move-to-filename)))

;; Dired hook

(defun my-dired-mode-hook ()
  (whitespace-mode -1)
  (toggle-truncate-lines))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(provide 'my-dired)
