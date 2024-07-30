;;; my-dired.el

(require 'dired)
(require 'dired-subtree)
(require 'my-font-lock)
(require 'my-ibuffer)

;; Settings

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-auto-revert-buffer t
              dired-dwim-target t
              dired-isearch-filenames 'dwim
              dired-listing-switches "-alv --group-directories-first --time-style=long-iso"
              dired-recursive-copies 'always
              dired-recursive-deletes 'always
              dired-subtree-line-prefix "")

;; Colorize

(defface my-dired-debug-face
  '((t (:foreground "darkorange2")))
  "Debug file face."
  :group 'dired-faces)

(font-lock-add-keywords
 'dired-mode
 '(("^. [0-9 	]*-..\\(x\\|...x\\|......x\\)[^:]" (".+" (dired-move-to-filename) nil (0 font-lock-variable-name-face)))
   ("[.]log$" (".+" (dired-move-to-filename) nil (0 'my-dired-debug-face)))
   ("~\\|#\\|\\([.]\\(d\\|orig\\|keep\\(\.[0-9]+\\)?\\|contrib\\(\.[0-9]+\\)?\\)$\\)" (".+" (dired-move-to-filename) nil (0 font-lock-comment-face))))
 'append)

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
  (if (> (dired-subtree--get-depth (dired-subtree--get-ov)) 0)
      (dired-subtree-up)
    (let ((current-dir (dired-current-directory)))
      (find-alternate-file "..")
      (dired-goto-file current-dir))))

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

;; Subtree

(defun my-dired-subtree-after-insert ()
  "Adjust inserted names to be tree-like."
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^..[-?dl]" nil t)
      (dired-move-to-filename)
      (let ((parent-col (current-column)) depth col)
        (while (not (eobp))
          (forward-line)
          (dired-move-to-filename)
          (setq depth (dired-subtree--get-depth (dired-subtree--get-ov)))
          (when (> depth 0)
            (let ((goal (+ parent-col (* 2 depth)))
                  (col (current-column)))
              (when (< col goal)
                (insert-char ?  (- goal col)))))))))
  (setq buffer-read-only t))

(add-hook 'dired-subtree-after-insert-hook 'my-dired-subtree-after-insert)

;; Keymap

(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "J") 'my-dired-jump-to-prev-dir)
(define-key dired-mode-map (kbd "M-<") 'my-dired-beginning-of-buffer)
(define-key dired-mode-map (kbd "M->") 'my-dired-end-of-buffer)
(define-key dired-mode-map (kbd "N") 'dired-subtree-next-sibling)
(define-key dired-mode-map (kbd "P") 'dired-subtree-previous-sibling)
(define-key dired-mode-map (kbd "RET") 'my-dired-open)
(define-key dired-mode-map (kbd "SPC") 'my-dired-toggle-mark)
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "b") 'my-dired-toggle-path)
(define-key dired-mode-map (kbd "j") 'my-dired-jump-to-dir)
(define-key dired-mode-map (kbd "n") 'my-dired-next-line)
(define-key dired-mode-map (kbd "o") 'my-dired-do-find-file)
(define-key dired-mode-map (kbd "p") 'my-dired-previous-line)
(define-key dired-mode-map (kbd "u") 'my-dired-up-dir)

(provide 'my-dired)
