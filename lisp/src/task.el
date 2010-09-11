;;; task.el --- Work with named tasks, i.e. open files and visual bookmarks

;; Copyright (C) 2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Feb 2010
;; Version: 1.0
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:
;;
;; 

;; TODO
;; Do this: (require 'task)
;;          (task-add-to-mode-line t)
;;          (global-set-key (kbd "C-x t") 'task-map)
;; auto-save, task-win, bookmark-win, unloaded-file bmks
;; local vs. global bmks
;; notes file
;; show in modeline
;;
;; (define-prefix-command 'task-map)
;; (define-key task-map (kbd "?") 'task-show-current-name)
;; (define-key task-map (kbd "n") 'task-new)
;; (define-key task-map (kbd "s") 'task-save)
;; (define-key task-map (kbd "a") 'task-save-as)
;; (define-key task-map (kbd "l") 'task-load)
;; (define-key task-map (kbd "r") 'task-reload)
;; (define-key task-map (kbd "q") 'task-quit)
;; (define-key task-map (kbd "Q") 'task-quit-no-save)
;; (define-key task-map (kbd "o") 'task-notes)
;; (define-key task-map (kbd "RET") 'task-list-show)
;; (define-key task-map (kbd "b") 'task-bmk-show-all)
;; (define-key task-map (kbd "B") 'task-bmk-show-buf)
;;
;; (if (not task-list-mode-map)
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "<down>") 'task-list-mode-next)
;;       (define-key map (kbd "<up>") 'task-list-mode-prev)
;;       (define-key map (kbd "C-n") 'task-list-mode-next)
;;       (define-key map (kbd "C-p") 'task-list-mode-prev)
;;       (define-key map "n" 'task-list-mode-next)
;;       (define-key map "p" 'task-list-mode-prev)
;;       (define-key map "d" 'task-list-mode-mark-current)
;;       (define-key map "u" 'task-list-mode-unmark-current)
;;       (define-key map "U" 'task-list-mode-unmark-all)
;;       (define-key map "x" 'task-list-mode-execute)
;;       (define-key map "r" 'task-list-mode-rename)
;;       (define-key map (kbd "RET") 'task-list-mode-load)
;;       (define-key map "q" 'task-list-mode-quit)
;;       (setq task-list-mode-map map)))
;;
;; (if (not task-bmk-mode-map)
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "<down>") 'task-bmk-mode-next)
;;       (define-key map (kbd "<up>") 'task-bmk-mode-prev)
;;       (define-key map (kbd "C-n") 'task-bmk-mode-next)
;;       (define-key map (kbd "C-p") 'task-bmk-mode-prev)
;;       (define-key map "n" 'task-bmk-mode-next)
;;       (define-key map "p" 'task-bmk-mode-prev)
;;       (define-key map (kbd "RET") 'task-bmk-mode-go)
;;       (define-key map (kbd "SPC") 'task-bmk-mode-show)
;;       (define-key map "d" 'task-bmk-mode-delete)
;;       (define-key map "g" 'task-bmk-mode-refresh)
;;       (define-key map "q" 'task-bmk-mode-quit)
;;       (setq task-bmk-mode-map map)))
;;
;; (my-keys-define "<f5>" 'task-bmk-toggle)
;; (my-keys-define "<f6>" 'task-bmk-buf-next)
;; (my-keys-define "<S-f6>" 'task-bmk-buf-prev)
;; (my-keys-define "<f7>" 'task-bmk-all-next)
;; (my-keys-define "<S-f7>" 'task-bmk-all-prev)

;;; Code:

(require 'desktop)
(require 'dired)
(require 'ido)

;; Customizable

;;;###autoload
(defgroup task nil
  "Task oriented interface"
  :group 'convenience)

;;;###autoload
(defcustom task-top-dir (concat (getenv "HOME") "/.emacs.d/tasks/")
  "*Top-level directory to save tasks in.
Needs to end with \"/\"."
  :group 'task
  :type 'string)

;;;###autoload
(defcustom task-notes-filename "task_notes.org"
  "*Name of task notes file."
  :group 'task
  :type 'string)

;;;###autoload
(defcustom task-ask-for-confirmation nil
  "*Ask for confirmation before doing 'destructive' things."
  :group 'task
  :type 'boolean)

;;;###autoload
(defcustom task-after-load-hook nil
  "*Hooks to run after a task is loaded."
  :group 'task
  :type 'hook)

;;;###autoload
(defcustom task-extra-vars-to-save '(buffer-history
                                     command-history
                                     compile-history
                                     file-name-history
                                     find-tag-history
                                     function-history
                                     list-command-history
                                     minibuffer-history
                                     query-replace-history
                                     read-command-history
                                     read-expression-history
                                     regexp-history
                                     regexp-search-ring
                                     search-ring
                                     shell-command-history
                                     variable-history)
  "*Extra variables to save as part of task."
  :group 'task
  :type '(repeat symbol))

;;;###autoload
(defcustom task-save-at-exit-name "auto-save"
  "*Name to auto-save current setup to when exiting Emacs."
  :group 'task
  :type 'string)

;;;###autoload
(defcustom task-list-mode-hook nil
  "*Hooks to run when entering task-list-mode"
  :group 'task
  :type 'hook)

;;;###autoload
(defface task-bmk-face
  '((t (:background "dodgerblue4")))
  "Face to highlight bookmarks"
  :group 'task)

;;;###autoload
(defcustom task-bmk-save-in-killed-buffers nil
  "*Save bookmarks when a buffer is killed"
  :group 'task
  :type 'boolean)

;;;###autoload
(defcustom task-bmk-mode-hook nil
  "*Hooks to run when entering task-bmk-mode"
  :group 'task
  :type 'hook)

;; TODO Hooks for: before-save, before-quit

;; Extra variables to save as part of task

(dolist (item task-extra-vars-to-save)
  (add-to-list 'desktop-globals-to-save item t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tasks

;; Variables

(defvar task-name-hist nil
  "Task name history.")

(defvar task-current-name nil
  "Current task name.")

;; Interactive functions

;;;###autoload
(defun task-show-current-name ()
  "Show the current task name in the minibuffer."
  (interactive)
  (if task-current-name
      (message (concat "Current task: " task-current-name))
    (message "No task being worked on")))

;;;###autoload
(defun task-new (&optional name)
  "Create new task."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "New task name? " nil nil nil task-name-hist)))
  (task-quit)
  (make-directory (concat task-top-dir name) t)
  (task-save name))

;;;###autoload
(defun task-save (&optional name)
  "Save task."
  (interactive)
  (if (or name task-current-name)
      (progn
        (desktop-save (concat task-top-dir (or name task-current-name)) t)
        (setq task-current-name (or name task-current-name))
        (task-bmk-save (concat task-top-dir task-current-name "/" task-bmk-save-filename))
        (message (concat "Task \"" task-current-name "\" saved")))
    (task-save-as)))

;;;###autoload
(defun task-save-as ()
  "Save task with a new name."
  (interactive)
  (let ((name (ido-completing-read "Save task as: " (task-get-saved-tasks) nil nil nil task-name-hist)))
    (make-directory (concat task-top-dir name) t)
    (task-save name)))

;;;###autoload
(defun task-load (&optional name)
  "Load task."
  (interactive)
  (unless name
    (setq name (ido-completing-read "Load task: " (task-get-saved-tasks) nil t nil task-name-hist)))
  (if (equal name task-current-name)
      (progn
        (desktop-clear)
        (task-bmk-clear))
    (task-quit))
  (task-bmk-load (concat task-top-dir name "/" task-bmk-save-filename))
  (desktop-read (concat task-top-dir name))
  (if (equal name task-save-at-exit-name)
      (setq task-current-name nil)
    (setq task-current-name name))
  (run-hooks 'task-after-load-hook))

;;;###autoload
(defun task-reload ()
  "Reload the current task."
  (interactive)
  (let ((name task-current-name))
    (task-quit-no-save)
    (task-load name)))

;;;###autoload
(defun task-quit ()
  "Save and quit task."
  (interactive)
  (when task-current-name
    (task-save))
  (task-quit-no-save))

;;;###autoload
(defun task-quit-no-save ()
  "Quit task without saving it."
  (interactive)
  (desktop-clear)
  (task-bmk-clear)
  (setq task-current-name nil))

;;;###autoload
(defun task-notes ()
  "Open task notes."
  (interactive)
  (find-file (concat task-top-dir task-current-name "/" task-notes-filename)))

;;;###autoload
(defun task-list-show ()
  "Show the task list."
  (interactive)
  (get-buffer-create task-list-buffer-name)
  (select-window (split-window-vertically))
  (switch-to-buffer task-list-buffer-name)
  (task-list-mode))

;; Helper functions

(defun task-add-to-mode-line (show)
  "Show task being worked on in mode line."
  (let ((task-elt '(:eval (concat "  [Task: " (or task-current-name "NONE") "]")))
        mode-line)
    (if show
        (unless (member task-elt default-mode-line-format)
          (dolist (elt default-mode-line-format)
            (when (and (stringp elt) (string= elt "-%-"))
              (setq mode-line (append mode-line (list task-elt))))
            (setq mode-line (append mode-line (list elt))))
          (setq default-mode-line-format mode-line))
      (setq default-mode-line-format (delete task-elt default-mode-line-format)))
    show))

(defun task-get-saved-tasks ()
  "Get saved tasks."
  (and (file-exists-p task-top-dir)
       (directory-files task-top-dir nil "^[^.].*" nil)))

;; Auto-save when exiting Emacs

(defun task-kill-emacs-hook ()
  "Save state before killing emacs."
  (when task-current-name
    (task-save))
  (unless (file-exists-p (concat task-top-dir task-save-at-exit-name))
    (make-directory (concat task-top-dir task-save-at-exit-name) t))
  (setq task-current-name task-save-at-exit-name)
  (setq desktop-file-modtime
        (nth 5 (file-attributes (desktop-full-file-name (concat task-top-dir task-save-at-exit-name)))))
  (task-save))

(add-hook 'kill-emacs-hook 'task-kill-emacs-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefix key commands

(define-prefix-command 'task-map)
(define-key task-map (kbd "?") 'task-show-current-name)
(define-key task-map (kbd "n") 'task-new)
(define-key task-map (kbd "s") 'task-save)
(define-key task-map (kbd "a") 'task-save-as)
(define-key task-map (kbd "l") 'task-load)
(define-key task-map (kbd "r") 'task-reload)
(define-key task-map (kbd "q") 'task-quit)
(define-key task-map (kbd "Q") 'task-quit-no-save)
(define-key task-map (kbd "o") 'task-notes)
(define-key task-map (kbd "RET") 'task-list-show)
(define-key task-map (kbd "b") 'task-bmk-show-all)
(define-key task-map (kbd "B") 'task-bmk-show-buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task list mode

(defvar task-list-buffer-name "*task-list*"
  "Task list buffer name.")

(defvar task-list-mode-task-regexp "^[D ] \\[?\\([-a-zA-Z0-9_.:]+\\)\\]?$"
  "Task regexp.")

;; Interactive functions

(defun task-list-mode-next ()
  "Go to next task."
  (interactive)
  (beginning-of-line)
  (unless (eobp)
    (when (looking-at task-list-mode-task-regexp)
      (forward-char))
    (re-search-forward task-list-mode-task-regexp nil t)
    (beginning-of-line)))

(defun task-list-mode-prev ()
  "Go to previous task."
  (interactive)
  (beginning-of-line)
  (re-search-backward task-list-mode-task-regexp nil t)
  (beginning-of-line))

(defun task-list-mode-mark-current ()
  "Mark current task for deletion."
  (interactive)
  (beginning-of-line)
  (when (looking-at task-list-mode-task-regexp)
    (setq buffer-read-only nil)
    (delete-char 1)
    (insert "D")
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (task-list-mode-next)))

(defun task-list-mode-unmark-current ()
  "Unmark current task for deletion."
  (interactive)
  (beginning-of-line)
  (when (looking-at task-list-mode-task-regexp)
    (setq buffer-read-only nil)
    (delete-char 1)
    (insert " ")
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (beginning-of-line)))

(defun task-list-mode-unmark-all ()
  "Unmark all tasks for deletion"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (while (re-search-forward task-list-mode-task-regexp nil t)
      (task-list-mode-unmark-current)
      (end-of-line))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun task-list-mode-execute ()
  "Do marked function on tasks."
  (interactive)
  (let (tasks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward task-list-mode-task-regexp nil t)
        (beginning-of-line)
        (when (= (char-after) ?D)
          (add-to-list 'tasks (match-string-no-properties 1) t))
        (end-of-line)))
    (when (y-or-n-p (concat "Delete tasks: " (mapconcat 'identity tasks ", ") " ? "))
      (dolist (task tasks)
        (when (string= task task-current-name)
          (setq task-current-name nil))
        (dired-delete-file (concat task-top-dir task) 'always))
      (task-list-mode-populate)
      (message "Tasks deleted"))))

(defun task-list-mode-rename ()
  "Rename task at point."
  (interactive)
  (beginning-of-line)
  (if (looking-at task-list-mode-task-regexp)
      (let* ((current-name (match-string-no-properties 1))
             (new-name (read-from-minibuffer
                        (concat "Rename task \"" current-name "\" to? " nil nil nil task-name-hist))))
        (dired-rename-file (concat task-top-dir current-name)
                           (concat task-top-dir new-name) t)
        (when (string= current-name task-current-name)
          (setq task-current-name new-name))
        (task-list-mode-populate))
    (error "Point not on a task")))

(defun task-list-mode-load ()
  "Load task at point."
  (interactive)
  (beginning-of-line)
  (when (looking-at task-list-mode-task-regexp)
    (let ((name (match-string-no-properties 1)))
      (task-list-mode-quit)
      (task-load name))))

(defun task-list-mode-quit ()
  "Quit and kill the current task-list buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

;; Helper functions

(defun task-list-mode-populate ()
  "Populate the task-list-mode buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "% Tasks\n")
  (insert "- -----\n")
  (dolist (task (task-get-saved-tasks))
    (if (string= task-current-name task)
        (insert "  [" task "]\n")
      (insert "  " task "\n")))
  (goto-char (point-min))
  (task-list-mode-next)
  (sort-regexp-fields nil task-list-mode-task-regexp "\\1" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (fit-window-to-buffer nil (/ (frame-height) 2)))

;; Mode map

(defvar task-list-mode-map nil
  "`task-list-mode' keymap.")

(if (not task-list-mode-map)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>") 'task-list-mode-next)
      (define-key map (kbd "<up>") 'task-list-mode-prev)
      (define-key map (kbd "C-n") 'task-list-mode-next)
      (define-key map (kbd "C-p") 'task-list-mode-prev)
      (define-key map "n" 'task-list-mode-next)
      (define-key map "p" 'task-list-mode-prev)
      (define-key map "d" 'task-list-mode-mark-current)
      (define-key map "u" 'task-list-mode-unmark-current)
      (define-key map "U" 'task-list-mode-unmark-all)
      (define-key map "x" 'task-list-mode-execute)
      (define-key map "r" 'task-list-mode-rename)
      (define-key map (kbd "RET") 'task-list-mode-load)
      (define-key map "q" 'task-list-mode-quit)
      (setq task-list-mode-map map)))

;; Font-lock

(defvar task-list-mode-font-lock-keywords
  `(
    ("^% .+$"
     0 font-lock-keyword-face)
    ("^- -+$"
     0 font-lock-keyword-face)
    ("^  \\[.+\\]"
     0 font-lock-type-face)
    (,(concat "^. " task-save-at-exit-name "$")
     0 font-lock-comment-face)
    ("^D.+"
     0 font-lock-warning-face)
    )
  "Keyword highlighting specification for `task-list-mode'.")

;; Mode

(defun task-list-mode ()
  "Major mode for working with the task list.

Key Bindings:

\\{task-list-mode-map}"

  (kill-all-local-variables)
  (setq truncate-lines t)
  (task-list-mode-populate)
  (setq major-mode 'task-list-mode)
  (setq mode-name "task-list")
  (use-local-map task-list-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(task-list-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'task-list-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task bookmarks

(defvar task-bmk-hash (make-hash-table :test 'equal)
  "Task bookmarks.")

(defvar task-bmk-nav-hit-end nil
  "Bookmark navigation hit end.")

(defvar task-bmk-save-alist nil
  "alist to save bookmarks.")

(defvar task-bmk-save-filename ".task-bookmarks"
  "Task bookmark save filename.")

;; Interactive functions

;;;###autoload
(defun task-bmk-toggle ()
  "Toggle bookmark."
  (interactive)
  (let ((ovl (task-bmk-overlay-here)))
    (if ovl
        (delete-overlay ovl)
      (task-bmk-add-overlay))))

;;;###autoload
(defun task-bmk-buf-next ()
  "Jump to the next bookmark in this buffer."
  (interactive)
  (task-bmk-buf-go t))

;;;###autoload
(defun task-bmk-buf-prev ()
  "Jump to the previous bookmark in this buffer."
  (interactive)
  (task-bmk-buf-go nil))

;;;###autoload
(defun task-bmk-all-next ()
  "Jump to the next bookmark in any buffer."
  (interactive)
  (task-bmk-all-go t))

;;;###autoload
(defun task-bmk-all-prev ()
  "Jump to the previous bookmark in any buffer."
  (interactive)
  (task-bmk-all-go nil))

;;;###autoload
(defun task-bmk-show-buf (&optional show-all)
  "Show bookmarks in this buffer."
  (interactive)
  (let* ((buf-name (if show-all task-bmk-buffer-all-name task-bmk-buffer-buf-name))
         (current-buf (current-buffer))
         (bmk-buf (get-buffer-create buf-name)))
    (select-window (or (get-buffer-window bmk-buf) (split-window-vertically)))
    (switch-to-buffer buf-name)
    (task-bmk-mode current-buf show-all)))

;;;###autoload
(defun task-bmk-show-all ()
  "Show bookmarks in all buffers."
  (interactive)
  (task-bmk-show-buf t))

;; Helper functions

(defun task-bmk-overlay-here ()
  "Return bookmark overlay on this line if there is one."
  (let ((ovls (overlays-at (point-at-bol))))
    (catch 'done
      (dolist (ovl ovls)
        (when (task-bmk-overlay ovl)
          (throw 'done ovl))))))

(defun task-bmk-overlay (ovl)
  "Is this overlay a bookmark overlay?"
  (let ((face (plist-get (overlay-properties ovl) 'face)))
    (and face (equal face 'task-bmk-face))))

(defun task-bmk-add-overlay ()
  "Add a bookmark overlay on this line."
  (let ((start (point-at-bol))
        (end (task-bmk-where-to-end))
        ovl)
    (when (< start end)
      (setq ovl (make-overlay start end))
      (overlay-put ovl 'evaporate t)
      (overlay-put ovl 'face 'task-bmk-face)
      (overlay-put ovl 'modification-hooks '(task-bmk-adjust))
      (overlay-put ovl 'insert-in-front-hooks '(task-bmk-adjust-in-front))
      (overlay-put ovl 'insert-behind-hooks '(task-bmk-adjust)))))

(defun task-bmk-where-to-end ()
  "Figure out where the bookmark should end."
  (min (1+ (point-at-eol)) (point-max)))

(defun task-bmk-adjust (ovl after begin end &optional len)
  "Adjust overlay when inserting inside or behind."
  (when after
    (let ((start (overlay-start ovl)))
      (when start
        (move-overlay ovl start (save-excursion
                                  (goto-char start)
                                  (task-bmk-where-to-end)))))))

(defun task-bmk-adjust-in-front (ovl after begin end &optional len)
  "Adjust overlay when inserting in front."
  (when after
    (move-overlay ovl (point-at-bol) (task-bmk-where-to-end))))

(defun task-bmk-buf-go (next)
  "Jump to the next/prev bookmark in this buffer."
  (unless (member last-command (list 'task-bmk-buf-next
                                     'task-bmk-buf-prev
                                     'task-bmk-all-next
                                     'task-bmk-all-prev))
    (task-bmk-scan-current-buffer))
  (unless (equal last-command (if next 'task-bmk-buf-next 'task-bmk-buf-prev))
    (setq task-bmk-nav-hit-end nil))
  (let ((bmks (gethash (buffer-file-name) task-bmk-hash))
        (curr-line (if task-bmk-nav-hit-end
                       (if next 0 most-positive-fixnum)
                     (line-number-at-pos (point-at-bol)))))
    (setq task-bmk-nav-hit-end nil)
    (if (not bmks)
        (message "No bookmarks in buffer.")
      (catch 'done
        (dolist (bmk (if next bmks (reverse bmks)))
          (when (if next (> (car bmk) curr-line) (< (car bmk) curr-line))
            (task-bmk-goto-line (car bmk))
            (beginning-of-line)
            (throw 'done t)))
        (setq task-bmk-nav-hit-end t)
        (message (concat "Reached " (if next "end" "start") " of buffer."))))))

(defun task-bmk-all-go (next)
  "Jump to next/prev bookmark in all buffers."
  (unless (member last-command (list 'task-bmk-all-next 'task-bmk-all-prev))
    (task-bmk-scan-all))
  (let ((filenames (task-bmk-get-sorted-filenames))
        (curr-filename (buffer-file-name)))
    (if (not filenames)
        (message "No bookmarks in any files.")
      (unless next (setq filenames (nreverse filenames)))
      (if (not (member curr-filename filenames))
          (let* ((filename (car filenames))
                 (bmks (gethash filename task-bmk-hash))
                 (bmk (if next (car bmks) (car (last bmks)))))
            (find-file filename)
            (task-bmk-goto-line (car bmk))
            (beginning-of-line))
        ;; Rotate list around so current filename is at head
        (catch 'done
          (dolist (filename filenames)
            (when (equal filename curr-filename)
              (throw 'done t))
            (setq filenames (nconc (cdr filenames) (list filename)))))
        (task-bmk-all-go-from-here filenames next)))))

(defun task-bmk-all-go-from-here (filenames next)
  "Go from here to next bookmark in this buffer, or next buffer, or wrap around."
  (let ((bmks (gethash (car filenames) task-bmk-hash))
        (curr-line (line-number-at-pos (point))))
    ;; First look for next bookmark in this buffer
    (unless (catch 'done
              (dolist (bmk (if next bmks (reverse bmks)))
                (when (if next (> (car bmk) curr-line) (< (car bmk) curr-line))
                  (task-bmk-goto-line (car bmk))
                  (beginning-of-line)
                  (throw 'done t))))
      ;; Past next/prev one in this buffer, see if there is another file with bookmarks
      (if (> (length filenames) 1)
          (let* ((bmks (gethash (cadr filenames) task-bmk-hash))
                 (bmk (if next (car bmks) (car (last bmks)))))
            (find-file (cadr filenames))
            (task-bmk-goto-line (car bmk))
            (beginning-of-line))
        ;; Goto first/last bookmark in this buffer
        (let ((bmk (if next (car bmks) (car (last bmks)))))
          (task-bmk-goto-line (car bmk))
          (beginning-of-line))))))

(defun task-bmk-goto-line (line)
  "Go to a line but don't set mark like `goto-line' does."
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

(defun task-bmk-scan-current-buffer ()
  "Scan current buffer for bookmarks."
  (task-bmk-scan-buffer (current-buffer)))

(defun task-bmk-kill-buffer-hook ()
  "Scan buffer or delete bookmarks."
  (if task-bmk-save-in-killed-buffers
      (task-bmk-scan-current-buffer)
    (remhash (buffer-file-name) task-bmk-hash)))

(add-hook 'kill-buffer-hook 'task-bmk-kill-buffer-hook)

(defun task-bmk-scan-buffer (buf)
  "Scan a buffer for bookmarks."
  (with-current-buffer buf
    (let ((ovls (overlays-in (point-min) (point-max)))
          bmk-list)
      (dolist (ovl ovls)
        (when (task-bmk-overlay ovl)
          (push (cons (line-number-at-pos (overlay-start ovl))
                      (buffer-substring (overlay-start ovl) (overlay-end ovl)))
                bmk-list)))
      (remhash (buffer-file-name) task-bmk-hash)
      (when bmk-list
        (puthash (buffer-file-name)
                 (sort bmk-list (lambda (a b) (< (car a) (car b))))
                 task-bmk-hash)))))

(defun task-bmk-scan-all ()
  "Scan all relevant buffers."
  (dolist (buf (buffer-list))
    (unless (string-match "^[ *].+$" (buffer-name buf))
      (task-bmk-scan-buffer buf))))

(defun task-bmk-get-sorted-filenames ()
  "Get bookmark filenames in sorted order."
  (let (filenames)
    (maphash (lambda (filename bmks) (push filename filenames)) task-bmk-hash)
    (sort filenames (lambda (a b) (string< a b)))))

(defun task-bmk-save (filename)
  "Save bookmarks to filename."
  (task-bmk-scan-all)
  (setq task-bmk-save-alist nil)
  (maphash (lambda (filename bmks)
             (push (cons filename bmks) task-bmk-save-alist)) task-bmk-hash)
  (find-file filename)
  (erase-buffer)
  (insert "(setq task-bmk-save-alist '" (prin1-to-string task-bmk-save-alist) ")\n")
  (save-buffer)
  (kill-buffer))

(defun task-bmk-clear ()
  "Clear out bookmarks."
  (clrhash task-bmk-hash))

(defun task-bmk-load (filename)
  "Load bookmarks from file."
  (when (file-exists-p filename)
    (load-file filename)
    (clrhash task-bmk-hash)
    (let (filename buf)
      (dolist (key-value-pair task-bmk-save-alist)
        (setq filename (car key-value-pair))
        (puthash filename (cdr key-value-pair) task-bmk-hash)
        (setq buf (get-file-buffer filename))
        (when buf
          (task-bmk-add-bookmarks buf))))))

(defun task-bmk-add-bookmarks (buf)
  "Add bookmarks to buffer."
  (with-current-buffer buf
    (let ((bmks (gethash (buffer-file-name buf) task-bmk-hash)))
      (save-excursion
        (dolist (bmk bmks)
          (task-bmk-goto-line (car bmk))
          (task-bmk-toggle))))))

(defun task-bmk-add-bookmarks-current-buffer ()
  "Add bookmarks to current buffer."
  (task-bmk-add-bookmarks (current-buffer)))

(add-hook 'find-file-hook 'task-bmk-add-bookmarks-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task bookmark mode

(defvar task-bmk-buffer-buf-name "*task-buf-bookmarks*"
  "Task bookmark buffer name.")

(defvar task-bmk-buffer-all-name "*task-all-bookmarks*"
  "Task bookmark buffer name.")

(defvar task-bmk-line-regexp "^\\s-*\\([0-9]+\\)|"
  "Task bookmark line regexp.")

(defvar task-bmk-filename-regexp "^[([]\\(.+\\)[])]"
  "Task bookmark filename regexp.")

;; Interactive functions

(defun task-bmk-mode-next ()
  "Go to next bookmark."
  (interactive)
  (end-of-line)
  (re-search-forward task-bmk-line-regexp nil 'go)
  (beginning-of-line)
  (when (eobp)
    (ding)
    (forward-line -1)))

(defun task-bmk-mode-prev ()
  "Go to previous bookmark."
  (interactive)
  (beginning-of-line)
  (re-search-backward task-bmk-line-regexp nil 'go)
  (when (bobp)
    (ding)
    (forward-line 1)))

(defun task-bmk-mode-go ()
  "Go to current bookmark."
  (interactive)
  (beginning-of-line)
  (let* ((info (task-bmk-mode-get-info))
         (filename (car info))
         (line-num (cdr info)))
    (task-bmk-mode-quit)
    (find-file filename)
    (task-bmk-goto-line line-num)))

(defun task-bmk-mode-show ()
  "Show current bookmark."
  (interactive)
  (beginning-of-line)
  (let* ((bmk-buf (current-buffer))
         (info (task-bmk-mode-get-info))
         (filename (car info))
         (line-num (cdr info))
         (refresh (not (get-file-buffer filename))))
    (find-file-other-window filename)
    (select-window (get-buffer-window (get-file-buffer filename)))
    (task-bmk-goto-line line-num)
    (select-window (get-buffer-window bmk-buf))
    (when refresh
      (setq line-num (line-number-at-pos (point)))
      (task-bmk-mode-refresh)
      (task-bmk-goto-line line-num))))

(defun task-bmk-mode-delete ()
  "Delete current bookmark."
  (interactive)
  (let* ((info (task-bmk-mode-get-info))
         (filename (car info))
         (line-num (cdr info))
         (buf (get-file-buffer filename)))
    (when (or (not task-ask-for-confirmation)
              (y-or-n-p "Delete this bookmark? "))
      (if buf
          (with-current-buffer buf
            (save-excursion
              (task-bmk-goto-line line-num)
              (task-bmk-toggle)))
        (let ((old-bmks (gethash filename task-bmk-hash))
              new-bmks)
          (dolist (bmk old-bmks)
            (unless (= (car bmk) line-num)
              (push bmk new-bmks)))
          (if new-bmks
              (puthash filename
                       (sort new-bmks (lambda (a b) (< (car a) (car b))))
                       task-bmk-hash)
            (remhash filename task-bmk-hash))))
      (task-bmk-mode-refresh))))

(defun task-bmk-mode-refresh ()
  "Refresh the bookmark list."
  (interactive)
  (let (current-buf show-all)
    (if (equal (buffer-name) task-bmk-buffer-all-name)
        (setq show-all t)
      (let (filename)
        (goto-char (point-min))
        (re-search-forward task-bmk-filename-regexp nil t)
        (setq filename (match-string-no-properties 1))
        (setq current-buf (get-file-buffer filename))))
    (unless (task-bmk-mode-populate current-buf show-all)
      (task-bmk-mode-quit)
      (if show-all
          (message "No bookmarks in any files.")
        (message "No bookmarks in this buffer.")))))

(defun task-bmk-mode-quit ()
  "Quit task bookmark window."
  (interactive)
  (kill-buffer nil)
  (delete-window))

;; Helper functions

(defun task-bmk-mode-populate (current-buf show-all)
  "Populate the task-bmk-mode buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (task-bmk-scan-all)
  (let ((filenames (if show-all (task-bmk-get-sorted-filenames) (list (buffer-file-name current-buf))))
        (bmk-width 0))
    (maphash (lambda (filename bmks)
               (when (member filename filenames)
                 (dolist (bmk bmks)
                   (setq bmk-width (max (1+ (floor (log10 (car bmk)))) bmk-width))))) task-bmk-hash)
    (when (> bmk-width 0)
      (let ((fmt (concat "%" (number-to-string bmk-width) "s"))
            buffer-loaded start end)
        (dolist (filename filenames)
          (setq buffer-loaded (get-file-buffer filename))
          (setq start (point))
          (if buffer-loaded
              (insert "[" filename "]\n")
            (insert "(" filename ")\n"))
          (setq end (point))
          (add-text-properties start end (if buffer-loaded
                                             '(face font-lock-variable-name-face)
                                           '(face font-lock-comment-face)))
          (dolist (bmk (gethash filename task-bmk-hash))
            (insert (format fmt (car bmk)) "|" (cdr bmk)))
          (insert "\n")))
      (delete-char -1)
      (goto-char (point-min))
      (task-bmk-mode-next)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (fit-window-to-buffer nil (/ (frame-height) 2)))))

(defun task-bmk-mode-get-info ()
  "Get (filename . line-num) for bookmark."
  (if (looking-at task-bmk-line-regexp)
      (let ((line-num (string-to-number (match-string-no-properties 1)))
            filename)
        (save-excursion
          (re-search-backward task-bmk-filename-regexp nil t)
          (cons (match-string-no-properties 1) line-num)))
    (error "Not on a bookmark.")))

;; Mode map

(defvar task-bmk-mode-map nil
  "`task-bmk-mode' keymap.")

(if (not task-bmk-mode-map)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>") 'task-bmk-mode-next)
      (define-key map (kbd "<up>") 'task-bmk-mode-prev)
      (define-key map (kbd "C-n") 'task-bmk-mode-next)
      (define-key map (kbd "C-p") 'task-bmk-mode-prev)
      (define-key map "n" 'task-bmk-mode-next)
      (define-key map "p" 'task-bmk-mode-prev)
      (define-key map (kbd "RET") 'task-bmk-mode-go)
      (define-key map (kbd "SPC") 'task-bmk-mode-show)
      (define-key map "d" 'task-bmk-mode-delete)
      (define-key map "g" 'task-bmk-mode-refresh)
      (define-key map "q" 'task-bmk-mode-quit)
      (setq task-bmk-mode-map map)))

;; Mode

(defun task-bmk-mode (current-buf show-all)
  "Major mode for working with task bookmarks.
Key Bindings:

\\{task-bmk-mode-map}"

  (kill-all-local-variables)
  (setq truncate-lines t)
  (if (task-bmk-mode-populate current-buf show-all)
      (progn
        (setq major-mode 'task-bmk-mode)
        (setq mode-name "task-bm")
        (use-local-map task-bmk-mode-map)
        (run-hooks 'task-bmk-mode-hook))
    (task-bmk-mode-quit)
    (if show-all
        (message "No bookmarks in any files.")
      (message "No bookmarks in this buffer."))))

(provide 'task)
;;; task.el ends here
