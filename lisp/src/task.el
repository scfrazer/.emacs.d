;;; task.el --- Work with named tasks

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
;; TODO
;; Do this: (require 'task)
;;          (task-add-to-mode-line t)
;;          (global-set-key (kbd "C-x t") 'task-map)
;; auto-save, task-win
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
(defcustom task-notes-filename "notes.org"
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
(defcustom task-before-save-hook nil
  "*Hooks to run before a task is saved."
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
        (setq task-current-name (or name task-current-name))
        (run-hooks 'task-before-save-hook)
        (desktop-save (concat task-top-dir task-current-name) t)
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
      (desktop-clear)
    (task-quit))
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

(provide 'task)
;;; task.el ends here
