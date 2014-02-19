;;; my-task.el

(require 'task)
(require 'bm)

(defvar my-task-saving-bookmarks nil)

(defadvice bm-buffer-save (around my-task-bm-buffer-save activate)
  (if my-task-saving-bookmarks
      (let ((bm-buffer-persistence t))
        ad-do-it)
    ad-do-it))

(defun my-task-before-save-hook ()
  "Save bookmarks file."
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el"))
        (my-task-saving-bookmarks t))
    (bm-buffer-save-all)
    (bm-repository-save filename)))

(add-hook 'task-before-save-hook 'my-task-before-save-hook)

(defvar my-task-loading-bookmarks nil)

(defadvice bm-bookmark-add (around my-task-bm-bookmark-add activate)
  (if my-task-loading-bookmarks
      (let ((bm-buffer-persistence nil))
        ad-do-it)
    ad-do-it))

(defun my-task-after-load-hook ()
  "Load bookmarks file if one exists."
  (when clearcase-setview-viewtag
    (dolist (buf (buffer-list))
      (when (buffer-file-name buf)
        (set-buffer buf)
        (clearcase-hook-find-file-hook))))
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el"))
        (my-task-loading-bookmarks t))
    (when (file-exists-p filename)
      (bm-repository-load filename)
      (bm-buffer-restore-all))))

(add-hook 'task-after-load-hook 'my-task-after-load-hook)

(provide 'my-task)
