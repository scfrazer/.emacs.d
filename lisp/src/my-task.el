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
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
    (setq my-task-saving-bookmarks t)
    (bm-buffer-save-all)
    (bm-repository-save filename)
    (setq my-task-saving-bookmarks nil)))

(add-hook 'task-before-save-hook 'my-task-before-save-hook)

(defvar my-task-loading-bookmarks nil)

(defadvice bm-bookmark-add (around my-task-bm-bookmark-add activate)
  (if my-task-loading-bookmarks
      (let ((bm-buffer-persistence nil))
        ad-do-it)
    ad-do-it))

(defun my-task-after-load-hook ()
  "Load bookmarks file if one exists."
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
    (when (file-exists-p filename)
      (setq my-task-loading-bookmarks t)
      (bm-repository-load filename)
      (bm-buffer-restore-all)
      (setq my-task-loading-bookmarks nil))))

(add-hook 'task-after-load-hook 'my-task-after-load-hook)

(provide 'my-task)
