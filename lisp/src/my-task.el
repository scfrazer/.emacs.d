;;; my-task.el

(require 'task)
(require 'my-clearcase)
(require 'bm)

;; (when (and clearcase-servers-online clearcase-setview-viewtag)
;;   (setq task-save-at-exit-name clearcase-setview-viewtag))

(defvar my-task-saving-bookmarks nil)

(defadvice bm-buffer-save (around my-task-bm-buffer-save activate)
  (if my-task-saving-bookmarks
      (let ((bm-buffer-persistence t))
        ad-do-it)
    ad-do-it))

(defun my-task-before-save-hook ()
  "Save bookmarks file."
  (setq my-task-saving-bookmarks t)
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
    (bm-buffer-save-all)
    (bm-repository-save filename))
  (setq my-task-saving-bookmarks nil))

(add-hook 'task-before-save-hook 'my-task-before-save-hook)

(defun my-task-after-load-hook ()
  "Load bookmarks file if one exists."
  (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
    (when (file-exists-p filename)
      (bm-repository-load filename)
      (bm-buffer-restore-all)))
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (set-buffer buf)
      (when clearcase-setview-viewtag
        (clearcase-hook-find-file-hook))
      (when bm-buffer-persistence
        (bm-toggle-buffer-persistence)))))

(add-hook 'task-after-load-hook 'my-task-after-load-hook)

(provide 'my-task)
