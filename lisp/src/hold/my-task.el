;;; my-task.el

(require 'task)
;; (require 'my-clearcase)

(defvar my-task-saving-bookmarks nil)

(defadvice bm-buffer-save (around my-task-bm-buffer-save activate)
  (if (and (featurep 'bm) my-task-saving-bookmarks)
      (let ((bm-buffer-persistence t))
        ad-do-it)
    ad-do-it))

(defun my-task-before-save-hook ()
  "Save bookmarks file."
  (when (featurep 'bm)
    (setq my-task-saving-bookmarks t)
    (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
      (bm-buffer-save-all)
      (bm-repository-save filename))
    (setq my-task-saving-bookmarks nil)))

(add-hook 'task-before-save-hook 'my-task-before-save-hook)

(defun my-task-after-load-hook ()
  "Load bookmarks file if one exists."
  (when (featurep 'bm)
    (let ((filename (concat task-top-dir task-current-name "/bookmarks.el")))
      (when (file-exists-p filename)
        (bm-repository-load filename)
        (bm-buffer-restore-all))))
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (set-buffer buf)
      ;; (when (and use-clearcase clearcase-setview-viewtag)
      ;;   (clearcase-hook-find-file-hook))
      (when (and (featurep 'bm) bm-buffer-persistence)
        (bm-toggle-buffer-persistence)))))

(add-hook 'task-after-load-hook 'my-task-after-load-hook)

(provide 'my-task)
