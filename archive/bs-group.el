(require 'bs)

(defvar bs-group-buffer-tags nil
  "*List of group tags for a buffer")
(make-variable-buffer-local 'bs-group-buffer-tags)

(defvar bs-group-active-tag nil
  "*Active tag for showing buffer list")

(defun bs-group-add-buffer-tag (tag)
  (interactive "sAdd buffer tag: ")
  (setq bs-group-buffer-tags (add-to-list 'bs-group-buffer-tags tag)))

(defun bs-group-delete-buffer-tag (tag)
  (interactive "sRemove buffer tag: ")
  (setq bs-group-buffer-tags (delete tag bs-group-buffer-tags)))

(defun bs-group-set-active-tag (tag)
  (interactive "sSet active tag: ")
  (setq bs-group-active-tag tag))

(defun bs-group-must-show-function (buf)
  (save-excursion
    (set-buffer buf)
    (member bs-group-active-tag bs-group-buffer-tags)))

(defun bs-group-dont-show-function (buf)
  (not (bs-group-must-show-function buf)))

(setq bs-configurations
      (add-to-list 'bs-configurations
                   '("tagged"
                     nil bs-group-must-show-function
                     nil bs-group-dont-show-function
                     nil)))

(provide 'bs-group)
