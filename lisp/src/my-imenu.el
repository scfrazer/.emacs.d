;;; my-imenu-extras.el

(require 'imenu)

(setq imenu-max-items 25)
(setq imenu-max-length 100)
(setq imenu-sort-function nil)
(add-hook 'imenu-after-jump-hook 'recenter)

(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (and index-item
       (progn
         (push-mark)
         (let* ((is-special-item (listp (cdr index-item)))
                (function
                 (if is-special-item
                     (nth 2 index-item) imenu-default-goto-function))
                (position (if is-special-item
                              (cadr index-item) (cdr index-item)))
                (rest (if is-special-item (cddr index-item))))
           (apply function (car index-item) position rest)))
       (run-hooks 'imenu-after-jump-hook)))

(provide 'my-imenu)
