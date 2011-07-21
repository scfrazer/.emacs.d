;;; my-mode-line.el

(require 'my-clearcase)
(require 'task)

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(setq-default mode-line-format
      '("  " mode-line-modified
        (list 'line-number-mode "  ")
        (:eval (if line-number-mode
                   (if (/= (buffer-size) (- (point-max) (point-min)))
                       (propertize "L%l" 'face 'my-todo-face)
                     "L%l")))
        (:eval (if (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                   (concat "/" my-mode-line-buffer-line-count)
                 ""))
        "  %p"
        (list 'column-number-mode "  C%c")
        "  " mode-line-buffer-identification
        "  " mode-line-modes
        (:eval (if (and clearcase-servers-online clearcase-setview-viewtag)
                   (concat "  [View: " clearcase-setview-viewtag "]")
                 ""))
        (:eval (if (not task-current-name)
                   ""
                 (concat "  [Task: " (or task-current-name "NONE") "]")))))

(nbutlast mode-line-modes 1)

(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(provide 'my-mode-line)
