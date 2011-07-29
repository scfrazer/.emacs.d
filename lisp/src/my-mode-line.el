;;; my-mode-line.el

(require 'narrow-nested)
(require 'my-clearcase)
(require 'task)

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(defface my-narrow-face
  '((t (:box t :foreground "firebrick2" :background "yellow")))
  "todo/fixme highlighting."
  :group 'faces)

(setq-default mode-line-format
      '("  " mode-line-modified
        (list 'line-number-mode "  ")
        (:eval (when line-number-mode
                 (let ((str "L%l"))
                   (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                     (setq str (concat str "/" my-mode-line-buffer-line-count)))
                   (if (/= (buffer-size) (- (point-max) (point-min)))
                       (propertize str 'face 'my-narrow-face)
                     str))))
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

(defadvice narrow-nested-dwim (after my-mode-line-nnd activate)
  (when (not (buffer-modified-p))
    (my-mode-line-count-lines)))

(provide 'my-mode-line)
