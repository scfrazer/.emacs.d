;;; my-mode-line.el

;; (require 'my-clearcase)
;; (defvar clearcase-setview-viewtag)

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(defface my-narrow-face
  '((t :inherit warning))
  "todo/fixme highlighting."
  :group 'faces)

(defface my-read-only-face
  '((t (:foreground "black" :background "orange2")))
  "Read-only buffer highlighting."
  :group 'faces)

(defface my-modified-face
  '((t :inherit error))
  "Modified buffer highlighting."
  :group 'faces)

(setq-default
 mode-line-format
 '("  "
   (:eval (let ((str (if buffer-read-only
                         (if (buffer-modified-p) "%%*" "%%%%")
                       (if (buffer-modified-p) "**" "--"))))
            (if buffer-read-only
                (propertize str 'face 'my-read-only-face)
              (if (buffer-modified-p)
                  (propertize str 'face 'my-modified-face)
                str))))
   (:eval (if (> (length (frame-list)) 1) "  %F" ""))
   (list 'line-number-mode "  ")
   (:eval (when line-number-mode
            (let ((str "L%l"))
              (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                (setq str (concat str "/" my-mode-line-buffer-line-count)))
              (if (/= (buffer-size) (- (point-max) (point-min)))
                  (propertize str 'face 'my-narrow-face)
                str))))
   "  %o"
   (list 'column-number-mode "  C%C")
   "  " mode-line-buffer-identification
   "  " mode-line-modes
   (:eval (when (and (boundp 'xterm-mouse-mode) xterm-mouse-mode) "  ◓"))
   (:eval (let ((workspace-path (getenv "WORKSPACE")))
            (when workspace-path
              (concat "  >" (file-name-nondirectory workspace-path) "<"))))
   (:eval (if (and (featurep 'task) task-current-name)
              (concat "  [Task: " (or task-current-name "NONE") "]")
            ""))))

(nbutlast mode-line-modes 1)

(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(defadvice narrow-to-region (after my-mode-line-ntr activate)
  (when (not (buffer-modified-p))
    (my-mode-line-count-lines)))

(defadvice narrow-to-defun (after my-mode-line-ntd activate)
  (when (not (buffer-modified-p))
    (my-mode-line-count-lines)))

(provide 'my-mode-line)
