;;; my-flymake.el

(require 'flymake)
(require 'popup)

(setq-default flymake-no-changes-timeout nil
              flymake-start-on-flymake-mode nil
              flymake-suppress-zero-counters t
              flymake-wrap-around nil)

(setq-default popup-tip-max-width 160)

(setq-default flymake-diagnostic-types-alist
  `((:error
     . ((flymake-category . flymake-error)
        (priority . -1)))
    (:warning
     . ((flymake-category . flymake-warning)
        (priority . -2)))
    (:note
     . ((flymake-category . flymake-note)
        (priority . -3)))))

(put 'flymake-note 'mode-line-face 'caution)

(defface my-flymake-error-face
  '((t (:inherit popup-tip-face :foreground "red3" :italic nil)))
  "Error dot face."
  :group 'faces)

(defface my-flymake-warning-face
  '((t (:inherit popup-tip-face :foreground "yellow1" :italic nil)))
  "Warning dot face."
  :group 'faces)

(defface my-flymake-note-face
  '((t (:inherit popup-tip-face :foreground "dodgerblue3" :italic nil)))
  "Note dot face."
  :group 'faces)

(defun my-flymake-show-current-error ()
  "Show the current error point is on."
  (interactive)
  (when (and (boundp 'flymake-mode) flymake-mode)
    (let ((diags (flymake-diagnostics (point)))
          (msg "") text)
      (dolist (diag diags)
        (unless (string= msg "")
          (setq msg (concat msg "\n")))
        (setq text (concat (propertize " ● " 'face
                                       (cl-case (flymake-diagnostic-type diag)
                                         (:error 'my-flymake-error-face)
                                         (:warning 'my-flymake-warning-face)
                                         (:note 'my-flymake-note-face)))
                           (propertize (concat (flymake-diagnostic-text diag) " ") 'face 'popup-tip-face)))
        (setq msg (concat msg text)))
      (when diags
        ;; (popup-tip msg :nostrip t)
        (message msg)))))

(defvar my-flymake-timer nil)
(unless my-flymake-timer
  (setq my-flymake-timer (run-with-idle-timer 1.0 t 'my-flymake-show-current-error)))

(provide 'my-flymake)
