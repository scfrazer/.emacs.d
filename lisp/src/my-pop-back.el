;;; my-pop-back.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ffap

(require 'ffap)

(defvar my-pop-back-ffap-stack nil)

(defadvice find-file-at-point (around my-pop-back-find-file-hook activate)
  (let ((orig-buf (current-buffer))
        (orig-file (buffer-file-name))
        (orig-point (point)))
    ad-do-it
    (unless (equal orig-buf (current-buffer))
      (push (list orig-buf orig-file orig-point) my-pop-back-ffap-stack))))

(defun my-pop-back-ffap (&optional kill-buf)
  "Pop back from last find-file-at-point"
  (interactive "P")
  (when my-pop-back-ffap-stack
    (let* ((item (car my-pop-back-ffap-stack))
           (buf (nth 0 item))
           (file (nth 1 item))
           (point (nth 2 item)))
      (if (buffer-live-p buf)
          (let ((prev-buf (current-buffer)))
            (switch-to-buffer buf)
            (goto-char point)
            (when (and kill-buf (not (equal (current-buffer) prev-buf)))
              (kill-buffer prev-buf)))
        (when file
          (find-file file)
          (goto-char point)
          (when kill-buf
            (kill-buffer (other-buffer))))))
    (setq my-pop-back-ffap-stack (cdr my-pop-back-ffap-stack))))

(defun my-pop-back-ffap-kill-buffer ()
  "Pop back from last find-file-at-point and kill the buffer"
  (interactive)
  (my-pop-back-ffap t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu

(defvar my-pop-back-imenu-stack nil)
(make-variable-buffer-local 'my-pop-back-imenu-stack)

(defun my-pop-back-imenu-save-pos ()
  (push (point) my-pop-back-imenu-stack))

(defadvice imenu-default-goto-function (before my-imenu-default-goto-function activate)
  (my-pop-back-imenu-save-pos))

(defun my-pop-back-imenu ()
  "Pop back from last imenu jump"
  (interactive)
  (when my-pop-back-imenu-stack
    (goto-char (car my-pop-back-imenu-stack))
    (recenter)
    (setq my-pop-back-imenu-stack (cdr my-pop-back-imenu-stack))))

(provide 'my-pop-back)
