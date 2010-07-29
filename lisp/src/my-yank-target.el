;;; my-yank-target.el

(defvar my-yank-target-marker (make-marker))
(defvar my-yank-target-source (make-marker))

(defun my-yank-target-set ()
  "Set yank target point."
  (interactive)
  (setq my-yank-target-marker (point-marker))
  (set-marker-insertion-type my-yank-target-marker t)
  (message "Yank target set"))

(defun my-yank-target-yank (&optional kill)
  "Yank to target."
  (interactive)
  (if (or (not (marker-buffer my-yank-target-marker))
          (not (marker-position my-yank-target-marker)))
      (error "No yank target")
    (let (start end text)
      (if (region-active-p)
          (setq start (region-beginning)
                end (region-end))
        (setq start (point-at-bol)
              end (point-at-bol 2)))
      (setq text (buffer-substring start end))
      (when kill
        (kill-region start end))
      (save-excursion
        (with-current-buffer (marker-buffer my-yank-target-marker)
          (goto-char (marker-position my-yank-target-marker))
          (setq start (point))
          (insert text)
          (when (not (member indent-line-function '(indent-relative sh-basic-indent-line)))
            (indent-region start (point)))))
      (setq my-yank-target-source (point-marker))
      (if (region-active-p)
          (progn (deactivate-mark)
                 (message "Sent region to target"))
        (message "Sent line to target")))))

(defun my-yank-target-kill ()
  "Kill to target."
  (interactive)
  (my-yank-target-yank 'kill))

(defun my-yank-target-go-target ()
  "Go to yank target."
  (interactive)
  (if (or (not (marker-buffer my-yank-target-marker))
          (not (marker-position my-yank-target-marker)))
      (error "Yank target buffer killed or position no longer exists")
    (setq my-yank-target-source (point-marker))
    (switch-to-buffer (marker-buffer my-yank-target-marker))
    (goto-char (marker-position my-yank-target-marker))))

(defun my-yank-target-yank-and-go ()
  "Yank to target and go there."
  (interactive)
  (my-yank-target-yank)
  (my-yank-target-go-target))

(defun my-yank-target-kill-and-go ()
  "Kill to target and go there."
  (interactive)
  (my-yank-target-kill)
  (my-yank-target-go-target))

(defun my-yank-target-go-source ()
  "Go to yank target source."
  (interactive)
  (if (or (not (marker-buffer my-yank-target-source))
          (not (marker-position my-yank-target-source)))
      (error "Yank source buffer killed or position no longer exists")
    (switch-to-buffer (marker-buffer my-yank-target-source))
    (goto-char (marker-position my-yank-target-source))))

(define-prefix-command 'my-yank-target-map)
(define-key my-yank-target-map (kbd "C-SPC") 'my-yank-target-set)
(define-key my-yank-target-map (kbd "y") 'my-yank-target-yank)
(define-key my-yank-target-map (kbd "C-y") 'my-yank-target-yank-and-go)
(define-key my-yank-target-map (kbd "k") 'my-yank-target-kill)
(define-key my-yank-target-map (kbd "C-k") 'my-yank-target-kill-and-go)
(define-key my-yank-target-map (kbd "t") 'my-yank-target-go-target)
(define-key my-yank-target-map (kbd "s") 'my-yank-target-go-source)

(provide 'my-yank-target)
