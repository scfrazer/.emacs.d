;;; my-ediff.el

(require 'ediff)

(setq-default ediff-ignore-similar-regions t
              ediff-keep-variants t
              ediff-split-window-function 'split-window-vertically
              ediff-window-setup-function 'ediff-setup-windows-plain)

(defun my-ediff-buffer-with-file ()
  "View the differences between current buffer and it's associated file using ediff."
  (interactive)
  (let* ((modified-buffer (current-buffer))
         (filename (or (buffer-file-name modified-buffer)
                       (error "Buffer %s has no associated file" modified-buffer)))
         (mode major-mode)
         (original-buffer (get-buffer-create "*my-ediff-buffer-with-file*")))
    (set-buffer original-buffer)
    (insert-file-contents filename)
    (call-interactively mode)
    (ediff-buffers original-buffer modified-buffer)))

(defun my-ediff-quit (reverse-default-keep-variants)
  "Don't ask if I really want to quit"
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (ediff-really-quit reverse-default-keep-variants))

(defvar my-ediff-window-config nil)

(defun my-ediff-before-setup-hook ()
  (setq my-ediff-window-config (current-window-configuration)))

(defun my-ediff-keymap-setup-hook ()
  (define-key ediff-mode-map "q" 'my-ediff-quit))

(defun my-ediff-quit-hook ()
  (set-window-configuration my-ediff-window-config)
  (let (buf (buf-list (buffer-list)))
    ;; Kill Clearcase revision buffers
    (mapc (lambda (x)
            (when (string-match ".+?@@.+?[0-9]+$" (buffer-name x))
              (kill-buffer x)))
          buf-list)
    ;; Kill vc revision buffers
    (mapc (lambda (x)
            (when (string-match "\\.~.+~$" (buffer-name x))
              (kill-buffer x)))
          buf-list)
    ;; Cleanup ediff buffers
    (mapc (lambda (x)
            (when (setq buf (get-buffer x))
              (kill-buffer buf)))
          (list "*Ediff Control Panel*"
                "*Ediff Registry*"
                "*ediff-diff*"
                "*ediff-errors*"
                "*ediff-fine-diff*"
                "*my-ediff-buffer-with-file*"))))

(add-hook 'ediff-before-setup-hook 'my-ediff-before-setup-hook)
(add-hook 'ediff-keymap-setup-hook 'my-ediff-keymap-setup-hook)
(add-hook 'ediff-quit-hook 'my-ediff-quit-hook)

(provide 'my-ediff)
