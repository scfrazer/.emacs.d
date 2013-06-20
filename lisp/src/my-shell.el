;;; my-shell.el

;; C-c comint-interrupt-subjob

(defun my-shell-mode-hook ()
  (define-key shell-mode-map (kbd "C-c C-p") 'comint-previous-input)
  (define-key shell-mode-map (kbd "C-c C-n") 'comint-next-input)
  (define-key shell-mode-map (kbd "C-c M-p") 'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "C-c M-n") 'comint-next-matching-input-from-input)
  (define-key shell-mode-map (kbd "C-c M-.") 'comint-insert-previous-argument)
  (define-key shell-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)
  (define-key shell-mode-map (kbd "C-c C-z") 'comint-stop-subjob)
  (define-key shell-mode-map (kbd "C-c C-d") 'comint-send-eof)
  (ansi-color-for-comint-mode-on))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(provide 'my-shell)
