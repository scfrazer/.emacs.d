;;; my-speedbar.el

(require 'sb-imenu)
(setq-default speedbar-default-position 'left
              speedbar-indentation-width 2
              speedbar-initial-expansion-list-name "sb-imenu"
              speedbar-use-images nil
              sr-speedbar-right-side nil)
(speedbar-add-supported-extension ".v")
(speedbar-add-supported-extension ".sv")
(speedbar-add-supported-extension ".svh")
(speedbar-add-supported-extension ".aop")

;; (define-key speedbar-mode-map (kbd "TAB") 'my-speedbar-toggle-line-expansion)
;; 
;; (defun my-speedbar-toggle-line-expansion ()
;;   "Toggle line expansion and stay in place."
;;   (interactive)
;;   (save-excursion
;;     (speedbar-toggle-line-expansion)))
;; 
;; ;; prettify-symbols-mode doesn't work in speedbar, so compose characters ourself
;; (defun speedbar-insert-image-button-maybe (start length)
;;   "Insert an image button based on text starting at START for LENGTH chars.
;; If buttontext is unknown, just insert that text.
;; If we have an image associated with it, use that image."
;;   (if speedbar-use-images
;;       (let ((ezimage-expand-image-button-alist
;;              speedbar-expand-image-button-alist))
;;         (ezimage-insert-image-button-maybe start length))
;;     (when (equal major-mode 'speedbar-mode)
;;       (save-excursion
;;         (goto-char start)
;;         (cond ((looking-at " =>")
;;                (compose-region start (+ start length) ?•))
;;               ((looking-at ".\\+.")
;;                (compose-region start (+ start length) ?►))
;;               ((looking-at ".-.")
;;                (compose-region start (+ start length) ?▼)))))))

(provide 'my-speedbar)
