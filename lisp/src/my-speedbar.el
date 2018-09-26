;;; my-speedbar.el

(require 'sb-imenu)
(setq-default speedbar-default-position 'left
              speedbar-indentation-width 2
              speedbar-initial-expansion-list-name "sb-imenu"
              speedbar-use-images nil
              sr-speedbar-default-width 50
              sr-speedbar-right-side nil)

(speedbar-add-supported-extension ".v")
(speedbar-add-supported-extension ".sv")
(speedbar-add-supported-extension ".svh")
(speedbar-add-supported-extension ".aop")

(advice-remove 'pop-to-buffer #'ad-Advice-pop-to-buffer)

(defun my-speedbar-reset-point (&optional arg)
  (with-current-buffer speedbar-buffer
    (goto-char (point-at-bol))))

(advice-add #'speedbar-refresh :after #'my-speedbar-reset-point)

(provide 'my-speedbar)
