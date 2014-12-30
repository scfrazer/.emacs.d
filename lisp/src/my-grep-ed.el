;;; my-grep-ed.el

(defvar use-clearcase)

(use-package
  grep-ed
  :commands grep-ed-start
  :config
  (when use-clearcase
    (defun my-grep-ed-vc-checkout-function (filename)
      "Checkout from ClearCase"
      (when (and (string-match "^/vob/" filename)
                 (not (file-writable-p filename)))
        (let ((clearcase-checkout-arguments (list "-unreserved" "-nmaster"))
              (clearcase-suppress-checkout-comments t))
          (clearcase-commented-checkout filename))))
    (setq grep-ed-vc-checkout-function 'my-grep-ed-vc-checkout-function)))

(provide 'my-grep-ed)
