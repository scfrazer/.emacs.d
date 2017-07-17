;;; my-grep-ed.el

(require 'grep-ed)

(defun my-grep-ed-vc-checkout-function (filename)
  "Checkout from version control"
  (cond ((and (string-match "^/vob/" filename)
              (not (file-writable-p filename)))
         (let ((clearcase-checkout-arguments (list "-unreserved" "-nmaster"))
               (clearcase-suppress-checkout-comments t))
           (clearcase-commented-checkout filename)))
        ((and (p4-with-temp-buffer (list "-s" "files" filename) (looking-at "info:"))
              (not (file-writable-p filename)))
         (p4-with-temp-buffer (list "-s" "edit" filename)))))

(setq grep-ed-vc-checkout-function 'my-grep-ed-vc-checkout-function)

(provide 'my-grep-ed)
