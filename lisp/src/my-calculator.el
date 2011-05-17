;;; my-calculator.el

(require 'calculator)

(setq-default calculator-prompt "Calc [%s]: "
              calculator-radix-grouping-separator "_"
              calculator-2s-complement t
              ;; TODO calculator-user-operators -- >>, <<, log2, w=copy, y=paste
              ;; TODO calculator-displayers -- separators in decimal mode
              )

(defadvice calculator-get-prompt (after my-calculator-get-prompt activate)
  "Replace '=' with '-'"
  (setq ad-return-value (replace-regexp-in-string "=" "-" ad-return-value)))

(defun my-calculator-mode-hook ()
  (define-key calculator-mode-map " " nil))

(add-hook 'calculator-mode-hook 'my-calculator-mode-hook)

(provide 'my-calculator)
