;;; my-calculator.el

(require 'calculator)

(setq-default calculator-prompt "Calc [%s]: "
              calculator-radix-grouping-separator "_"
              calculator-2s-complement t
              calculator-user-operators '(("<" << (lsh TX TY) 2 2)
                                          (">" >> (lsh TX (* -1 TY)) 2 2)) ;; TODO log2
              ;; TODO calculator-displayers -- separators in decimal mode
              )

(defadvice calculator-get-prompt (after my-calculator-get-prompt activate)
  "Replace '=' with '-'"
  (setq ad-return-value (replace-regexp-in-string "=" "-" ad-return-value)))

(defun my-calculator-mode-hook ()
  (define-key calculator-mode-map " " nil)
  (define-key calculator-mode-map "w" 'calculator-copy)
  (define-key calculator-mode-map "y" 'calculator-paste))

(add-hook 'calculator-mode-hook 'my-calculator-mode-hook)

(provide 'my-calculator)
