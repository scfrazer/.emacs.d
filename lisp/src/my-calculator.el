;;; my-calculator.el

(require 'calculator)

(setq-default calculator-prompt "Calc [%s]: "
              calculator-paste-decimals nil
              calculator-radix-grouping-separator "_"
              calculator-2s-complement t
              calculator-user-operators '(("<" << (lsh TX TY) 2 2)
                                          (">" >> (lsh TX (* -1 TY)) 2 2)))

;; Show commas in decimal mode

(defun my-calculator-displayer (num)
  (let ((std-result (calculator-standard-displayer num ?n)))
    std-result))

;; TODO calculator-displayers -- separators in decimal mode

(defadvice calculator-get-prompt (after my-calculator-get-prompt activate)
  "Replace '=' with '-'"
  (setq ad-return-value (replace-regexp-in-string "=" "-" ad-return-value)))

(defadvice calculator-copy (after my-calculator-copy activate)
  (message "Copied value to kill-ring"))

;; Copy defun and remove annoying warning

(defun calculator-string-to-number (str)
  "Convert the given STR to a number, according to the value of
`calculator-input-radix'."
  (if calculator-input-radix
      (let ((radix
             (cdr (assq calculator-input-radix
                        '((bin . 2) (oct . 8) (hex . 16)))))
            (i -1) (value 0) (new-value 0))
        ;; assume mostly valid input (e.g., characters in range)
        (while (< (setq i (1+ i)) (length str))
          (setq new-value
                (let* ((ch (upcase (aref str i)))
                       (n (cond ((< ch ?0)  nil)
                                ((<= ch ?9) (- ch ?0))
                                ((< ch ?A)  nil)
                                ((<= ch ?Z) (- ch (- ?A 10)))
                                (t          nil))))
                  (if (and n (<= 0 n) (< n radix))
                      (+ n (* radix value))
                      value)))
          (if (if (< new-value 0) (> value 0) (< value 0))
              (calculator-message "Warning: Overflow in input."))
          (setq value new-value))
        value)
    (car (read-from-string
          (cond ((equal "." str) "0.0")
                ((string-match "[eE][+-]?$" str) (concat str "0"))
                ((string-match "\\.[0-9]\\|[eE]" str) str)
                ((string-match "\\." str)
                 ;; do this because Emacs reads "23." as an integer
                 (concat str "0"))
                ((stringp str) (concat str ".0"))
                (t "0.0"))))))

(defun my-calculator-mode-hook ()
  (set-window-dedicated-p nil t)
  (define-key calculator-mode-map " " nil)
  (define-key calculator-mode-map "w" 'calculator-copy)
  (define-key calculator-mode-map "y" 'calculator-paste))

(add-hook 'calculator-mode-hook 'my-calculator-mode-hook)

(provide 'my-calculator)
