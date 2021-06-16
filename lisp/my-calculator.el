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

;; Copy defun and remove separators from result and add message

(defun calculator-copy ()
  "Copy current number to the `kill-ring'."
  (interactive)
  (let ((calculator-displayer
         (or calculator-copy-displayer calculator-displayer))
        (calculator-displayers
         (if calculator-copy-displayer nil calculator-displayers)))
    (calculator-enter)
    ;; remove trailing spaces and an index and grouping separator
    (let ((s (cdr calculator-stack-display)))
      (when s
        (kill-new
         (replace-regexp-in-string
          calculator-radix-grouping-separator ""
          (replace-regexp-in-string
           "^\\([^ ]+\\) *\\(\\[[0-9/]+\\]\\)? *$" "\\1" s)))
        (message "Copied value to kill-ring")))))

;; Hook

(defun my-calculator-mode-hook ()
  (set-window-dedicated-p nil t)
  (define-key calculator-mode-map (kbd " ") nil)
  (define-key calculator-mode-map (kbd "_") 'calculator-rotate-displayer)
  (define-key calculator-mode-map (kbd "w") 'calculator-copy)
  (define-key calculator-mode-map (kbd "y") 'calculator-paste)
  (define-key calculator-mode-map (kbd "DEL") 'calculator-backspace))

(add-hook 'calculator-mode-hook 'my-calculator-mode-hook)

(provide 'my-calculator)
