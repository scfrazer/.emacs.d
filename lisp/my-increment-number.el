;; Convert numbers

(defun my-dec-to-hex (&optional arg)
  "Print the decimal number under point as hex."
  (interactive "P")
  (if arg
      (setq arg (read-from-minibuffer "Decimal number? "))
    (save-excursion
      (save-match-data
        (skip-chars-backward "0-9")
        (when (re-search-forward "[0-9]+" nil t)
          (setq arg (match-string 0))))))
  (message (format "0x%X" (string-to-number arg 10))))

(defun my-hex-to-dec (&optional arg)
  "Print the hex number under point as decimal."
  (interactive "P")
  (if arg
      (setq arg (replace-regexp-in-string "^[0xX]+" "" (read-from-minibuffer "Hexadecimal number? ")))
    (save-excursion
      (save-match-data
        (skip-chars-backward "0-9a-fA-F")
        (skip-chars-forward "0xX")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq arg (match-string 0))))))
  (message (format "%d" (string-to-number arg 16))))

;; Increment decimal number

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0-9")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

;; Increment hexadecimal number

(defun my-increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0-9a-fA-F")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))

;; Increment binary number

(defun my-format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun my-increment-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (my-format-bin answer field-width)))))))

(provide 'my-increment-number)
