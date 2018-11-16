;;; my-avy.el

(require 'avy)

(setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z))
      avy-all-windows nil
      avy-case-fold-search nil
      avy-style 'at
      avy-timeout-seconds 0.5)

(defun my-avy-goto-line (&optional arg)
  "Jump to start of a line, or with prefix arg end of a line."
  (interactive "P")
  (if (null arg)
      (call-interactively 'avy-goto-line)
    (avy-with avy-goto-char
      (avy--generic-jump "\n" nil avy-style))))

(defun my-avy-goto (char)
  "Jump to CHAR at a word start, or string if C-k, or BOL if C-l, or EOL if C-m."
  (interactive (list (read-char "Char: ")))
  (cond ((= 11 char)
         (let ((avy-timeout-seconds 0))
           (call-interactively 'avy-goto-char-timer)))
        ((= 12 char)
         (call-interactively 'avy-goto-line))
        ((and (not (< 31 char 127))
              (not (= 13 char)))
         (error "Unknown char"))
        (t
         (avy-with avy-goto-word-1
           (let* ((str (string char))
                  (regex (cond ((= 13 char)
                                "\n")
                               ((= 32 char)
                                "\\s-+")
                               ((string= str ".")
                                "\\.")
                               ((and avy-word-punc-regexp
                                     (string-match avy-word-punc-regexp str))
                                (regexp-quote str))
                               ((< 64 char 91)
                                (concat "\\b" str "\\|" str "[a-z0-9]"))
                               (t
                                (concat "\\b" str)))))
             (if (eq major-mode 'php-mode)
                 (let ((table (copy-syntax-table (syntax-table))))
                   (modify-syntax-entry ?$ "." table)
                   (with-syntax-table table
                     (avy--generic-jump regex nil avy-style)))
               (avy--generic-jump regex nil avy-style)))))))

(provide 'my-avy)
