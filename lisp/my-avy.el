;;; my-avy.el

(require 'avy)

(setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z))
      avy-all-windows nil
      avy-case-fold-search nil
      avy-style 'at
      avy-timeout-seconds 1.0)

(defun my-avy-goto-line (&optional arg)
  "Jump to start of a line, or with prefix arg end of a line."
  (interactive "P")
  (if (null arg)
      (call-interactively 'avy-goto-line)
    (avy-with avy-goto-char
      (avy-jump "\n"))))

(defun my-avy-goto-char-timer (char)
  "Like `avy-goto-char-timer', but:
* Jump to line if C-l
* Kill to line if C-k
* Copy to line if C-o"
  (interactive (list (read-char "Char: ")))
  (cond ((= char ?\C-l)
         (call-interactively 'avy-goto-line))
        ((= char ?\C-k)
         (let ((beg (point)))
           (call-interactively 'avy-goto-line)
           (kill-region beg (point))))
        ((= char ?\C-o)
         (let ((beg (point)) end ov)
           (save-excursion
             (call-interactively 'avy-goto-line)
             (setq end (point)))
           (copy-region-as-kill beg end)
           (setq ov (make-overlay beg end))
           (overlay-put ov 'face 'region)
           (sit-for 0.5)
           (delete-overlay ov)))
        (t
         (setq unread-command-events (listify-key-sequence (char-to-string char)))
         (avy-with avy-goto-char-timer
           (setq avy--old-cands (avy--read-candidates))
           (avy-process avy--old-cands)))))

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
                     (avy-jump regex)))
               (avy-jump regex)))))))

(provide 'my-avy)
