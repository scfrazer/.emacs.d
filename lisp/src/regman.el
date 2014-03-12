;;; regman.el

;; /vob/sse/asic/shared/ver/build/mmap/chip/regman -f regwt csr_access_details
;; regawt
;; regrwt
;; regwt

(require 'ido)

(defvar regman-program
  "/vob/sse/asic/shared/ver/build/mmap/chip/regman"
  "Where regman is.")

(defvar regman-mode-hook nil
  "Hook run after regman-mode is loaded.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar regman-buffer-name "*regman*"
  "Buffer name.")

(defvar regman-reg-history nil
  "Register history.")

(defun regman (&optional multiple)
  "Describe a register."
  (interactive "P")
  (let ((reg (regman-get-reg multiple)) resize-mini-windows)
    (unless (string= reg "")
      (get-buffer-create regman-buffer-name)
      (with-current-buffer regman-buffer-name
        (setq buffer-read-only nil)
        (erase-buffer)
        (shell-command (concat regman-program " -f default '" reg "'") regman-buffer-name)
        (set-buffer-modified-p nil)
        (regman-mode))
      (pop-to-buffer regman-buffer-name))))

(defun regman-get-reg (multiple)
  "Get a register."
  (let (reg)
    (save-excursion
      (skip-syntax-backward "w_")
      (let ((beg (point)) guess)
        (skip-syntax-forward "w_")
        (setq guess (buffer-substring-no-properties beg (point)))
        (if (string= guess "")
            (setq reg (read-from-minibuffer "Register: " nil nil nil 'regman-reg-history))
          (setq reg (read-from-minibuffer
                     (format "Register (default %s): " guess) nil nil nil 'regman-reg-history)))
        (when (string= reg "")
          (setq reg guess))))
    (unless (or (string= reg "") multiple)
      (let ((regs (split-string (shell-command-to-string (concat regman-program " -f simple '" reg "'")))))
        (setq reg
              (if (= (length regs) 1)
                  (car regs)
                (ido-completing-read "Register: " regs nil t)))))
    reg))

(defun regman-quit ()
  "Quit regman buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar regman-instance-regexp
  "^\\(chip[.]\\S-+\\)"
  "regman instance delimiter.")

(defvar regman-section-regexp
  (concat "^" (regexp-opt (list "Register name:" "Register type:" "Description:" "Access properties:" "Fields:")))
  "regman section delimiters.")

(defvar regman-mode-font-lock-keywords
  `(
    (,regman-instance-regexp
     (1 'font-lock-function-name-face))
    (,regman-section-regexp
     (0 'font-lock-keyword-face))
    ("^\\s-+Field\\s-+[0-9]+\\s-+\\(pad_[0-9_]+\\)\\(\\s-\\|(\\)"
     (1 'font-lock-comment-face))
    ("^\\s-+Field\\s-+[0-9]+\\s-+\\([a-zA-Z0-9_]+\\)"
     (1 'font-lock-variable-name-face))
    ("^[*]+"
     (0 'font-lock-string-face))
    )
  "Font locking for `regman-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar regman-mode-map nil
  "`regman-mode' keymap.")

(if (not regman-mode-map)
    (let ((map (make-keymap)))
      (define-key map "q" 'regman-quit)
      (setq regman-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun regman-mode ()
  "regman-mode is a major mode for browsing regman output.\n\n
\\{regman-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'regman-mode)
  (setq mode-name "regman")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map regman-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(regman-mode-font-lock-keywords t))
  (turn-on-font-lock)
  (setq truncate-lines nil)
  (setq word-wrap t)
  (regman-mode-tidy-buffer)
  (set-buffer-modified-p nil)
  (run-mode-hooks 'regman-mode-hook))

(defun regman-mode-tidy-buffer ()
  "Tidy the buffer up."
  ;; Separate multiple instances
  (goto-char (point-min))
  (forward-line)
  (while (re-search-forward regman-instance-regexp nil t)
    (beginning-of-line)
    (insert "********************************************************************************\n\n")
    (forward-line))
  ;; Offset sections from one another
  (goto-char (point-min))
  (while (re-search-forward regman-section-regexp nil t)
    (beginning-of-line)
    (insert "\n")
    (forward-line))
  ;; Font-lock and reformat field descriptions
  (goto-char (point-min))
  (while (re-search-forward "^Fields:" nil t)
    (forward-paragraph)
    (skip-syntax-forward " ")
    (beginning-of-line)
    (while (and (not (eobp)) (looking-at "[a-zA-Z0-9_]+"))
      (add-text-properties (match-beginning 0) (match-end 0) '(font-lock-face font-lock-variable-name-face))
      (goto-char (match-end 0))
      (delete-horizontal-space)
      (insert "\n")
      (fill-region (point) (progn (forward-line) (point)))
      (insert "\n")))
  ;; Clean up excess whitespace
  (delete-trailing-whitespace)
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (backward-char)
    (delete-blank-lines))
  (goto-char (point-max))
  (delete-blank-lines))

(provide 'regman)
