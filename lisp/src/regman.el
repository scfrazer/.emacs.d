;;; regman.el

;; /vob/sse/asic/shared/ver/build/mmap/chip/regman -f regwt csr_access_details
;; regawt
;; regrwt
;; regwt

(require 'ido)

(defgroup regman nil
  "regman."
  :group 'tools)

(defcustom regman-tb "chip"
  "*Testbench to use for regman."
  :group 'regman
  :type 'string)

(defcustom regman-mode-hook nil
  "*List of functions to call on entry to regman-mode."
  :group 'regman
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar regman-base-path (concat (getenv "PROJ") "/asic/shared/ver/build/mmap")
  "Base regman path.")

(defvar regman-program nil
  "Calculated regman program path.")

(defvar regman-buffer-name "*regman*"
  "Buffer name.")

(defvar regman-reg-history nil
  "Register history.")

;;;###autoload
(defun regman (&optional multiple)
  "Describe a register."
  (interactive "P")
  (let ((reg (regman-get-reg multiple)) resize-mini-windows)
    (unless (string= reg "")
      (get-buffer-create regman-buffer-name)
      (with-current-buffer regman-buffer-name
        (setq buffer-read-only nil)
        (erase-buffer)
        (call-process regman-program nil t nil "-f" "default" (concat "^" reg "$"))
        (set-buffer-modified-p nil)
        (regman-mode))
      (pop-to-buffer regman-buffer-name)
      (goto-char (point-min)))))

;;;###autoload
(defun regman-insert-register ()
  "Insert a register access."
  (interactive "*")
  (let ((reg (regman-get-reg nil))
        (pos (point)))
    (call-process regman-program nil t nil "-f" "regwt" (concat "^" reg "$"))
    (indent-region pos (point))))

;;;###autoload
(defun regman-insert-register-with-reset ()
  "Insert a register access with reset."
  (interactive "*")
  (let ((reg (regman-get-reg nil))
        (pos (point)))
    (call-process regman-program nil t nil "-f" "regrwt" (concat "^" reg "$"))
    (indent-region pos (point))))

;;;###autoload
(defun regman-insert-register-group ()
  "Insert a grouped register access."
  (interactive "*")
  (let ((reg (regman-get-reg nil))
        (pos (point)))
    (call-process regman-program nil t nil "-f" "regawt" (concat "^" reg "$"))
    (indent-region pos (point))))

(defun regman-get-reg (multiple)
  "Get a register."
  (let (reg)
    (save-excursion
      (skip-syntax-backward "w_")
      (let ((beg (point)) guess)
        (skip-syntax-forward "w_")
        (setq guess (replace-regexp-in-string "\\`m_" "" (buffer-substring-no-properties beg (point))))
        (if (string= guess "")
            (setq reg (read-from-minibuffer "Register: " nil nil nil 'regman-reg-history))
          (setq reg (read-from-minibuffer
                     (format "Register (default %s): " guess) nil nil nil 'regman-reg-history)))
        (when (string= reg "")
          (setq reg guess))))
    (regman-program-deduce)
    (unless (or (string= reg "") multiple)
      (let ((regs (split-string (shell-command-to-string (concat regman-program " -f simple '" reg "'")))))
        (setq reg
              (if (= (length regs) 1)
                  (car regs)
                (ido-completing-read "Register: " regs nil t)))))
    reg))

(defun regman-program-deduce ()
  "Best effort at deducing `regman-program'."
  (let ((buf-name (buffer-file-name))
        (base-path regman-base-path)
        program)
;; TODO Put this back in if regman stops using absolute paths to find libs
;;     (when (and buf-name (string-match "\\(/view/[^/]+\\)/" buf-name))
;;       (setq base-path (concat (match-string 1 buf-name) base-path)))
    (setq program (concat base-path "/" regman-tb "/regman"))
    (if (file-exists-p program)
        (setq regman-program program)
      (let (tbs)
        (dolist (name (directory-files base-path))
          (when (and (file-directory-p (concat base-path "/" name))
                     (file-exists-p (concat base-path "/" name "/regman")))
            (push name tbs)))
        (cond ((= (length tbs) 1)
               (setq regman-program (concat base-path "/" (car tbs) "/regman")))
              ((> (length tbs) 1)
               (setq regman-program (concat base-path "/" (ido-completing-read "regman TB: " tbs nil t) "/regman")))
              (t
               (error "Couldn't find any regman executable.")))))))

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
    ("^\\s-+Field\\s-+[0-9]+\\s-+\\(pad_[0-9_]+\\)"
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
  (setq truncate-lines t)
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
  ;; Reformat descriptions
  (goto-char (point-min))
  (while (re-search-forward "^Description:" nil t)
    (forward-line)
    (delete-horizontal-space)
    (fill-region (point) (progn (forward-line) (point))))
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
