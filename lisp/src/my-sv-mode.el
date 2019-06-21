;;; my-sv-mode.el

(require 'sv-mode)
(require 'quick-edit)
(require 'my-verilog-mode)

(defun my-sv-mode-bit-vector ()
  "Expand bit vector."
  (interactive)
  (let (num-bits)
    (skip-chars-backward "0-9")
    (when (looking-at "[0-9]+")
      (setq num-bits (match-string-no-properties 0))
      (delete-char (length num-bits))
      (setq num-bits (string-to-number num-bits))
      (unless (looking-back "[a-zA-Z_]\\s-*" (point-at-bol))
        (insert "bit "))
      (insert "[" (number-to-string (1- num-bits)) ":0] "))))

(defun my-sv-mode-uvm-create ()
  "Expand UVM create var."
  (interactive)
  (let ((pos (point)))
    (when (re-search-backward "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)\\s-*=" (point-at-bol) t)
      (let ((var (match-string 1)))
        (goto-char pos)
        (when (re-search-backward (concat "^\\s-*[^a-zA-Z0-9_:]\\([a-zA-Z0-9_:]+\\)\\s-+" var) nil t)
          (let ((type (match-string 1)))
            (goto-char pos)
            (insert type "::type_id::create(\"" var "\", this);")))))))

(defun my-sv-mode-uvm-new (component)
  "Fill in 'new' function for uvm_(object|component)."
  (let* ((pos (point))
         (class-type (sv-mode-get-class-type))
         (name (nth 0 class-type)))
    (unless current-prefix-arg
      (insert "extern "))
    (insert "function new(string name = \"" name "\"")
    (when component
      (insert ", uvm_component parent"))
    (insert ");")
    (when current-prefix-arg
      (insert "\n")
      (sv-mode-insert-super)
      (insert "\n")
      (sv-mode-insert-end)
      (insert "\n")
      (indent-region pos (point)))
    (unless current-prefix-arg
      (goto-char pos))))

(defun my-sv-mode-uvm-phase ()
  "Insert a UVM phase declaration."
  (interactive "*")
  (let* ((pos (point))
         (phases (list (cons "build_phase" 'function)
                       (cons "connect_phase" 'function)
                       (cons "end_of_elaboration_phase" 'function)
                       (cons "start_of_simulation_phase" 'function)
                       (cons "run_phase" 'task)
                       (cons "pre_reset_phase" 'task)
                       (cons "pre_oir" 'task)
                       (cons "reset_phase" 'task)
                       (cons "reset_pon_active" 'task)
                       (cons "reset_pon_deassert" 'task)
                       (cons "reset_hard_active" 'task)
                       (cons "reset_hard_deassert" 'task)
                       (cons "reset_soft_active" 'task)
                       (cons "reset_soft_deassert" 'task)
                       (cons "reset_prep" 'task)
                       (cons "post_reset_phase" 'task)
                       (cons "pre_configure_phase" 'task)
                       (cons "config_memory" 'task)
                       (cons "config_memory_enable" 'task)
                       (cons "config_memory_bulk_init" 'task)
                       (cons "snapshot_post_init_invariant_save" 'task)
                       (cons "snapshot_save_resume" 'task)
                       (cons "configure_phase" 'task)
                       (cons "config_enable" 'task)
                       (cons "post_configure_phase" 'task)
                       (cons "snapshot_post_init_done_save" 'task)
                       (cons "pre_main_phase" 'task)
                       (cons "main_phase" 'task)
                       (cons "post_main_phase" 'task)
                       (cons "pre_shutdown_phase" 'task)
                       (cons "shutdown_phase" 'task)
                       (cons "post_shutdown_phase" 'task)
                       (cons "extract_phase" 'function)
                       (cons "check_phase" 'function)
                       (cons "report_phase" 'function)
                       (cons "final_phase" 'function)
                       (cons "phase_started" 'function)
                       (cons "phase_ready_to_end" 'function)
                       (cons "phase_ended" 'function)))
         (phase (ido-completing-read "Insert phase: " (mapcar #'car phases) nil t))
         (kind (cdr (assoc phase phases))))
    (when (and phase kind)
      (unless current-prefix-arg
        (insert "extern "))
      (insert "virtual ")
      (if (equal kind 'function)
          (insert "function void ")
        (insert "task "))
      (insert phase "(uvm_phase phase);")
      (when current-prefix-arg
        (insert "\n")
        (sv-mode-insert-super)
        (insert "\n")
        (sv-mode-insert-end)
        (insert "\n"))
      (indent-region pos (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-xclip)

(defun my-sv-breakpoint (&optional arg)
  "Create a VCS breakpoint string and copy to the clipboard.
With prefix argument, add a condition."
  (interactive "P")
  (let* ((condition (and arg (read-string "Breakpoint condition? ")))
         (breakpoint (concat "stop -file {" (buffer-file-name) "} -line {" (number-to-string (line-number-at-pos)) "}"
                             (if arg (concat " -condition {" condition "}") ""))))
    (my-xclip-copy-text breakpoint)
    (message breakpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (rule (reverse align-verilog-rules-list))
  (add-to-list 'sv-mode-align-rules-list rule))
(dolist (rule (reverse align-exclude-verilog-rules-list))
  (add-to-list 'sv-mode-align-exclude-rules-list rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-sv-mode-hook ()
  (font-lock-add-keywords nil '(("\\_<\\(bool\\|uint\\)\\_>" (0 'font-lock-type-face))) 'add-to-end)
  (define-key sv-mode-map (kbd "C-c C-e") 'my-verilog-mode-auto-inst)
  (define-key sv-mode-map (kbd "<f10>") 'my-sv-breakpoint)
  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv"))
                              ("\\.s$" (".v"))
                              ("\\.v$" (".s" ".vh")))))

(add-hook 'sv-mode-hook 'my-sv-mode-hook)

(add-to-list 'sv-mode-macros-without-semi "`csco_[a-z_]+")

(defadvice sv-mode-create-skeleton-from-prototype (before ct-co-sv-mode-create-proto activate)
  (when (ff-other-file-name)
    (ff-get-other-file))
  (let ((filename (buffer-file-name)))
    (when (and (string-match "^/vob/" filename)
               (not (file-writable-p filename)))
      (let ((clearcase-checkout-arguments (list "-unreserved" "-nmaster"))
            (clearcase-suppress-checkout-comments t))
        (clearcase-commented-checkout filename)))
    (when (ff-other-file-name)
      (switch-to-buffer (other-buffer)))))

(defadvice qe-forward-block (around sv-forward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-end-of-block)
    ad-do-it))

(defadvice qe-backward-block (around sv-backward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-beginning-of-block)
    ad-do-it))

(defun my-sv-mode-uvm-info (verbosity)
  (interactive "sVerbosity? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_info(\"" sv-mode-uvm-tag ":\", \"TODO\", " verbosity ");")
  (sv-mode-indent-line)
  (search-backward ":")
  (forward-char 1))
(setq sv-mode-uvm-info-function 'my-sv-mode-uvm-info)

(defun my-sv-mode-uvm-err (type)
  (interactive "sType? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_" type "(\"" sv-mode-uvm-tag ":\", \"TODO\");")
  (sv-mode-indent-line)
  (search-backward ":")
  (forward-char 1))
(setq sv-mode-uvm-err-function 'my-sv-mode-uvm-err)

(defadvice expand-abbrev (around my-sv-expand-abbrev-advice activate)
  (if (and (equal major-mode 'sv-mode)
           (looking-back "[0-9]"))
      (my-sv-mode-bit-vector)
    ad-do-it))

(define-abbrev sv-mode-abbrev-table
  "sfor"
  ""
  (lambda ()
    (insert "$sformatf(\"\", )")
    (backward-char 4)))

(define-abbrev sv-mode-abbrev-table
  "for"
  "for"
  (lambda()
    (let ((var (read-from-minibuffer "Loop variable? " "idx"))
          (limit (read-from-minibuffer "Limit? ")))
      (insert "(int " var " = 0; " var " < " limit "; " var "++) begin\n")
      (sv-mode-indent-line)
      (save-excursion
        (insert "\nend")
        (sv-mode-indent-line)))))

(define-abbrev sv-mode-abbrev-table
  "forenum"
  "for"
  (lambda()
    (let ((type (read-from-minibuffer "Enum type? "))
          (var (read-from-minibuffer "Enum loop variable name? ")))
      (insert "(int idx=0, " type " " var "=" var ".first(); idx < " var ".num(); idx++, " var "=" var ".next()) begin\n")
      (sv-mode-indent-line)
      (save-excursion
        (insert "\nend")
        (sv-mode-indent-line)))))

(define-abbrev sv-mode-abbrev-table
  "create"
  ""
  'my-sv-mode-uvm-create)

(define-abbrev sv-mode-abbrev-table
  "uon"
  ""
  (lambda () (my-sv-mode-uvm-new nil)))

(define-abbrev sv-mode-abbrev-table
  "ucn"
  ""
  (lambda () (my-sv-mode-uvm-new t)))

(define-abbrev sv-mode-abbrev-table
  "up"
  ""
  (lambda () (my-sv-mode-uvm-phase)))

(define-abbrev sv-mode-abbrev-table
  "uto"
  "set_type_override_by_type(TODO_orig::get_type(), TODO_over::get_type());"
  (lambda () (back-to-indentation) (search-forward "(")))

(define-abbrev sv-mode-abbrev-table
  "uio"
  "set_inst_override_by_type(\"TODO.path\", TODO_orig::get_type(), TODO_over::get_type());"
  (lambda () (back-to-indentation) (search-forward "\"")))

(define-abbrev sv-mode-abbrev-table
  "phr"
  ""
  (lambda()
    (insert "phase.raise_objection(this);")
    (sv-mode-indent-line)))

(define-abbrev sv-mode-abbrev-table
  "phd"
  ""
  (lambda()
    (insert "phase.drop_objection(this);")
    (sv-mode-indent-line)))

(provide 'my-sv-mode)
