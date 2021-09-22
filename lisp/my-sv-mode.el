;;; my-sv-mode.el

(require 'sv-mode)
(require 'doxymacs)
(require 'quick-edit)
(require 'my-complete)

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
                       (cons "reset_phase" 'task)
                       (cons "pre_configure_phase" 'task)
                       (cons "configure_phase" 'task)
                       (cons "post_configure_phase" 'task)
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
         (phase (completing-read "Insert phase: " (mapcar #'car phases) nil t))
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

(defun my-sv-breakpoint (&optional arg)
  "Create a VCS breakpoint string.
With prefix argument, add a condition."
  (interactive "P")
  (let* ((condition (and arg (read-string "Breakpoint condition? ")))
         (breakpoint (concat "stop -file {" (buffer-file-name) "} -line {" (number-to-string (line-number-at-pos)) "}"
                             (if arg (concat " -condition {" condition "}") ""))))
    (message breakpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-sv-mode-prettify ()
;;   (let ((spec '(("<=" default "<═"))))
;;     (dolist (s spec)
;;       (let ((regexp (nth 0 s))
;;             (face (nth 1 s))
;;             (repl (nth 2 s)))
;;         (font-lock-add-keywords nil
;;                                 `((,regexp 0
;;                                            (prog1 ',face
;;                                              (add-text-properties (match-beginning 0) (match-end 0)
;;                                                                   '(display ,repl)))))))))
;;   (push 'display font-lock-extra-managed-props)
;;   (push 'composition font-lock-extra-managed-props))

(defun my-sv-mode-hook ()
  (doxymacs-mode 1)
  (define-key sv-mode-map (kbd "<f9>") 'my-sv-breakpoint)
  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv")))))

(add-hook 'sv-mode-hook 'my-sv-mode-hook)

(defadvice qe-forward-block (around sv-forward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-end-of-block)
    ad-do-it))

(defadvice qe-backward-block (around sv-backward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-beginning-of-block)
    ad-do-it))

(defadvice expand-abbrev (around my-sv-expand-abbrev-advice activate)
  (if (and (equal major-mode 'sv-mode)
           (looking-back "[0-9]") (point-at-bol))
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
  "lint"
  ""
  (lambda()
    (if (looking-back "^[[:space:]]*" (point-at-bol))
        (let (pos)
          (indent-according-to-mode)
          (insert "// @DVT_LINTER_WAIVER_START \"")
          (setq pos (point))
          (insert "\" DISABLE foo\n")
          (indent-according-to-mode)
          (insert "// @DVT_LINTER_WAIVER_END \"\"\n")
          (goto-char pos))
      (insert "// @DVT_LINTER_WAIVER \"\" DISABLE foo")
      (backward-char 13))))

(define-abbrev sv-mode-abbrev-table
  "uc"
  ""
  'my-sv-uvm-component)

(define-skeleton my-sv-uvm-component
  "Insert UVM component"
  nil
  > _ "class "
  (setq v1 (skeleton-read "Name: " (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
  (when (setq v2 (y-or-n-p "Parameterized?")) "#(TODO PARAMS = TODO)")
  " extends "
  (skeleton-read "Extends: " "uvm_component") ";\n"
  \n
  (if v2 "`uvm_component_param_utils_begin(" "`uvm_component_utils_begin(") v1 (when v2 "#(PARAMS)") ")" \n
  "`uvm_component_utils_end" > "\n"
  \n
  "//! Constructor" \n
  "//! @param name - Name" \n
  "//! @param parent - Parent" \n
  "extern function new(string name=\"" v1 "\", uvm_component parent);\n"
  \n
  "//! Build phase" \n
  "//! @param phase - Phase" \n
  "extern virtual function void build_phase(uvm_phase phase);\n"
  \n
  "//! Connect phase" \n
  "//! @param phase - Phase" \n
  "extern virtual function void connect_phase(uvm_phase phase);\n"
  \n
  "endclass : " v1 > "\n"
  \n
  "function " v1 "::new(string name=\"" v1 "\", uvm_component parent);" \n
  "super.new(name, parent);" \n
  "endfunction : new" > "\n"
  \n
  "function void " v1 "::build_phase(uvm_phase phase);" \n
  "super.build_phase(phase);" \n
  "endfunction : build_phase" > "\n"
  \n
  "function void " v1 "::connect_phase(uvm_phase phase);" \n
  "super.connect_phase(phase);" \n
  "endfunction : connect_phase" > "\n"
  )

(provide 'my-sv-mode)
