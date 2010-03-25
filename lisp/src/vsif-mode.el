;;; vsif-mode.el

(defgroup vsif-mode nil
  "*vsif mode."
  :group 'programming)

(defcustom vsif-indent-offset 4
  "*Indentation offset for `vsif-mode'"
  :group 'vsif-mode
  :type 'integer)

(defcustom vsif-mode-hook nil
  "*List of functions to call on entry to `vsif-mode'."
  :group 'vsif-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar vsif-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\@ "." table)
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\. "." table)

    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\'  "\"" table)
    (modify-syntax-entry ?\_  "w" table)

    (modify-syntax-entry ?/  ". 12" table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?\n "> 34" table)
    table)
  "Syntax table for `vsif-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun vsif-indent-line ()
  "Indent current line for `vsif-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "{")
              (setq indent-col (+ indent-col vsif-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col vsif-indent-offset))
        (setq indent-col (- indent-col vsif-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

; (concat "#\\<" (regexp-opt '("include" "if" "ifdef" "ifndef" "else" "elif" "endif" "define" "undef") t) "\\>")
; (concat "\\<" (regexp-opt '("group" "test" "session" "extend") t) "\\>")
; (concat "\\<" (regexp-opt '("default_dispatch_parameters" "drm" "master_submission_policy" "max_runs_in_parallel" "model_dir" "output_mode" "post_session_dispatch_parameters" "pre_session_dispatch_parameters" "pre_session_script" "post_session_script" "top_dir" "verification_scope") t) "\\>")
; (concat "\\<" (regexp-opt '("repetitions" "bundle_group" "code_coverage" "count" "depends_on" "details" "dut_name" "exit_on" "gui_mode" "hdl_files" "ifv_assertions" "ifv_effort" "ifv_engine" "ifv_halo" "model_dir" "pre_commands" "pre_group_script" "pre_run_script" "post_commands" "post_group_script" "post_run_script" "post_simulate_script" "run_mode" "run_script" "runs_dispatch_parameters" "scan_script" "seed" "sv_seed" "sve_name" "test_command" "timeout" "top_files" "verification_scope" "verbosity" "vplan_ref" "waveform") t) "\\>")

(defvar vsif-mode-font-lock-keywords
  '(
    ("#\\<\\(define\\|e\\(?:l\\(?:if\\|se\\)\\|ndif\\)\\|i\\(?:f\\(?:n?def\\)?\\|nclude\\)\\|undef\\)\\>"
     0 font-lock-preprocessor-face)
    ("\\<\\(extend\\|group\\|session\\|test\\)\\>\\s-+\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("\\<\\(d\\(?:efault_dispatch_parameters\\|rm\\)\\|m\\(?:a\\(?:ster_submission_policy\\|x_runs_in_parallel\\)\\|odel_dir\\)\\|output_mode\\|p\\(?:ost_session_\\(?:dispatch_parameters\\|script\\)\\|re_session_\\(?:dispatch_parameters\\|script\\)\\)\\|top_dir\\|verification_scope\\)\\>"
     0 font-lock-type-face)
    ("\\<\\(bundle_group\\|co\\(?:de_coverage\\|unt\\)\\|d\\(?:e\\(?:pends_on\\|tails\\)\\|ut_name\\)\\|exit_on\\|gui_mode\\|hdl_files\\|ifv_\\(?:assertions\\|e\\(?:ffort\\|ngine\\)\\|halo\\)\\|model_dir\\|p\\(?:ost_\\(?:commands\\|\\(?:group\\|run\\|simulate\\)_script\\)\\|re_\\(?:commands\\|\\(?:group\\|run\\)_script\\)\\)\\|r\\(?:epetitions\\|un\\(?:_\\(?:mode\\|script\\)\\|s_dispatch_parameters\\)\\)\\|s\\(?:can_script\\|eed\\|v\\(?:_seed\\|e_name\\)\\)\\|t\\(?:est_command\\|imeout\\|op_files\\)\\|v\\(?:er\\(?:bosity\\|ification_scope\\)\\|plan_ref\\)\\|waveform\\)\\>"
     0 font-lock-builtin-face)
    ("^\\s-*\\([a-zA-Z0-9_]+\\)\\s-*:"
     1 font-lock-variable-name-face)
    ("<.+?>"
     0 font-lock-doc-face)
    ("$[a-zA-Z0-9_]+"
     0 font-lock-constant-face)
    )
  "Keyword highlighting specification for `vsif-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar vsif-mode-map nil "`vsif-mode' keymap.")
(if (not vsif-mode-map)
    (let ((map (make-keymap)))
      (setq vsif-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode startup

(defun vsif-mode ()
  "vsif-mode is a major mode for editing ANTLR 3 files.\n\n
\\{vsif-mode-map}"
  (interactive)

  (kill-all-local-variables)
  (setq major-mode 'vsif-mode)
  (setq mode-name "vsif")

  (use-local-map vsif-mode-map)

  (set-syntax-table vsif-mode-syntax-table)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "//[ \t]*")

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (make-local-variable 'vsif-indent-offset)
  (set (make-local-variable 'indent-line-function) 'vsif-indent-line)

  (set (make-local-variable 'font-lock-defaults) '(vsif-mode-font-lock-keywords))
  (turn-on-font-lock)

  (run-hooks 'vsif-mode-hook))

(provide 'vsif-mode)
