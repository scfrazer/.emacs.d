;;; asic-compile.el -*- lexical-binding: t; -*-

(require 'compile)

(defvar asic-compile-error-regexp-alist
  (list
   '("^Error-\\[.+?\\].+\n\\(.+\n\\)*?\\s-*\"?\\([^,\"]+\\)\"?,[ \t\n]+\\([0-9]+\\)" 2 3) ;; VCS
   `("^[*][*][*] LINT.+at line: \\([0-9,]+\\) in file: \\(.+\\)\\s-*$" 2 ,'asic-compile-get-line-from-match-data ) ;; SV Lint
   `("^[*][*][*] \\(?:SEMANTIC\\|COMPILE\\) ERROR.+at line: \\([0-9,]+\\) in file: \\(.+\\)\\s-*$" 2 ,'asic-compile-get-line-from-match-data) ;; SV Lint
   '("^\\([^:]+\\):\\([0-9]+\\): warning: .+$" 1 2) ;; Doxygen
   ))

(defun asic-compile-get-line-from-match-data()
  "Get the line number from match-data.
Need to do this because SV lint puts commas in the line number."
  (save-match-data
    (string-to-number (replace-regexp-in-string "," "" (match-string 1)))))

(defvar asic-compile-error-regexp-alist-alist nil)

(defvar asic-compile-mode-font-lock-keywords
  (list
   `(,(concat "\\_<\\(" (regexp-opt '("warning"  "Warning" "WARNING")) "\\)\\_>")
     (1 'warning))
   `(,(concat "\\_<\\(" (regexp-opt '("error"  "Error" "ERROR")) "\\)\\_>")
     (1 'error))
   `(,(concat "\\_<\\(" (regexp-opt '("pass" "Pass" "PASS" "passed" "Passed" "PASSED")) "\\)\\_>")
     (1 'success))
   `(,(concat "\\_<\\(" (regexp-opt '("fail" "Fail" "FAIL" "failed" "Failed" "FAILED")) "\\)\\_>")
     (1 'error))
   ))

(define-compilation-mode asic-compile-mode "asic-compile"
  "ASIC compilation mode.")

(defvar asic-compile-command nil)
(defvar asic-compile-command-list
  (list "pb fe build --tb lawson::lawson_pam_msg_tb --clean"
        "pb fe build --tb lawson::lawson_pam_prot_tb --clean"
        "pb fe build --tb lawson::lawson_pam_coh_tb --clean"
        "pb fe lint dv --tb lawson::lawson_dv_lint"))

(defun asic-compile ()
  "ASIC compile."
  (interactive)
  (setq asic-compile-command
        (completing-read "ASIC compile command: " asic-compile-command-list nil nil nil compile-history))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or (getenv "WORKSPACE") default-directory)))
    (setq asic-compile-command-list (delq asic-compile-command asic-compile-command-list))
    (add-to-list 'asic-compile-command-list asic-compile-command)
    (compilation-start asic-compile-command 'asic-compile-mode)))

(provide 'asic-compile)
