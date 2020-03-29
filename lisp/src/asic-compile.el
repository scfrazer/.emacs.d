;;; asic-compile.el -*- lexical-binding: t; -*-

(require 'compile)
(require 'ido)

(defvar asic-compile-error-regexp-alist
  (list
   '("^Error-\\[.+?\\].+\n\\(.+\n\\)*?\\s-*\"?\\([^,\"]+\\)\"?,[ \t\n]+\\([0-9]+\\)" 2 3) ;; VCS
   '("^[*][*][*] LINT.+at line: \\([0-9]+\\) in file: \\(.+\\)\\s-*$" 2 1) ;; SV Lint
   '("^[*][*][*] SEMANTIC ERROR.+at line: \\([0-9]+\\) in file: \\(.+\\)\\s-*$" 2 1) ;; SV Lint
   ))

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
  (list "$WORKSPACE/lawson/test/bin/run_build"
        "$WORKSPACE/lawson/test/bin/run_dv_lint"))

(defun asic-compile ()
  "ASIC compile."
  (interactive)
  (setq asic-compile-command
        (ido-completing-read "ASIC compile command: " asic-compile-command-list nil nil nil compile-history))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or (getenv "WORKSPACE") default-directory)))
    (setq asic-compile-command-list (delq asic-compile-command asic-compile-command-list))
    (add-to-list 'asic-compile-command-list asic-compile-command)
    (compilation-start asic-compile-command 'asic-compile-mode)))

(provide 'asic-compile)
