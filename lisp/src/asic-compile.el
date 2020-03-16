;;; asic-compile.el -*- lexical-binding: t; -*-

(require 'compile)
(require 'ido)

(define-compilation-mode asic-compile-mode "asic-compile"
  "ASIC compilation mode."
  (set (make-local-variable 'compilation-error-regexp-alist)
       (list
        '("^Error-\\[.+?\\].+\n\\(.+\n\\)*?\\s-*\"?\\([^,\"]+\\)\"?,[ \t\n]+\\([0-9]+\\)" 2 3)  ;; VCS
        '("^[*][*][*] LINT.+at line: \\([0-9]+\\) in file: \\(.+\\)\\s-*$" 2 1))))  ;; SV Lint

(defun asic-compilation-read-command (command)
  (read-shell-command "ASIC compile command: " command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)))

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
