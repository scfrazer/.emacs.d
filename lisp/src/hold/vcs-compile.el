;;; vcs-compile.el

(require 'compile)
(require 'ido)

(defun vcs-compile-find-file (filename)
  "Unmangle filename if necessary."
  (save-match-data
    (cond ((string-match ".+_run\\(sc\\)?_\\([^-]+\\)-\\(aop\\|sv\\)=\\([^/-]+\\)" filename)
           (let ((block-name (match-string 2 filename))
                 (type (match-string 3 filename))
                 (path (match-string 4 filename)))
             (concat "/vob/sse/asic/shared/ver/tb/" block-name "/test/" (replace-regexp-in-string ":" "/" path) "." type)))
          (t
           filename))))

(define-compilation-mode vcs-compile-mode "vcs-compile"
  "VCS compilation mode."
  (set (make-local-variable 'compilation-error-regexp-alist)
;;        (list '("^Error-\\[.+?\\].+\n\\s-*\\(.+\\),[ \t\n]+\\([0-9]+\\)" 1 2)
;;              '("^Error-\\[.+?\\].+\n\\(.+\n\\)+\\s-*\"\\(.+\\)\",[ \t\n]+\\([0-9]+\\)" 2 3)))
       (list '("^Error-\\[.+?\\].+\n\\(.+\n\\)*?\\s-*\"?\\([^,\"]+\\)\"?,[ \t\n]+\\([0-9]+\\)" 2 3)
             '("^Error - .+?spec:\\([^:]+\\):" 1)))
  (set (make-local-variable 'compilation-parse-errors-filename-function) 'vcs-compile-find-file))

(defun vcs-compilation-read-command (command)
  (read-shell-command "VCS compile command: " command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)))

(defvar vcs-compile-command "find_fail_log -c ."
  "*Default VCS compile command")

(defvar vcs-compile-command-list (list vcs-compile-command))

(defun vcs-compile (&optional arg)
  "VCS compile.  With prefix arg, don't set run directory to $RESULTSDIR."
  (interactive "P")
  (setq vcs-compile-command
        (vcs-compilation-read-command
         (ido-completing-read "VCS compile command: " vcs-compile-command-list)))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or (and (not arg)
                                    (getenv "RESULTSDIR")) default-directory)))
    (setq vcs-compile-command-list (delq vcs-compile-command vcs-compile-command-list))
    (add-to-list 'vcs-compile-command-list vcs-compile-command)
    (compilation-start vcs-compile-command 'vcs-compile-mode)))

(provide 'vcs-compile)
