;;; vcs-compile.el

(require 'compile)

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

(defvar vcs-compile-command "find_fail_log -c"
  "*Default VCS compile command")

(defun vcs-compile (command &optional comint)
  "VCS compile."
  (interactive
   (list
    (let ((command (eval vcs-compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (vcs-compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval vcs-compile-command))
    (setq vcs-compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command 'vcs-compile-mode))

(provide 'vcs-compile)
