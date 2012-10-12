;;; vcs-compile.el

(require 'compile)

(defun vcs-compile-find-file (filename)
  "Unmangle filename if necessary."
  (save-match-data
    (cond ((string-match ".+_run\\(sc\\)?_\\([^-]+\\)-\\(aop\\|sv\\)=\\([^/-]+\\)" filename)
           (let ((block-name (match-string 2 filename))
                 (type (match-string 3 filename))
                 (path (replace-regexp-in-string ":" "/" (match-string 4 filename))))
             (concat "/vob/sse/asic/shared/ver/tb/" block-name "/test/" path "." type)))
          (t
           filename))))

(define-compilation-mode vcs-compile-mode "vcs-compile"
  "VCS compilation mode."
  (set (make-local-variable 'compilation-error-regexp-alist)
       (list '("^Error-\\[.+?\\].+\n\\s-*\\(.+\\),[ \t\n]+\\([0-9]+\\)" 1 2)
             '("^Error-\\[.+?\\].+\n\\(.+\n\\)+\\s-*\"\\(.+\\)\",[ \t\n]+\\([0-9]+\\)" 2 3)))
  (set (make-local-variable 'compilation-parse-errors-filename-function) 'vcs-compile-find-file))

(defun vcs-compile (command &optional comint)
  "VCS compile."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command 'vcs-compile-mode))

(provide 'vcs-compile)
