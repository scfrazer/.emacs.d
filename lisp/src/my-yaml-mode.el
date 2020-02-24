;;; my-yaml-mode.el  -*- lexical-binding: t; -*-

;; Lint

(defvar yaml-flymake-command "yamllint")
(defvar yaml-flymake-options '("-f" "parsable"))
(defvar-local yaml--flymake-proc nil)

(defun my-yaml-flymake (report-fn &rest _args)
  (unless (executable-find yaml-flymake-command)
    (error "Cannot find a suitable checker"))
  (when (process-live-p yaml--flymake-proc)
    (kill-process yaml--flymake-proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq yaml--flymake-proc
            (make-process
             :name "yaml-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *yaml-flymake*")
             :command (append (cons yaml-flymake-command yaml-flymake-options) (cons (buffer-file-name) nil))
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc yaml--flymake-proc))
                       (my-yaml-flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc))))))))))

(defun my-yaml-flymake-parse-output (source proc report-fn)
  "Parse output from checker."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp
            "^[^:]+:\\([0-9]+\\):[0-9]+:\\s-+\\(.+\\)$"
            nil t)
     for msg = (match-string 2)
     for (beg . end) = (flymake-diag-region
                        source
                        (string-to-number (match-string 1)))
     for type = (if (string-match "^[[]warning[]]" msg)
                    :warning
                  :error)
     collect (flymake-make-diagnostic source
                                      beg
                                      end
                                      type
                                      msg)
     into diags
     finally (funcall report-fn diags))))

;; Mode hook

(defun my-yaml-mode-hook ()
  (add-hook 'flymake-diagnostic-functions 'my-yaml-flymake nil t)
  (flymake-mode 1))
(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)

(provide 'my-yaml-mode)
