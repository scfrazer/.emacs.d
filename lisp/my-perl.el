;;; my-perl.el  -*- lexical-binding: t -*-

(require 'perl-mode)
(require 'my-flymake)

(setq-default perl-flymake-command '("flymake_perl"))

;; Fix variable expression requiring it being equal to something
(setq perl-imenu-generic-expression
      '(;; Functions
        (nil "^[ \t]*sub\\s-+\\([-[:alnum:]+_:]+\\)" 1)
        ;;Variables
        ("Variables" "^\\(?:my\\|our\\)\\s-+\\([$@%][-[:alnum:]+_:]+\\)" 1)
        ("Packages" "^[ \t]*package\\s-+\\([-[:alnum:]+_:]+\\);" 1)
        ("Doc sections" "^=head[0-9][ \t]+\\(.*\\)" 1)))

(defun my-perl-set-indent (arg)
  "Set indent."
  (interactive "nIndent level: ")
  (setq-default perl-indent-level arg
                perl-continued-statement-offset arg
                perl-continued-brace-offset (* -1 arg)))

(defun my-perl-mode-hook ()
  (flymake-mode 1))
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

(defun my-perl-flymake (orig-fun report-fn &rest _args)
  (unless (executable-find (car perl-flymake-command))
    (error "Cannot find a suitable checker"))

  (when (process-live-p perl--flymake-proc)
    (kill-process perl--flymake-proc))

  (let* ((source (current-buffer))
         (filename (buffer-file-name))
         (regexp (format "^\\(.+\\) at %s line \\([0-9]+\\)" filename)))
    (save-restriction
      (widen)
      (setq
       perl--flymake-proc
       (make-process
        :name "perl-flymake"
        :noquery t
        :connection-type 'pipe
        :buffer (generate-new-buffer " *perl-flymake*")
        :command (list (car perl-flymake-command) (buffer-file-name))
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc perl--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp regexp nil t)
                       for msg = (match-string 1)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 2)))
                       for type =
                       (if (string-match
                            "\\(Scalar value\\|Useless use\\|Unquoted string\\)"
                            msg)
                           :warning
                         :error)
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags))
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp "\\([0-9]+\\): <\\([0-9]+\\)> \\(.+\\)" nil t)
                       for msg = (match-string 3)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1)))
                       for type =
                       (if (> (string-to-number (match-string 2)) 3)
                           :error
                         :warning)
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :debug "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))))))))))

(advice-add 'perl-flymake :around #'my-perl-flymake)

(defun my-perl-tidy ()
  "Run perltidy on marked region, or entire buffer."
  (interactive "*")
  (let ((pos (point))
        beg end)
  (if (region-active-p)
      (setq beg (region-beginning)
            end (region-end))
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end (format "perltidy -q -i=%d" perl-indent-level) nil t)
  (goto-char pos)))

(provide 'my-perl)
