;;; my-flymake.el

(require 'flymake)

(setq-default flymake-no-changes-timeout nil
              flymake-start-syntax-check-on-newline nil
              flymake-start-on-flymake-mode nil
              flymake-wrap-around nil)

(defun my-flymake-show-current-error ()
  "Show the current error point is on."
  (interactive)
  (when (and (boundp 'flymake-mode) flymake-mode)
    (let ((diags (flymake-diagnostics (point)))
          (msg ""))
      (dolist (diag diags)
        (setq msg (concat msg (flymake-diagnostic-text diag) "\n")))
      (when diags
        (message "%s" (string-trim-right msg))))))

(run-with-idle-timer 1.0 t 'my-flymake-show-current-error)

;; Don't show a count of zero in a non-default face
(defun flymake--mode-line-format ()
  "Produce a pretty minor mode indicator."
  (let* ((known (hash-table-keys flymake--backend-state))
         (running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (reported (flymake-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    `((:propertize " Flymake"
                   mouse-face mode-line-highlight
                   help-echo
                   ,(concat (format "%s known backends\n" (length known))
                            (format "%s running\n" (length running))
                            (format "%s disabled\n" (length disabled))
                            "mouse-1: Display minor mode menu\n"
                            "mouse-2: Show help for minor mode")
                   keymap
                   ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line down-mouse-1]
                        flymake-menu)
                      (define-key map [mode-line mouse-2]
                        (lambda ()
                          (interactive)
                          (describe-function 'flymake-mode)))
                      map))
      ,@(pcase-let ((`(,ind ,face ,explain)
                     (cond ((null known)
                            `("?" mode-line "No known backends"))
                           (some-waiting
                            `("Wait" compilation-mode-line-run
                              ,(format "Waiting for %s running backend(s)"
                                       (length some-waiting))))
                           (all-disabled
                            `("!" compilation-mode-line-run
                              "All backends disabled"))
                           (t
                            `(nil nil nil)))))
          (when ind
            `((":"
               (:propertize ,ind
                            face ,face
                            help-echo ,explain
                            keymap
                            ,(let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                 'flymake-switch-to-log-buffer)
                               map))))))
      ,@(unless (or all-disabled
                    (null known))
          (cl-loop
           for (type . severity)
           in (cl-sort (mapcar (lambda (type)
                                 (cons type (flymake--lookup-type-property
                                             type
                                             'severity
                                             (warning-numeric-level :error))))
                               (cl-union (hash-table-keys diags-by-type)
                                         '(:error :warning)))
                       #'>
                       :key #'cdr)
           for diags = (gethash type diags-by-type)
           for face = (flymake--lookup-type-property type
                                                     'mode-line-face
                                                     'compilation-error)
           when (or diags
                    (>= severity (warning-numeric-level :warning)))
           collect `(:propertize
                     ,(format "%d" (length diags))
                     face ,(when (> (length diags) 0) face)
                     mouse-face mode-line-highlight
                     keymap
                     ,(let ((map (make-sparse-keymap))
                            (type type))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-down-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (flymake-goto-prev-error 1 (list type) t))))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-up-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (flymake-goto-next-error 1 (list type) t))))
                        map)
                     help-echo
                     ,(concat (format "%s diagnostics of type %s\n"
                                      (propertize (format "%d"
                                                          (length diags))
                                                  'face face)
                                      (propertize (format "%s" type)
                                                  'face face))
                              (format "%s/%s: previous/next of this type"
                                      mouse-wheel-down-event
                                      mouse-wheel-up-event)))
           into forms
           finally return
           `((:propertize "[")
             ,@(cl-loop for (a . rest) on forms by #'cdr
                        collect a when rest collect
                        '(:propertize " "))
             (:propertize "]")))))))

(provide 'my-flymake)
